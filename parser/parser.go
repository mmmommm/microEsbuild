package parser

import (
	"github.com/mmmommm/microEsbuild/ast"
	"github.com/mmmommm/microEsbuild/lexer"
	"github.com/mmmommm/microEsbuild/location"
	"github.com/mmmommm/microEsbuild/util"
)

type parser struct {
	source         location.Source
	lexer          lexer.Lexer
	localTypeNames map[string]bool
	currentScope   *ast.Scope
	allocatedNames             []string
}

func (p *parser) parsePath() (location.Loc, string, *[]ast.AssertEntry) {
	pathLoc := p.lexer.Loc()
	pathText := util.UTF16ToString(p.lexer.StringLiteral())
	if p.lexer.Token == lexer.TNoSubstitutionTemplateLiteral {
		p.lexer.Next()
	} else {
		p.lexer.Expect(lexer.TStringLiteral)
	}

	// See https://github.com/tc39/proposal-import-assertions for more info
	var assertions *[]ast.AssertEntry
	if !p.lexer.HasNewlineBefore && p.lexer.IsContextualKeyword("assert") {
		// "import './foo.json' assert { type: 'json' }"
		var entries []ast.AssertEntry
		duplicates := make(map[string]location.Range)
		p.lexer.Next()
		p.lexer.Expect(lexer.TOpenBrace)

		for p.lexer.Token != lexer.TCloseBrace {
			// Parse the key
			keyLoc := p.lexer.Loc()
			preferQuotedKey := false
			var key []uint16
			var keyText string
			if p.lexer.IsIdentifierOrKeyword() {
				keyText = p.lexer.Identifier.String
				key = util.StringToUTF16(keyText)
			} else if p.lexer.Token == lexer.TStringLiteral {
				key = p.lexer.StringLiteral()
				keyText = util.UTF16ToString(key)
			} else {
				p.lexer.Expect(lexer.TIdentifier)
			}
			duplicates[keyText] = p.lexer.Range()
			p.lexer.Next()
			p.lexer.Expect(lexer.TColon)

			// Parse the value
			valueLoc := p.lexer.Loc()
			value := p.lexer.StringLiteral()
			p.lexer.Expect(lexer.TStringLiteral)

			entries = append(entries, ast.AssertEntry{
				Key:             key,
				KeyLoc:          keyLoc,
				Value:           value,
				ValueLoc:        valueLoc,
				PreferQuotedKey: preferQuotedKey,
			})

			if p.lexer.Token != lexer.TComma {
				break
			}
			p.lexer.Next()
		}

		p.lexer.Expect(lexer.TCloseBrace)
		assertions = &entries
	}

	return pathLoc, pathText, assertions
}

// The name is temporarily stored in the ref until the scope traversal pass
// happens, at which point a symbol will be generated and the ref will point
// to the symbol instead.
//
// The scope traversal pass will reconstruct the name using one of two methods.
// In the common case, the name is a slice of the file itself. In that case we
// can just store the slice and not need to allocate any extra memory. In the
// rare case, the name is an externally-allocated string. In that case we store
// an index to the string and use that index during the scope traversal pass.
func (p *parser) storeNameInRef(name lexer.MaybeSubstring) ast.Ref {
	// Is the data in "name" a subset of the data in "p.source.Contents"?
	if name.Start.IsValid() {
		// The name is a slice of the file contents, so we can just reference it by
		// length and don't have to allocate anything. This is the common case.
		//
		// It's stored as a negative value so we'll crash if we try to use it. That
		// way we'll catch cases where we've forgotten to call loadNameFromRef().
		// The length is the negative part because we know it's non-zero.
		return ast.Ref{SourceIndex: -uint32(len(name.String)), InnerIndex: uint32(name.Start.GetIndex())}
	} else {
		// The name is some memory allocated elsewhere. This is either an inline
		// string constant in the parser or an identifier with escape sequences
		// in the source code, which is very unusual. Stash it away for later.
		// This uses allocations but it should hopefully be very uncommon.
		ref := ast.Ref{SourceIndex: 0x80000000, InnerIndex: uint32(len(p.allocatedNames))}
		p.allocatedNames = append(p.allocatedNames, name.String)
		return ref
	}
}

func (p *parser) parseClauseAlias(kind string) lexer.MaybeSubstring {
	// The alias may now be a string (see https://github.com/tc39/ecma262/pull/2154)
	if p.lexer.Token == lexer.TStringLiteral {
		alias, _, _ := util.UTF16ToStringWithValidation(p.lexer.StringLiteral())
		return lexer.MaybeSubstring{String: alias}
	}

	// The alias may be a keyword
	if !p.lexer.IsIdentifierOrKeyword() {
		p.lexer.Expect(lexer.TIdentifier)
	}

	alias := p.lexer.Identifier
	return alias
}

func (p *parser) parseExportClause() ([]ast.ClauseItem, bool) {
	items := []ast.ClauseItem{}
	firstNonIdentifierLoc := location.Loc{}
	p.lexer.Expect(lexer.TOpenBrace)
	isSingleLine := !p.lexer.HasNewlineBefore

	for p.lexer.Token != lexer.TCloseBrace {
		alias := p.parseClauseAlias("export")
		aliasLoc := p.lexer.Loc()
		name := ast.LocRef{Loc: aliasLoc, Ref: p.storeNameInRef(alias)}
		originalName := alias

		// The name can actually be a keyword if we're really an "export from"
		// statement. However, we won't know until later. Allow keywords as
		// identifiers for now and throw an error later if there's no "from".
		//
		//   // This is fine
		//   export { default } from 'path'
		//
		//   // This is a syntax error
		//   export { default }
		//
		if p.lexer.Token != lexer.TIdentifier && firstNonIdentifierLoc.Start == 0 {
			firstNonIdentifierLoc = p.lexer.Loc()
		}
		p.lexer.Next()

		if alias.String == "type" && p.lexer.Token != lexer.TComma && p.lexer.Token != lexer.TCloseBrace {
			if p.lexer.IsContextualKeyword("as") {
				p.lexer.Next()
				if p.lexer.IsContextualKeyword("as") {
					alias = p.parseClauseAlias("export")
					aliasLoc = p.lexer.Loc()
					p.lexer.Next()

					if p.lexer.Token != lexer.TComma && p.lexer.Token != lexer.TCloseBrace {
						// "export { type as as as }"
						// "export { type as as foo }"
						// "export { type as as 'foo' }"
						p.parseClauseAlias("export")
						p.lexer.Next()
					} else {
						// "export { type as as }"
						items = append(items, ast.ClauseItem{
							Alias:        alias.String,
							AliasLoc:     aliasLoc,
							Name:         name,
							OriginalName: originalName.String,
						})
					}
				} else if p.lexer.Token != lexer.TComma && p.lexer.Token != lexer.TCloseBrace {
					// "export { type as xxx }"
					// "export { type as 'xxx' }"
					alias = p.parseClauseAlias("export")
					aliasLoc = p.lexer.Loc()
					p.lexer.Next()

					items = append(items, ast.ClauseItem{
						Alias:        alias.String,
						AliasLoc:     aliasLoc,
						Name:         name,
						OriginalName: originalName.String,
					})
				}
			} else {
				// The name can actually be a keyword if we're really an "export from"
				// statement. However, we won't know until later. Allow keywords as
				// identifiers for now and throw an error later if there's no "from".
				//
				//   // This is fine
				//   export { type default } from 'path'
				//
				//   // This is a syntax error
				//   export { type default }
				//
				if p.lexer.Token != lexer.TIdentifier && firstNonIdentifierLoc.Start == 0 {
					firstNonIdentifierLoc = p.lexer.Loc()
				}

				// "export { type xx }"
				// "export { type xx as yy }"
				// "export { type xx as if }"
				// "export { type default } from 'path'"
				// "export { type default as if } from 'path'"
				// "export { type xx as 'yy' }"
				// "export { type 'xx' } from 'mod'"
				p.parseClauseAlias("export")
				p.lexer.Next()

				if p.lexer.IsContextualKeyword("as") {
					p.lexer.Next()
					p.parseClauseAlias("export")
					p.lexer.Next()
				}
			}
		} else {
			if p.lexer.IsContextualKeyword("as") {
				p.lexer.Next()
				alias = p.parseClauseAlias("export")
				aliasLoc = p.lexer.Loc()
				p.lexer.Next()
			}

			items = append(items, ast.ClauseItem{
				Alias:        alias.String,
				AliasLoc:     aliasLoc,
				Name:         name,
				OriginalName: originalName.String,
			})
		}

		if p.lexer.Token != lexer.TComma {
			break
		}
		if p.lexer.HasNewlineBefore {
			isSingleLine = false
		}
		p.lexer.Next()
		if p.lexer.HasNewlineBefore {
			isSingleLine = false
		}
	}

	if p.lexer.HasNewlineBefore {
		isSingleLine = false
	}
	p.lexer.Expect(lexer.TCloseBrace)

	// Throw an error here if we found a keyword earlier and this isn't an
	// "export from" statement after all
	if firstNonIdentifierLoc.Start != 0 && !p.lexer.IsContextualKeyword("from") {
		// r := lexer.RangeOfIdentifier(p.source, firstNonIdentifierLoc)
		// p.log.Add(location.Error, &p.tracker, r, fmt.Sprintf("Expected identifier but found %q", p.source.TextForRange(r)))
		panic(lexer.LexerPanic{})
	}

	return items, isSingleLine
}

func ConvertStringToSource(contents string) location.Source {
	return location.Source{
		Index: 0,
		// KeyPath:        location.Path{Text: "<stdin>"},
		// PrettyPath:     "<stdin>",
		Contents:       contents,
		IdentifierName: "stdin",
	}
}

func CheckLexToken(contents string) lexer.Lexer {
	lexer := lexer.NewLexer(ConvertStringToSource(contents))
	return lexer
}

func newParser(source location.Source, lexer lexer.Lexer) *parser {
	p := &parser{
		source: source,
		lexer:  lexer,
	}
	return p
}

func Parse(source location.Source) (result *ast.AST) {
	p := newParser(source, lexer.NewLexer(source))

	hashbang := ""
	if p.lexer.Token == lexer.THashbang {
		hashbang = p.lexer.Identifier.String
		p.lexer.Next()
	}

}

func (p *parser) parseStmt() ast.Stmt {
	loc := p.lexer.Loc()

	switch p.lexer.Token {
	case lexer.TSemicolon:
		p.lexer.Next()
		return ast.Stmt{Loc: loc, Data: &ast.SEmpty{}}
	case lexer.TExport:
		p.lexer.Next()
		switch p.lexer.Token {
		case lexer.TClass, lexer.TConst, lexer.TFunction, lexer.TVar:
			return p.parseStmt()
		case lexer.TImport:
			p.lexer.Unexpected()
			return ast.Stmt{}
		case lexer.TEnum:
			return p.parseStmt()
		case lexer.TIdentifier:
			if p.lexer.IsContextualKeyword("let") {
				return p.parseStmt()
			}
			if p.lexer.IsContextualKeyword("as") {
				p.lexer.Next()
				p.lexer.ExpectContextualKeyword("namespace")
				p.lexer.Expect(lexer.TIdentifier)
				p.lexer.ExpectOrInsertSemicolon()
				return ast.Stmt{Loc: loc, Data: &ast.STypeScript{}}
			}
			if p.lexer.IsContextualKeyword("async") {
				asyncRange := p.lexer.Range()
				p.lexer.Next()
				if p.lexer.HasNewlineBefore {
					panic(lexer.LexerPanic{})
				}
				p.lexer.Expect(lexer.TFunction)
				return p.parseFnStmt(loc, true, asyncRange)
			}
			switch p.lexer.Identifier.String {
			case "type":
				typeRange := p.lexer.Range()
				p.lexer.Next()
				if p.lexer.HasNewlineBefore {
					panic(lexer.LexerPanic{})
				}
				p.skipTypeScriptTypeStmt()
				return ast.Stmt{Loc: loc, Data: &ast.STypeScript{}}
			case "namespace", "abstract", "module", "interface":
				return p.parseStmt()
			case "declare":
				return p.parseStmt()
			}
		}
		p.lexer.Unexpected()
		return ast.Stmt{}
	case lexer.TDefault:
		defaultLoc := p.lexer.Loc()
		p.lexer.Next()
		createDefaultName := func() ast.LocRef {
			defaultName := ast.LocRef{Loc: defaultLoc, Ref: p.newSymbol(ast.SymbolOther, "default")}
			p.currentScope.Generated = append(p.currentScope.Generated, defaultName.Ref)
			return defaultName
		}
		if p.lexer.IsContextualKeyword("async") {
			asyncRange := p.lexer.Range()
			p.lexer.Next()
			if p.lexer.Token == lexer.TFunction && !p.lexer.HasNewlineBefore {
				p.lexer.Next()
				stmt := p.parseStmt()
				if _, ok := stmt.Data.(*ast.STypeScript); ok {
					return stmt
				}
				var defaultName ast.LocRef
				if s, ok := stmt.Data.(*ast.STypeScript); ok && s.Fn.Name != nil {
					defaultName = ast.LocRef{Loc: defaultLoc, Ref: s.Fn.Name.Ref}
				} else {
					defaultName = createDefaultName()
				}
				return ast.Stmt{Loc: loc, Data: &ast.SExportDefault{DefaultName: defaultName, Value: stmt}}
			}
			defaultName := createDefaultName()
			p.lexer.ExpectOrInsertSemicolon()
			return ast.Stmt{Loc: loc, Data: &ast.SExportDefault{
				DefaultName: defaultName, Value: ast.Stmt{Loc: loc, Data: &ast.SExpr{Value: expr}},
			}}
		}
		if p.lexer.Token == lexer.TFunction || p.lexer.Token == lexer.TClass || p.lexer.IsContextualKeyword("interface") {
				stmt := p.parseStmt()
				if _, ok := stmt.Data.(*ast.STypeScript); ok {
					return stmt // This was just a type annotation
				}

				// Use the statement name if present, since it's a better name
				var defaultName ast.LocRef
				switch s := stmt.Data.(type) {
				case *ast.SFunction:
					if s.Fn.Name != nil {
						defaultName = ast.LocRef{Loc: defaultLoc, Ref: s.Fn.Name.Ref}
					} else {
						defaultName = createDefaultName()
					}
				case *ast.SClass:
					if s.Class.Name != nil {
						defaultName = ast.LocRef{Loc: defaultLoc, Ref: s.Class.Name.Ref}
					} else {
						defaultName = createDefaultName()
					}
				default:
					panic("Internal error")
				}

				return ast.Stmt{Loc: loc, Data: &ast.SExportDefault{DefaultName: defaultName, Value: stmt}}
			}

			isIdentifier := p.lexer.Token == lexer.TIdentifier
			name := p.lexer.Identifier.String
			expr := p.parseExpr(ast.LComma)

			// Handle the default export of an abstract class in TypeScript
			if isIdentifier && name == "abstract" {
				if _, ok := expr.Data.(*ast.EIdentifier); ok && (p.lexer.Token == lexer.TClass) {

					// Use the statement name if present, since it's a better name
					var defaultName ast.LocRef
					if s, ok := stmt.Data.(*ast.SClass); ok && s.Class.Name != nil {
						defaultName = ast.LocRef{Loc: defaultLoc, Ref: s.Class.Name.Ref}
					} else {
						defaultName = createDefaultName()
					}

					return ast.Stmt{Loc: loc, Data: &ast.SExportDefault{DefaultName: defaultName, Value: stmt}}
				}
			}

			p.lexer.ExpectOrInsertSemicolon()
			defaultName := createDefaultName()
			return ast.Stmt{Loc: loc, Data: &ast.SExportDefault{
				DefaultName: defaultName, Value: ast.Stmt{Loc: loc, Data: &ast.SExpr{Value: expr}}}}

		case lexer.TAsterisk:
			p.lexer.Next()
			var namespaceRef ast.Ref
			var alias *ast.ExportStarAlias
			var pathLoc location.Loc
			var pathText string
			var assertions *[]ast.AssertEntry

			if p.lexer.IsContextualKeyword("as") {
				// "export * as ns from 'path'"
				p.lexer.Next()
				name := p.parseClauseAlias("export")
				namespaceRef = p.storeNameInRef(name)
				alias = &ast.ExportStarAlias{Loc: p.lexer.Loc(), OriginalName: name.String}
				p.lexer.Next()
				p.lexer.ExpectContextualKeyword("from")
				pathLoc, pathText, assertions = p.parsePath()
			} else {
				// "export * from 'path'"
				p.lexer.ExpectContextualKeyword("from")
				pathLoc, pathText, assertions = p.parsePath()
				name := ast.GenerateNonUniqueNameFromPath(pathText) + "_star"
				namespaceRef = p.storeNameInRef(lexer.MaybeSubstring{String: name})
			}
			importRecordIndex := p.addImportRecord(ast.ImportStmt, pathLoc, pathText, assertions)

			// Export-star statements anywhere in the file disable top-level const
			// local prefix because import cycles can be used to trigger TDZ
			p.currentScope.IsAfterConstLocalPrefix = true

			p.lexer.ExpectOrInsertSemicolon()
			return ast.Stmt{Loc: loc, Data: &ast.SExportStar{
				NamespaceRef:      namespaceRef,
				Alias:             alias,
				ImportRecordIndex: importRecordIndex,
			}}

		case lexer.TOpenBrace:
			items, isSingleLine := p.parseExportClause()
			if p.lexer.IsContextualKeyword("from") {
				// "export {} from 'path'"
				p.lexer.Next()
				pathLoc, pathText, assertions := p.parsePath()
				importRecordIndex := p.addImportRecord(ast.ImportStmt, pathLoc, pathText, assertions)
				name := "import_" + ast.GenerateNonUniqueNameFromPath(pathText)
				namespaceRef := p.storeNameInRef(lexer.MaybeSubstring{String: name})

				// Export clause statements anywhere in the file disable top-level const
				// local prefix because import cycles can be used to trigger TDZ
				p.currentScope.IsAfterConstLocalPrefix = true

				p.lexer.ExpectOrInsertSemicolon()
				return ast.Stmt{Loc: loc, Data: &ast.SExportFrom{
					Items:             items,
					NamespaceRef:      namespaceRef,
					ImportRecordIndex: importRecordIndex,
					IsSingleLine:      isSingleLine,
				}}
			}

			p.lexer.ExpectOrInsertSemicolon()
			return ast.Stmt{Loc: loc, Data: &ast.SExportClause{Items: items, IsSingleLine: isSingleLine}}

		case lexer.TEquals:
			// "export = value;"
			// p.esmExportKeyword = previousExportKeyword // This wasn't an ESM export statement after all
			p.lexer.Next()
			value := p.parseExpr(ast.LLowest)
			p.lexer.ExpectOrInsertSemicolon()
			return ast.Stmt{Loc: loc, Data: &ast.SExportEquals{Value: value}}
		p.lexer.Unexpected()
			return ast.Stmt{}

		default:
			p.lexer.Unexpected()
			return ast.Stmt{}

	case lexer.TFunction:
		p.lexer.Next()
		return p.parseFnStmt(loc, false /* isAsync */, location.Range{})

	case lexer.TEnum:
		return p.parseTsEnumStmt(loc)

	case lexer.TAt:
		// Parse decorators before class statements, which are potentially exported
			scopeIndex := len(p.scopesInOrder)
			tsDecorators := p.parseTypeScriptDecorators(p.currentScope)

			// If this turns out to be a "declare class" statement, we need to undo the
			// scopes that were potentially pushed while parsing the decorator arguments.
			// That can look like any one of the following:
			//
			//   "@decorator declare class Foo {}"
			//   "@decorator declare abstract class Foo {}"
			//   "@decorator export declare class Foo {}"
			//   "@decorator export declare abstract class Foo {}"
			//

			// "@decorator class Foo {}"
			// "@decorator abstract class Foo {}"
			// "@decorator declare class Foo {}"
			// "@decorator declare abstract class Foo {}"
			// "@decorator export class Foo {}"
			// "@decorator export abstract class Foo {}"
			// "@decorator export declare class Foo {}"
			// "@decorator export declare abstract class Foo {}"
			// "@decorator export default class Foo {}"
			// "@decorator export default abstract class Foo {}"
			if p.lexer.Token != lexer.TClass && p.lexer.Token != lexer.TExport &&
				!p.lexer.IsContextualKeyword("abstract") && !p.lexer.IsContextualKeyword("declare") {
			}

			return p.parseStmt()
		return ast.Stmt{}

	case lexer.TClass:
		return p.parseClassStmt(loc)

	case lexer.TVar:
		p.lexer.Next()
		decls := p.parseAndDeclareDecls(ast.SymbolHoisted)
		p.lexer.ExpectOrInsertSemicolon()
		return ast.Stmt{Loc: loc, Data: &ast.SLocal{
			Kind:     ast.LocalVar,
			Decls:    decls,
		}}

	case lexer.TConst:
		p.forbidLexicalDecl(loc)
		p.markSyntaxFeature(compat.Const, p.lexer.Range())
		p.lexer.Next()

		if p.options.ts.Parse && p.lexer.Token == lexer.TEnum {
			return p.parseTypeScriptEnumStmt(loc)
		}

		decls := p.parseAndDeclareDecls(ast.SymbolConst)
		p.lexer.ExpectOrInsertSemicolon()
		p.requireInitializers(decls)
		return ast.Stmt{Loc: loc, Data: &ast.SLocal{
			Kind:     ast.LocalConst,
			Decls:    decls,
		}}

	case lexer.TIf:
		p.lexer.Next()
		p.lexer.Expect(lexer.TOpenParen)
		test := p.parseExpr(ast.LLowest)
		p.lexer.Expect(lexer.TCloseParen)
		yes := p.parseStmt()
		var noOrNil ast.Stmt
		if p.lexer.Token == lexer.TElse {
			p.lexer.Next()
			noOrNil = p.parseStmt()
		}
		return ast.Stmt{Loc: loc, Data: &ast.SIf{Test: test, Yes: yes, NoOrNil: noOrNil}}

	case lexer.TDo:
		p.lexer.Next()
		body := p.parseStmt()
		p.lexer.Expect(lexer.TWhile)
		p.lexer.Expect(lexer.TOpenParen)
		test := p.parseExpr(ast.LLowest)
		p.lexer.Expect(lexer.TCloseParen)

		// This is a weird corner case where automatic semicolon insertion applies
		// even without a newline present
		if p.lexer.Token == lexer.TSemicolon {
			p.lexer.Next()
		}
		return ast.Stmt{Loc: loc, Data: &ast.SDoWhile{Body: body, Test: test}}

	case lexer.TWhile:
		p.lexer.Next()
		p.lexer.Expect(lexer.TOpenParen)
		test := p.parseExpr(ast.LLowest)
		p.lexer.Expect(lexer.TCloseParen)
		body := p.parseStmt()
		return ast.Stmt{Loc: loc, Data: &ast.SWhile{Test: test, Body: body}}

	case lexer.TWith:
		p.lexer.Next()
		p.lexer.Expect(lexer.TOpenParen)
		test := p.parseExpr(ast.LLowest)
		bodyLoc := p.lexer.Loc()
		p.lexer.Expect(lexer.TCloseParen)

		// Push a scope so we make sure to prevent any bare identifiers referenced
		// within the body from being renamed. Renaming them might change the
		// semantics of the code.
		p.pushScopeForParsePass(ast.ScopeWith, bodyLoc)
		body := p.parseStmt()
		p.popScope()

		return ast.Stmt{Loc: loc, Data: &ast.SWith{Value: test, BodyLoc: bodyLoc, Body: body}}

	case lexer.TSwitch:
		p.lexer.Next()
		p.lexer.Expect(lexer.TOpenParen)
		test := p.parseExpr(ast.LLowest)
		p.lexer.Expect(lexer.TCloseParen)

		bodyLoc := p.lexer.Loc()
		p.pushScopeForParsePass(ast.ScopeBlock, bodyLoc)
		defer p.popScope()

		p.lexer.Expect(lexer.TOpenBrace)
		cases := []ast.Case{}
		foundDefault := false

		for p.lexer.Token != lexer.TCloseBrace {
			var value ast.Expr
			body := []ast.Stmt{}

			if p.lexer.Token == lexer.TDefault {
				if foundDefault {
					panic(lexer.LexerPanic{})
				}
				foundDefault = true
				p.lexer.Next()
				p.lexer.Expect(lexer.TColon)
			} else {
				p.lexer.Expect(lexer.TCase)
				value = p.parseExpr(ast.LLowest)
				p.lexer.Expect(lexer.TColon)
			}

		caseBody:
			for {
				switch p.lexer.Token {
				case lexer.TCloseBrace, lexer.TCase, lexer.TDefault:
					break caseBody

				default:
					body = append(body, p.parseStmt())
				}
			}

			cases = append(cases, ast.Case{ValueOrNil: value, Body: body})
		}

		p.lexer.Expect(lexer.TCloseBrace)
		return ast.Stmt{Loc: loc, Data: &ast.SSwitch{
			Test:    test,
			BodyLoc: bodyLoc,
			Cases:   cases,
		}}

	case lexer.TTry:
		p.lexer.Next()
		blockLoc := p.lexer.Loc()
		p.lexer.Expect(lexer.TOpenBrace)
		p.pushScopeForParsePass(ast.ScopeBlock, loc)
		body := p.parseStmtsUpTo(lexer.TCloseBrace)
		p.popScope()
		closeBraceLoc := p.lexer.Loc()
		p.lexer.Next()

		var catch *ast.Catch = nil
		var finally *ast.Finally = nil

		if p.lexer.Token == lexer.TCatch {
			catchLoc := p.lexer.Loc()
			p.pushScopeForParsePass(ast.ScopeCatchBinding, catchLoc)
			p.lexer.Next()
			var bindingOrNil ast.Binding

			// The catch binding is optional, and can be omitted
			if p.lexer.Token == lexer.TOpenBrace {
					// Generate a new symbol for the catch binding for older browsers
					ref := p.newSymbol(ast.SymbolOther, "e")
					p.currentScope.Generated = append(p.currentScope.Generated, ref)
					bindingOrNil = ast.Binding{Loc: p.lexer.Loc(), Data: &ast.BIdentifier{Ref: ref}}
			} else {
				p.lexer.Expect(lexer.TOpenParen)
				bindingOrNil = p.parseBinding()

				// Skip over types
				if p.lexer.Token == lexer.TColon {
					p.lexer.Expect(lexer.TColon)
					p.skipTsType(ast.LLowest)
				}

				p.lexer.Expect(lexer.TCloseParen)

				// Bare identifiers are a special case
				kind := ast.SymbolOther
				if _, ok := bindingOrNil.Data.(*ast.BIdentifier); ok {
					kind = ast.SymbolCatchIdentifier
				}
				p.declareBinding(kind, bindingOrNil)
			}

			blockLoc := p.lexer.Loc()
			p.lexer.Expect(lexer.TOpenBrace)

			p.pushScopeForParsePass(ast.ScopeBlock, blockLoc)
			stmts := p.parseStmtsUpTo(lexer.TCloseBrace)
			p.popScope()

			closeBraceLoc := p.lexer.Loc()
			p.lexer.Next()
			catch = &ast.Catch{Loc: catchLoc, BindingOrNil: bindingOrNil, BlockLoc: blockLoc, Block: ast.SBlock{Stmts: stmts, CloseBraceLoc: closeBraceLoc}}
			p.popScope()
		}

		if p.lexer.Token == lexer.TFinally || catch == nil {
			finallyLoc := p.lexer.Loc()
			p.pushScopeForParsePass(ast.ScopeBlock, finallyLoc)
			p.lexer.Expect(lexer.TFinally)
			p.lexer.Expect(lexer.TOpenBrace)
			stmts := p.parseStmtsUpTo(lexer.TCloseBrace)
			closeBraceLoc := p.lexer.Loc()
			p.lexer.Next()
			finally = &ast.Finally{Loc: finallyLoc, Block: ast.SBlock{Stmts: stmts, CloseBraceLoc: closeBraceLoc}}
			p.popScope()
		}

		return ast.Stmt{Loc: loc, Data: &ast.STry{
			BlockLoc: blockLoc,
			Block:    ast.SBlock{Stmts: body, CloseBraceLoc: closeBraceLoc},
			Catch:    catch,
			Finally:  finally,
		}}

	case lexer.TFor:
		p.pushScopeForParsePass(ast.ScopeBlock, loc)
		defer p.popScope()

		p.lexer.Next()

		// "for await (let x of y) {}"
		isForAwait := p.lexer.IsContextualKeyword("await")
		if isForAwait {
			awaitRange := p.lexer.Range()
			if p.fnOrArrowDataParse.await != allowExpr {
				isForAwait = false
			} else {
				didGenerateError := p.markSyntaxFeature(compat.ForAwait, awaitRange)
				if p.fnOrArrowDataParse.isTopLevel && !didGenerateError {
					p.topLevelAwaitKeyword = awaitRange
					p.markSyntaxFeature(compat.TopLevelAwait, awaitRange)
				}
			}
			p.lexer.Next()
		}

		p.lexer.Expect(lexer.TOpenParen)

		var initOrNil ast.Stmt
		var testOrNil ast.Expr
		var updateOrNil ast.Expr

		// "in" expressions aren't allowed here
		p.allowIn = false

		var badLetRange location.Range
		if p.lexer.IsContextualKeyword("let") {
			badLetRange = p.lexer.Range()
		}
		decls := []ast.Decl{}
		initLoc := p.lexer.Loc()
		isVar := false
		switch p.lexer.Token {
		case lexer.TVar:
			isVar = true
			p.lexer.Next()
			decls = p.parseAndDeclareDecls(ast.SymbolHoisted, parseStmtOpts{})
			initOrNil = ast.Stmt{Loc: initLoc, Data: &ast.SLocal{Kind: ast.LocalVar, Decls: decls}}

		case lexer.TConst:
			p.markSyntaxFeature(compat.Const, p.lexer.Range())
			p.lexer.Next()
			decls = p.parseAndDeclareDecls(ast.SymbolConst, parseStmtOpts{})
			initOrNil = ast.Stmt{Loc: initLoc, Data: &ast.SLocal{Kind: ast.LocalConst, Decls: decls}}

		case lexer.TSemicolon:

		default:
			var expr ast.Expr
			var stmt ast.Stmt
			expr, stmt, decls = p.parseExprOrLetStmt(parseStmtOpts{
				lexicalDecl:        lexicalDeclAllowAll,
				isForLoopInit:      true,
				isForAwaitLoopInit: isForAwait,
			})
			if stmt.Data != nil {
				badLetRange = location.Range{}
				initOrNil = stmt
			} else {
				initOrNil = ast.Stmt{Loc: initLoc, Data: &ast.SExpr{Value: expr}}
			}
		}

		// "in" expressions are allowed again
		p.allowIn = true

		// Detect for-of loops
		if p.lexer.IsContextualKeyword("of") || isForAwait {
			if isForAwait && !p.lexer.IsContextualKeyword("of") {
				if initOrNil.Data != nil {
					p.lexer.ExpectedString("\"of\"")
				} else {
					p.lexer.Unexpected()
				}
			}
			p.forbidInitializers(decls, "of", false)
			p.markSyntaxFeature(compat.ForOf, p.lexer.Range())
			p.lexer.Next()
			value := p.parseExpr(ast.LComma)
			p.lexer.Expect(lexer.TCloseParen)
			body := p.parseStmt()
			return ast.Stmt{Loc: loc, Data: &ast.SForOf{IsAwait: isForAwait, Init: initOrNil, Value: value, Body: body}}
		}

		// Detect for-in loops
		if p.lexer.Token == lexer.TIn {
			p.forbidInitializers(decls, "in", isVar)
			p.lexer.Next()
			value := p.parseExpr(ast.LLowest)
			p.lexer.Expect(lexer.TCloseParen)
			body := p.parseStmt()
			return ast.Stmt{Loc: loc, Data: &ast.SForIn{Init: initOrNil, Value: value, Body: body}}
		}

		// Only require "const" statement initializers when we know we're a normal for loop
		if local, ok := initOrNil.Data.(*ast.SLocal); ok && local.Kind == ast.LocalConst {
			p.requireInitializers(decls)
		}

		p.lexer.Expect(lexer.TSemicolon)

		if p.lexer.Token != lexer.TSemicolon {
			testOrNil = p.parseExpr(ast.LLowest)
		}

		p.lexer.Expect(lexer.TSemicolon)

		if p.lexer.Token != lexer.TCloseParen {
			updateOrNil = p.parseExpr(ast.LLowest)
		}

		p.lexer.Expect(lexer.TCloseParen)
		body := p.parseStmt(parseStmtOpts{})
		return ast.Stmt{Loc: loc, Data: &ast.SFor{
			InitOrNil:   initOrNil,
			TestOrNil:   testOrNil,
			UpdateOrNil: updateOrNil,
			Body:        body,
		}}

	case lexer.TImport:
		previousImportStatementKeyword := p.esmImportStatementKeyword
		p.esmImportStatementKeyword = p.lexer.Range()
		p.lexer.Next()
		stmt := ast.SImport{}
		wasOriginallyBareImport := false

		// "export import foo = bar"
		// "import foo = bar" in a namespace
		if (p.lexer.Token != lexer.TIdentifier) {
			p.lexer.Expected(lexer.TIdentifier)
		}

		switch p.lexer.Token {
		case lexer.TOpenParen, lexer.TDot:
			// "import('path')"
			// "import.meta"
			p.esmImportStatementKeyword = previousImportStatementKeyword // This wasn't an ESM import statement after all
			expr := p.parseSuffix(p.parseImportExpr(loc, ast.LLowest), ast.LLowest, nil, 0)
			p.lexer.ExpectOrInsertSemicolon()
			return ast.Stmt{Loc: loc, Data: &ast.SExpr{Value: expr}}

		case lexer.TStringLiteral, lexer.TNoSubstitutionTemplateLiteral:
			// "import 'path'"

			wasOriginallyBareImport = true

		case lexer.TAsterisk:
			// "import * as ns from 'path'"

			p.lexer.Next()
			p.lexer.ExpectContextualKeyword("as")
			stmt.NamespaceRef = p.storeNameInRef(p.lexer.Identifier)
			starLoc := p.lexer.Loc()
			stmt.StarNameLoc = &starLoc
			p.lexer.Expect(lexer.TIdentifier)
			p.lexer.ExpectContextualKeyword("from")

		case lexer.TOpenBrace:
			// "import {item1, item2} from 'path'"

			items, isSingleLine := p.parseImportClause()
			stmt.Items = &items
			stmt.IsSingleLine = isSingleLine
			p.lexer.ExpectContextualKeyword("from")

		case lexer.TIdentifier:
			// "import defaultItem from 'path'"
			// "import foo = bar"

			defaultName := p.lexer.Identifier
			stmt.DefaultName = &ast.LocRef{Loc: p.lexer.Loc(), Ref: p.storeNameInRef(defaultName)}
			p.lexer.Next()

				// Skip over type-only imports
				if defaultName.String == "type" {
					switch p.lexer.Token {
					case lexer.TIdentifier:
						if p.lexer.Identifier.String != "from" {
							defaultName = p.lexer.Identifier
							stmt.DefaultName.Loc = p.lexer.Loc()
							p.lexer.Next()
							if p.lexer.Token == lexer.TEquals {
								// "import type foo = require('bar');"
								// "import type foo = bar.baz;"
								return p.parseTypeScriptImportEqualsStmt(loc, opts, stmt.DefaultName.Loc, defaultName.String)
							} else {
								// "import type foo from 'bar';"
								p.lexer.ExpectContextualKeyword("from")
								p.parsePath()
								p.lexer.ExpectOrInsertSemicolon()
								return ast.Stmt{Loc: loc, Data: &ast.STypeScript{}}
							}
						}

					case lexer.TAsterisk:
						// "import type * as foo from 'bar';"
						p.lexer.Next()
						p.lexer.ExpectContextualKeyword("as")
						p.lexer.Expect(lexer.TIdentifier)
						p.lexer.ExpectContextualKeyword("from")
						p.parsePath()
						p.lexer.ExpectOrInsertSemicolon()
						return ast.Stmt{Loc: loc, Data: &ast.STypeScript{}}

					case lexer.TOpenBrace:
						// "import type {foo} from 'bar';"
						p.parseImportClause()
						p.lexer.ExpectContextualKeyword("from")
						p.parsePath()
						p.lexer.ExpectOrInsertSemicolon()
						return ast.Stmt{Loc: loc, Data: &ast.STypeScript{}}
					}
				}

				// Parse TypeScript import assignment statements
				if (p.lexer.Token == lexer.TEquals) {
					p.esmImportStatementKeyword = previousImportStatementKeyword // This wasn't an ESM import statement after all
					return p.parseTypeScriptImportEqualsStmt(loc, opts, stmt.DefaultName.Loc, defaultName.String)
				}

			if p.lexer.Token == lexer.TComma {
				p.lexer.Next()
				switch p.lexer.Token {
				case lexer.TAsterisk:
					// "import defaultItem, * as ns from 'path'"
					p.lexer.Next()
					p.lexer.ExpectContextualKeyword("as")
					stmt.NamespaceRef = p.storeNameInRef(p.lexer.Identifier)
					starLoc := p.lexer.Loc()
					stmt.StarNameLoc = &starLoc
					p.lexer.Expect(lexer.TIdentifier)

				case lexer.TOpenBrace:
					// "import defaultItem, {item1, item2} from 'path'"
					items, isSingleLine := p.parseImportClause()
					stmt.Items = &items
					stmt.IsSingleLine = isSingleLine

				default:
					p.lexer.Unexpected()
				}
			}

			p.lexer.ExpectContextualKeyword("from")

		default:
			p.lexer.Unexpected()
			return ast.Stmt{}
		}

		pathLoc, pathText, assertions := p.parsePath()
		stmt.ImportRecordIndex = p.addImportRecord(ast.ImportStmt, pathLoc, pathText, assertions)
		if wasOriginallyBareImport {
			p.importRecords[stmt.ImportRecordIndex].Flags |= ast.WasOriginallyBareImport
		}
		p.lexer.ExpectOrInsertSemicolon()

		if stmt.StarNameLoc != nil {
			name := p.loadNameFromRef(stmt.NamespaceRef)
			stmt.NamespaceRef = p.declareSymbol(ast.SymbolImport, *stmt.StarNameLoc, name)
		} else {
			// Generate a symbol for the namespace
			name := "import_" + ast.GenerateNonUniqueNameFromPath(pathText)
			stmt.NamespaceRef = p.newSymbol(ast.SymbolOther, name)
			p.currentScope.Generated = append(p.currentScope.Generated, stmt.NamespaceRef)
		}
		itemRefs := make(map[string]ast.LocRef)

		// Link the default item to the namespace
		if stmt.DefaultName != nil {
			name := p.loadNameFromRef(stmt.DefaultName.Ref)
			ref := p.declareSymbol(ast.SymbolImport, stmt.DefaultName.Loc, name)
			p.isImportItem[ref] = true
			stmt.DefaultName.Ref = ref
		}

		// Link each import item to the namespace
		if stmt.Items != nil {
			for i, item := range *stmt.Items {
				name := p.loadNameFromRef(item.Name.Ref)
				ref := p.declareSymbol(ast.SymbolImport, item.Name.Loc, name)
				p.checkForUnrepresentableIdentifier(item.AliasLoc, item.Alias)
				p.isImportItem[ref] = true
				(*stmt.Items)[i].Name.Ref = ref
				itemRefs[item.Alias] = ast.LocRef{Loc: item.Name.Loc, Ref: ref}
			}
		}

		// Track the items for this namespace
		p.importItemsForNamespace[stmt.NamespaceRef] = itemRefs

		// Import statements anywhere in the file disable top-level const
		// local prefix because import cycles can be used to trigger TDZ
		p.currentScope.IsAfterConstLocalPrefix = true
		return ast.Stmt{Loc: loc, Data: &stmt}

	case lexer.TBreak:
		p.lexer.Next()
		name := p.parseLabelName()
		p.lexer.ExpectOrInsertSemicolon()
		return ast.Stmt{Loc: loc, Data: &ast.SBreak{Label: name}}

	case lexer.TContinue:
		p.lexer.Next()
		name := p.parseLabelName()
		p.lexer.ExpectOrInsertSemicolon()
		return ast.Stmt{Loc: loc, Data: &ast.SContinue{Label: name}}

	case lexer.TReturn:
		if p.fnOrArrowDataParse.isReturnDisallowed {
			p.log.Add(logger.Error, &p.tracker, p.lexer.Range(), "A return statement cannot be used here:")
		}
		p.lexer.Next()
		var value ast.Expr
		if p.lexer.Token != lexer.TSemicolon &&
			!p.lexer.HasNewlineBefore &&
			p.lexer.Token != lexer.TCloseBrace &&
			p.lexer.Token != lexer.TEndOfFile {
			value = p.parseExpr(ast.LLowest)
		}
		p.latestReturnHadSemicolon = p.lexer.Token == lexer.TSemicolon
		p.lexer.ExpectOrInsertSemicolon()
		return ast.Stmt{Loc: loc, Data: &ast.SReturn{ValueOrNil: value}}

	case lexer.TThrow:
		p.lexer.Next()
		if p.lexer.HasNewlineBefore {
			p.log.Add(logger.Error, &p.tracker, logger.Range{Loc: logger.Loc{Start: loc.Start + 5}},
				"Unexpected newline after \"throw\"")
			panic(lexer.LexerPanic{})
		}
		expr := p.parseExpr(ast.LLowest)
		p.lexer.ExpectOrInsertSemicolon()
		return ast.Stmt{Loc: loc, Data: &ast.SThrow{Value: expr}}

	case lexer.TDebugger:
		p.lexer.Next()
		p.lexer.ExpectOrInsertSemicolon()
		return ast.Stmt{Loc: loc, Data: &ast.SDebugger{}}

	case lexer.TOpenBrace:
		p.pushScopeForParsePass(ast.ScopeBlock, loc)
		defer p.popScope()

		p.lexer.Next()
		stmts := p.parseStmtsUpTo(lexer.TCloseBrace, parseStmtOpts{})
		closeBraceLoc := p.lexer.Loc()
		p.lexer.Next()
		return ast.Stmt{Loc: loc, Data: &ast.SBlock{Stmts: stmts, CloseBraceLoc: closeBraceLoc}}

	default:
		isIdentifier := p.lexer.Token == lexer.TIdentifier
		name := p.lexer.Identifier.String

		// Parse either an async function, an async expression, or a normal expression
		var expr ast.Expr
		if isIdentifier && p.lexer.Raw() == "async" {
			asyncRange := p.lexer.Range()
			p.lexer.Next()
			if p.lexer.Token == lexer.TFunction && !p.lexer.HasNewlineBefore {
				p.lexer.Next()
				return p.parseFnStmt(asyncRange.Loc, opts, true /* isAsync */, asyncRange)
			}
			expr = p.parseSuffix(p.parseAsyncPrefixExpr(asyncRange, ast.LLowest, 0), ast.LLowest, nil, 0)
		} else {
			var stmt ast.Stmt
			expr, stmt, _ = p.parseExprOrLetStmt(opts)
			if stmt.Data != nil {
				p.lexer.ExpectOrInsertSemicolon()
				return stmt
			}
		}

		if isIdentifier {
			if ident, ok := expr.Data.(*ast.EIdentifier); ok {
				if p.lexer.Token == lexer.TColon && opts.tsDecorators == nil {
					p.pushScopeForParsePass(ast.ScopeLabel, loc)
					defer p.popScope()

					// Parse a labeled statement
					p.lexer.Next()
					name := ast.LocRef{Loc: expr.Loc, Ref: ident.Ref}
					nestedOpts := parseStmtOpts{}
					if opts.lexicalDecl == lexicalDeclAllowAll || opts.lexicalDecl == lexicalDeclAllowFnInsideLabel {
						nestedOpts.lexicalDecl = lexicalDeclAllowFnInsideLabel
					}
					stmt := p.parseStmt(nestedOpts)
					return ast.Stmt{Loc: loc, Data: &ast.SLabel{Name: name, Stmt: stmt}}
				}

				if p.options.ts.Parse {
					switch name {
					case "type":
						if p.lexer.Token == lexer.TIdentifier && !p.lexer.HasNewlineBefore {
							// "type Foo = any"
							p.skipTypeScriptTypeStmt(parseStmtOpts{isModuleScope: opts.isModuleScope})
							return ast.Stmt{Loc: loc, Data: &ast.STypeScript{}}
						}

					case "namespace", "module":
						// "namespace Foo {}"
						// "module Foo {}"
						// "declare module 'fs' {}"
						// "declare module 'fs';"
						if (opts.isModuleScope || opts.isNamespaceScope) && (p.lexer.Token == lexer.TIdentifier ||
							(p.lexer.Token == lexer.TStringLiteral && opts.isTypeScriptDeclare)) {
							return p.parseTypeScriptNamespaceStmt(loc, opts)
						}

					case "interface":
						// "interface Foo {}"
						p.skipTypeScriptInterfaceStmt(parseStmtOpts{isModuleScope: opts.isModuleScope})
						return ast.Stmt{Loc: loc, Data: &ast.STypeScript{}}

					case "abstract":
						if p.lexer.Token == lexer.TClass || opts.tsDecorators != nil {
							return p.parseClassStmt(loc, opts)
						}

					case "global":
						// "declare module 'fs' { global { namespace NodeJS {} } }"
						if opts.isNamespaceScope && opts.isTypeScriptDeclare && p.lexer.Token == lexer.TOpenBrace {
							p.lexer.Next()
							p.parseStmtsUpTo(lexer.TCloseBrace, opts)
							p.lexer.Next()
							return ast.Stmt{Loc: loc, Data: &ast.STypeScript{}}
						}

					case "declare":
						opts.lexicalDecl = lexicalDeclAllowAll
						opts.isTypeScriptDeclare = true

						// "@decorator declare class Foo {}"
						// "@decorator declare abstract class Foo {}"
						if opts.tsDecorators != nil && p.lexer.Token != lexer.TClass && !p.lexer.IsContextualKeyword("abstract") {
							p.logMisplacedDecoratorError(opts.tsDecorators)
						}

						// "declare global { ... }"
						if p.lexer.IsContextualKeyword("global") {
							p.lexer.Next()
							p.lexer.Expect(lexer.TOpenBrace)
							p.parseStmtsUpTo(lexer.TCloseBrace, opts)
							p.lexer.Next()
							return ast.Stmt{Loc: loc, Data: &ast.STypeScript{}}
						}

						// "declare const x: any"
						stmt := p.parseStmt(opts)
						if opts.tsDecorators != nil {
							p.discardScopesUpTo(opts.tsDecorators.scopeIndex)
						}

						// Unlike almost all uses of "declare", statements that use
						// "export declare" with "var/let/const" inside a namespace affect
						// code generation. They cause any declared bindings to be
						// considered exports of the namespace. Identifier references to
						// those names must be converted into property accesses off the
						// namespace object:
						//
						//   namespace ns {
						//     export declare const x
						//     export function y() { return x }
						//   }
						//
						//   (ns as any).x = 1
						//   console.log(ns.y())
						//
						// In this example, "return x" must be replaced with "return ns.x".
						// This is handled by replacing each "export declare" statement
						// inside a namespace with an "export var" statement containing all
						// of the declared bindings. That "export var" statement will later
						// cause identifiers to be transformed into property accesses.
						if opts.isNamespaceScope && opts.isExport {
							var decls []ast.Decl
							if s, ok := stmt.Data.(*ast.SLocal); ok {
								for _, decl := range s.Decls {
									decls = extractDeclsForBinding(decl.Binding, decls)
								}
							}
							if len(decls) > 0 {
								return ast.Stmt{Loc: loc, Data: &ast.SLocal{
									Kind:     ast.LocalVar,
									IsExport: true,
									Decls:    decls,
								}}
							}
						}

						return ast.Stmt{Loc: loc, Data: &ast.STypeScript{}}
					}
				}
			}
		}

		p.lexer.ExpectOrInsertSemicolon()
		return ast.Stmt{Loc: loc, Data: &ast.SExpr{Value: expr}}
	}
}

func (p *parser) parseStmtsUpTo(end lexer.T) []ast.Stmt {
	stmts := []ast.Stmt{}
	for {
		if p.lexer.Token == end {
			break
		}
		stmt := p.parseStmt()
		// skip typescript types entirely
		if _, ok := stmt.Data.(*ast.STypeScript); ok {
			continue
		}
		stmts = append(stmts, stmt)
	}
	return stmts
}
