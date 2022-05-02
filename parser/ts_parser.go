package parser

import (
	"github.com/mmmommm/microEsbuild/ast"
	"github.com/mmmommm/microEsbuild/lexer"
	"github.com/mmmommm/microEsbuild/location"
)

// skipTsBinding はtokenを解析してlexerを進める
func (p *parser) skipTsBinding() {
	switch p.lexer.Token {
	case lexer.TIdentifier, lexer.TThis:
		p.lexer.Next()
	case lexer.TOpenBracket: // [
		p.lexer.Next()
		// [,,a] みたいなののケア
		for p.lexer.Token == lexer.TComma {
			p.lexer.Next()
		}
		// ] が来るまでforが回る
		for p.lexer.Token != lexer.TCloseBracket {
			p.skipTsBinding()
			if p.lexer.Token != lexer.TComma {
				break
			}
			p.lexer.Next()
		}
		// ] が来るか確認してきてなかったらエラー返して終了する
		p.lexer.Expect(lexer.TCloseBracket)
	case lexer.TOpenBrace: // {
		p.lexer.Next()
		// } が来るまでforが回る
		for p.lexer.Token != lexer.TCloseBrace {
			foundIdentifier := false
			switch p.lexer.Token {
			case lexer.TDotDotDot: // {...
				p.lexer.Next()
				// {... の後に変数来なかったらエラー返して終了する
				if p.lexer.Token != lexer.TIdentifier {
					p.lexer.Unexpected()
				}
				foundIdentifier = true
				p.lexer.Next()
			case lexer.TIdentifier: // {x
				foundIdentifier = true
				p.lexer.Next()
			case lexer.TStringLiteral, lexer.TNumericLiteral:
				p.lexer.Next()
			default:
				// {if: x}
				if p.lexer.IsIdentifierOrKeyword() {
					p.lexer.Next()
				} else {
					p.lexer.Unexpected()
				}
			}
			// : が来た時で且つTokenが見つかっていない時
			if p.lexer.Token == lexer.TColon || !foundIdentifier {
				p.lexer.Expect(lexer.TColon)
				p.skipTsBinding()
			}
			if p.lexer.Token != lexer.TComma {
				break
			}
			p.lexer.Next()
		}
		p.lexer.Expect(lexer.TCloseBrace)
	default:
		p.lexer.Unexpected()
	}
}

func (p *parser) skipTsFnArgs() {
	p.lexer.Expect(lexer.TOpenParen) // (
	// ) が来るまでforが回る
	// () => {} のパターンもあるのでTokenチェックしない
	for p.lexer.Token != lexer.TCloseParen {
		// (...a)
		if p.lexer.Token == lexer.TDotDotDot {
			p.lexer.Next()
		}
		p.skipTsBinding()

		// (a?)
		if p.lexer.Token == lexer.TQuestion {
			p.lexer.Next()
		}

		// (a:any)
		if p.lexer.Token == lexer.TColon {
			p.lexer.Next()
			p.skipTsType(ast.LLowest)
		}

		if p.lexer.Token != lexer.TComma {
			break
		}
		p.lexer.Next()
	}
	p.lexer.Expect(lexer.TCloseParen)
}

func (p *parser) skipTsParenOrFnType() {
		p.lexer.Expect(lexer.TOpenParen)
		p.skipTsType(ast.LLowest)
		p.lexer.Expect(lexer.TCloseParen)
}

func (p *parser) skipTsType(level ast.L) {
	for {
		switch p.lexer.Token {
		case lexer.TNumericLiteral, lexer.TBigIntegerLiteral, lexer.TStringLiteral, lexer.TNoSubstitutionTemplateLiteral, lexer.TTrue, lexer.TFalse, lexer.TNull, lexer.TVoid:
			p.lexer.Next()
		case lexer.TConst:
			//r := p.lexer.Range()
			p.lexer.Next()

			if p.lexer.Token == lexer.TColon {
			}
		case lexer.TThis:
			p.lexer.Next()
			if p.lexer.IsContextualKeyword("is") && !p.lexer.HasNewlineBefore {
				p.lexer.Next()
				p.skipTsType(ast.LLowest)
				return
			}
		case lexer.TMinus:
			p.lexer.Next()
			if p.lexer.Token == lexer.TBigIntegerLiteral {
				p.lexer.Next()
			} else {
				p.lexer.Expect(lexer.TNumericLiteral)
			}
		// 多分ミス case lexer.TAmpersand, lexer.TBar:
		// "type Foo = | A | B" and "type Foo = & A & B"
		case lexer.TAmpersand, lexer.TBar:
			p.lexer.Next()
			continue
		case lexer.TImport:
			// import('fs')
			p.lexer.Next()
			// [import: number]
			if p.lexer.Token == lexer.TColon {
				return
			}
			p.lexer.Expect(lexer.TOpenParen)     // (
			p.lexer.Expect(lexer.TStringLiteral) // "string"
			// import('./foo.json', { assert: { type: 'json' } })
			if p.lexer.Token == lexer.TComma {
				p.lexer.Next()
				p.skipTsObjectType()

				if p.lexer.Token == lexer.TComma {
					p.lexer.Next()
				}
			}
			p.lexer.Expect(lexer.TCloseParen)
		case lexer.TNew:
			// new() => Foo
			// new<T>() => Foo<T>
			p.lexer.Next()
			// [new: number]
			if p.lexer.Token == lexer.TColon {
				return
			}

			p.skipTsTypeParameters(typeParametersNormal)
			p.skipTsParenOrFnType()

		case lexer.TLessThan:
			// <T>() => Foo<T>
			p.skipTsTypeParameters(typeParametersNormal)
			p.skipTsParenOrFnType()
		case lexer.TOpenParen:
			// (number | string)
			p.skipTsParenOrFnType()
		case lexer.TIdentifier:
			kind := tsTypeIdentifierMap[p.lexer.Identifier.String]
			if kind == tsTypeIdentifierPrefix {
				p.lexer.Next()
				// Valid:
				//   "[keyof: string]"
				//   "{[keyof: string]: number}"
				//
				// Invalid:
				//   "A extends B ? keyof : string"
				//
				if p.lexer.Token != lexer.TColon {
					p.skipTsType(ast.LPrefix)
				}
				break
			}
			checkTypeParameters := true
			if kind == tsTypeIdentifierUnique {
				p.lexer.Next()
				// let foo: unique symbol
				if p.lexer.IsContextualKeyword("symbol") {
					p.lexer.Next()
					break
				}
			} else if kind == tsTypeIdentifierAbstract {
				p.lexer.Next()
				if p.lexer.Token == lexer.TNew {
					continue
				}
			} else if kind == tsTypeIdentifierAsserts {
				p.lexer.Next()
				// "function assert(x: boolean): asserts x"
				// "function assert(x: boolean): asserts x is boolean"
				if !p.lexer.HasNewlineBefore && (p.lexer.Token == lexer.TIdentifier || p.lexer.Token == lexer.TThis) {
					p.lexer.Next()
				}
			} else if kind == tsTypeIdentifierPrimitive {
				p.lexer.Next()
				checkTypeParameters = false
			} else {
				p.lexer.Next()
			}

			// function assert(x: any): x is boolean
			if p.lexer.IsContextualKeyword("is") && !p.lexer.HasNewlineBefore {
				p.lexer.Next()
				p.skipTsType(ast.LLowest)
				return
			}

			// "let foo: any \n <number>foo" must not become a single type
			if checkTypeParameters && !p.lexer.HasNewlineBefore {
				p.skipTsTypeArguments(false /* isInsideJSXElement */)
			}
		case lexer.TTypeof:
			p.lexer.Next()
			// [typeof: number]
			if p.lexer.Token == lexer.TColon {
				return
			}

			if p.lexer.Token == lexer.TImport {
				// typeof import('fs')
				continue
			} else {
				// typeof x などの Tokenが来ない場合
				if !p.lexer.IsIdentifierOrKeyword() {
					p.lexer.Expected(lexer.TIdentifier)
				}
				p.lexer.Next()

				// typeof x.y
				// typeof x.#y
				for p.lexer.Token == lexer.TDot {
					p.lexer.Next()
					if !p.lexer.IsIdentifierOrKeyword() && p.lexer.Token != lexer.TPrivateIdentifier {
						p.lexer.Expected(lexer.TIdentifier)
					}
					p.lexer.Next()
				}

				if !p.lexer.HasNewlineBefore {
					p.skipTsTypeArguments(false /* isInsideJSXElement */)
				}
			}
		case lexer.TOpenBracket:
			// [number, string]
			// [first: number, second: string]
			p.lexer.Next()
			for p.lexer.Token != lexer.TCloseBracket {
				if p.lexer.Token == lexer.TDotDotDot {
					// [...props]
					p.lexer.Next()
				}
				p.skipTsType(ast.LLowest)
				if p.lexer.Token == lexer.TQuestion {
					p.lexer.Next()
				}
				if p.lexer.Token == lexer.TColon {
					p.lexer.Next()
					p.skipTsType(ast.LLowest)
				}
				if p.lexer.Token != lexer.TComma {
					break
				}
				p.lexer.Next()
			}
			p.lexer.Expect(lexer.TCloseBracket)
		case lexer.TOpenBrace:
			p.skipTsObjectType()

		case lexer.TTemplateHead:
			// "`${'a' | 'b'}-${'c' | 'd'}`"
			for {
				p.lexer.Next()
				p.skipTsType(ast.LLowest)
				p.lexer.RescanCloseBraceAsTemplateToken()
				if p.lexer.Token == lexer.TTemplateTail {
					p.lexer.Next()
					break
				}
			}

		default:
			// "[function: number]"
			if p.lexer.IsIdentifierOrKeyword() {
				if p.lexer.Token != lexer.TFunction {
				}
				p.lexer.Next()
				if p.lexer.Token != lexer.TColon {
					p.lexer.Expect(lexer.TColon)
				}
				return
			}

			p.lexer.Unexpected()
		}
		break
	}

	for {
		switch p.lexer.Token {
		case lexer.TBar:
			if level >= ast.LBitwiseOr {
				return
			}
			p.lexer.Next()
			p.skipTsType(ast.LBitwiseOr)

		case lexer.TAmpersand:
			if level >= ast.LBitwiseAnd {
				return
			}
			p.lexer.Next()
			p.skipTsType(ast.LBitwiseAnd)

		case lexer.TExclamation:
			// A postfix "!" is allowed in JSDoc types in TypeScript, which are only
			// present in comments. While it's not valid in a non-comment position,
			// it's still parsed and turned into a soft error by the TypeScript
			// compiler. It turns out parsing this is important for correctness for
			// "as" casts because the "!" token must still be consumed.
			if p.lexer.HasNewlineBefore {
				return
			}
			p.lexer.Next()

		case lexer.TDot:
			p.lexer.Next()
			if !p.lexer.IsIdentifierOrKeyword() {
				p.lexer.Expect(lexer.TIdentifier)
			}
			p.lexer.Next()

			// "{ <A extends B>(): c.d \n <E extends F>(): g.h }" must not become a single type
			if !p.lexer.HasNewlineBefore {
				p.skipTsTypeArguments(false /* isInsideJSXElement */)
			}

		case lexer.TOpenBracket:
			// "{ ['x']: string \n ['y']: string }" must not become a single type
			if p.lexer.HasNewlineBefore {
				return
			}
			p.lexer.Next()
			if p.lexer.Token != lexer.TCloseBracket {
				p.skipTsType(ast.LLowest)
			}
			p.lexer.Expect(lexer.TCloseBracket)

		case lexer.TExtends:
			// "{ x: number \n extends: boolean }" must not become a single type
			if p.lexer.HasNewlineBefore || level >= ast.LConditional {
				return
			}
			p.lexer.Next()

			// The type following "extends" is not permitted to be another conditional type
			p.skipTsType(ast.LConditional)
			p.lexer.Expect(lexer.TQuestion)
			p.skipTsType(ast.LLowest)
			p.lexer.Expect(lexer.TColon)
			p.skipTsType(ast.LLowest)
		default:
			return
		}
	}
}

func (p *parser) skipTsObjectType() {
	p.lexer.Expect(lexer.TOpenBrace)

	for p.lexer.Token != lexer.TCloseBrace {
		// "{ -readonly [K in keyof T]: T[K] }"
		// "{ +readonly [K in keyof T]: T[K] }"
		if p.lexer.Token == lexer.TPlus || p.lexer.Token == lexer.TMinus {
			p.lexer.Next()
		}

		// Skip over modifiers and the property identifier
		foundKey := false
		for p.lexer.IsIdentifierOrKeyword() ||
			p.lexer.Token == lexer.TStringLiteral ||
			p.lexer.Token == lexer.TNumericLiteral {
			p.lexer.Next()
			foundKey = true
		}

		if p.lexer.Token == lexer.TOpenBracket {
			// Index signature or computed property
			p.lexer.Next()
			p.skipTsType(ast.LLowest)

			// "{ [key: string]: number }"
			// "{ readonly [K in keyof T]: T[K] }"
			if p.lexer.Token == lexer.TColon {
				p.lexer.Next()
				p.skipTsType(ast.LLowest)
			} else if p.lexer.Token == lexer.TIn {
				p.lexer.Next()
				p.skipTsType(ast.LLowest)
				if p.lexer.IsContextualKeyword("as") {
					// "{ [K in keyof T as `get-${K}`]: T[K] }"
					p.lexer.Next()
					p.skipTsType(ast.LLowest)
				}
			}

			p.lexer.Expect(lexer.TCloseBracket)

			// "{ [K in keyof T]+?: T[K] }"
			// "{ [K in keyof T]-?: T[K] }"
			if p.lexer.Token == lexer.TPlus || p.lexer.Token == lexer.TMinus {
				p.lexer.Next()
			}

			foundKey = true
		}

		// "?" indicates an optional property
		// "!" indicates an initialization assertion
		if foundKey && (p.lexer.Token == lexer.TQuestion || p.lexer.Token == lexer.TExclamation) {
			p.lexer.Next()
		}

		// Type parameters come right after the optional mark
		p.skipTsTypeParameters(typeParametersNormal)

		switch p.lexer.Token {
		case lexer.TColon:
			// Regular property
			if !foundKey {
				p.lexer.Expect(lexer.TIdentifier)
			}
			p.lexer.Next()
			p.skipTsType(ast.LLowest)

		case lexer.TOpenParen:
			// Method signature
			p.skipTsFnArgs()
			if p.lexer.Token == lexer.TColon {
				p.lexer.Next()
				p.skipTsType(ast.LLowest)
			}

		default:
			if !foundKey {
				p.lexer.Unexpected()
			}
		}

		switch p.lexer.Token {
		case lexer.TCloseBrace:

		case lexer.TComma, lexer.TSemicolon:
			p.lexer.Next()

		default:
			if !p.lexer.HasNewlineBefore {
				p.lexer.Unexpected()
			}
		}
	}

	p.lexer.Expect(lexer.TCloseBrace)
}

type typeParameters uint8

const (
	typeParametersNormal typeParameters = iota
	typeParametersWithInOutVarianceAnnotations
)

// This is the type parameter declarations that go with other symbol
// declarations (class, function, type, etc.)
func (p *parser) skipTsTypeParameters(mode typeParameters) {
	if p.lexer.Token == lexer.TLessThan {
		p.lexer.Next()

		for {
			hasIn := false
			hasOut := false
			expectIdentifier := true
			invalidModifierRange := location.Range{}

			// Scan over a sequence of "in" and "out" modifiers (a.k.a. optional variance annotations)
			for {
				if p.lexer.Token == lexer.TIn {
					if invalidModifierRange.Len == 0 && (mode != typeParametersWithInOutVarianceAnnotations || hasIn || hasOut) {
						// Valid:
						//   "type Foo<in T> = T"
						// Invalid:
						//   "type Foo<in in T> = T"
						//   "type Foo<out in T> = T"
						invalidModifierRange = p.lexer.Range()
					}
					p.lexer.Next()
					hasIn = true
					expectIdentifier = true
					continue
				}

				if p.lexer.IsContextualKeyword("out") {
					r := p.lexer.Range()
					if invalidModifierRange.Len == 0 && mode != typeParametersWithInOutVarianceAnnotations {
						invalidModifierRange = r
					}
					p.lexer.Next()
					if invalidModifierRange.Len == 0 && hasOut && (p.lexer.Token == lexer.TIn || p.lexer.Token == lexer.TIdentifier) {
						// Valid:
						//   "type Foo<out T> = T"
						//   "type Foo<out out> = T"
						//   "type Foo<out out, T> = T"
						//   "type Foo<out out = T> = T"
						//   "type Foo<out out extends T> = T"
						// Invalid:
						//   "type Foo<out out in T> = T"
						//   "type Foo<out out T> = T"
						invalidModifierRange = r
					}
					hasOut = true
					expectIdentifier = false
					continue
				}

				break
			}

			// Only report an error for the first invalid modifier
			if invalidModifierRange.Len > 0 {
				// p.log.Add(location.Error, &p.tracker, invalidModifierRange, fmt.Sprintf(
				// 	"The modifier %q is not valid here:", p.source.TextForRange(invalidModifierRange)))
			}

			// expectIdentifier => Mandatory identifier (e.g. after "type Foo <in ___")
			// !expectIdentifier => Optional identifier (e.g. after "type Foo <out ___" since "out" may be the identifier)
			if expectIdentifier || p.lexer.Token == lexer.TIdentifier {
				p.lexer.Expect(lexer.TIdentifier)
			}

			// "class Foo<T extends number> {}"
			if p.lexer.Token == lexer.TExtends {
				p.lexer.Next()
				p.skipTsType(ast.LLowest)
			}

			// "class Foo<T = void> {}"
			if p.lexer.Token == lexer.TEquals {
				p.lexer.Next()
				p.skipTsType(ast.LLowest)
			}

			if p.lexer.Token != lexer.TComma {
				break
			}
			p.lexer.Next()
			if p.lexer.Token == lexer.TGreaterThan {
				break
			}
		}

		p.lexer.ExpectGreaterThan(false /* isInsideJSXElement */)
	}
}

func (p *parser) skipTsTypeArguments(isInsideJSXElement bool) bool {
	switch p.lexer.Token {
	case lexer.TLessThan, lexer.TLessThanEquals,
		lexer.TLessThanLessThan, lexer.TLessThanLessThanEquals:
	default:
		return false
	}

	p.lexer.ExpectLessThan(false /* isInsideJSXElement */)

	for {
		p.skipTsType(ast.LLowest)
		if p.lexer.Token != lexer.TComma {
			break
		}
		p.lexer.Next()
	}

	// This type argument list must end with a ">"
	p.lexer.ExpectGreaterThan(isInsideJSXElement)
	return true
}

func (p *parser) skipTypeScriptInterfaceStmt() {
	p.lexer.Expect(lexer.TIdentifier)

	p.skipTsTypeParameters(typeParametersWithInOutVarianceAnnotations)

	if p.lexer.Token == lexer.TExtends {
		p.lexer.Next()
		for {
			p.skipTsType(ast.LLowest)
			if p.lexer.Token != lexer.TComma {
				break
			}
			p.lexer.Next()
		}
	}

	if p.lexer.IsContextualKeyword("implements") {
		p.lexer.Next()
		for {
			p.skipTsType(ast.LLowest)
			if p.lexer.Token != lexer.TComma {
				break
			}
			p.lexer.Next()
		}
	}

	p.skipTsObjectType()
}

func (p *parser) skipTsTypeStmt() {
	if p.lexer.Token == lexer.TOpenBrace {
		// "export type {foo}"
		// "export type {foo} from 'bar'"
		p.parseExportClause()
		if p.lexer.IsContextualKeyword("from") {
			p.lexer.Next()
			p.parsePath()
		}
		p.lexer.ExpectOrInsertSemicolon()
		return
	}

	p.lexer.Expect(lexer.TIdentifier)

	p.skipTsTypeParameters(typeParametersWithInOutVarianceAnnotations)
	p.lexer.Expect(lexer.TEquals)
	p.skipTsType(ast.LLowest)
	p.lexer.ExpectOrInsertSemicolon()
}

// func (p *parser) parseTypeScriptDecorators(tsDecoratorScope *ast.Scope) []ast.Expr {
// 	var tsDecorators []ast.Expr

// 	// TypeScript decorators cause us to temporarily revert to the scope that
// 	// encloses the class declaration, since that's where the generated code
// 	// for TypeScript decorators will be inserted.
// 	oldScope := p.currentScope
// 	p.currentScope = tsDecoratorScope

// 	for p.lexer.Token == lexer.TAt {
// 		loc := p.lexer.Loc()
// 		p.lexer.Next()

// 		// Parse a new/call expression with "exprFlagTSDecorator" so we ignore
// 		// EIndex expressions, since they may be part of a computed property:
// 		//
// 		//   class Foo {
// 		//     @foo ['computed']() {}
// 		//   }
// 		//
// 		// This matches the behavior of the TypeScript compiler.
// 		value := p.parseExprWithFlags(ast.LNew, exprFlagTSDecorator)
// 		value.Loc = loc
// 		tsDecorators = append(tsDecorators, value)
// 	}

// 	// Avoid "popScope" because this decorator scope is not hierarchical
// 	p.currentScope = oldScope

// 	return tsDecorators
// }

// func (p *parser) logInvalidDecoratorError(classKeyword location.Range) {
// 	if p.lexer.Token == lexer.TAt {
// 		// Forbid decorators inside class expressions
// 		// p.lexer.AddRangeErrorWithNotes(p.lexer.Range(), "Decorators can only be used with class declarations in TypeScript",
// 		// 	[]location.MsgData{p.tracker.MsgData(classKeyword, "This is a class expression, not a class declaration:")})

// 		// Parse and discard decorators for error recovery
// 		scopeIndex := len(p.scopesInOrder)
// 		p.parseTypeScriptDecorators(p.currentScope)
// 		p.discardScopesUpTo(scopeIndex)
// 	}
// }

// // func (p *parser) logMisplacedDecoratorError(tsDecorators *deferredTSDecorators) {
// // 	found := fmt.Sprintf("%q", p.lexer.Raw())
// // 	if p.lexer.Token == lexer.TEndOfFile {
// // 		found = "end of file"
// // 	}

// // 	// Try to be helpful by pointing out the decorator
// // 	//p.lexer.AddRangeErrorWithNotes(p.lexer.Range(), fmt.Sprintf("Expected \"class\" after TypeScript decorator but found %s", found), []location.MsgData{
// // 		// p.tracker.MsgData(location.Range{Loc: tsDecorators.values[0].Loc}, "The preceding TypeScript decorator is here:"),
// // 		// {Text: "Decorators can only be used with class declarations in TypeScript."},
// // 	//})
// // 	p.discardScopesUpTo(tsDecorators.scopeIndex)
// }

// func (p *parser) parseTypeScriptEnumStmt(loc location.Loc, opts parseStmtOpts) ast.Stmt {
// 	p.lexer.Expect(lexer.TEnum)
// 	nameLoc := p.lexer.Loc()
// 	nameText := p.lexer.Identifier.String
// 	p.lexer.Expect(lexer.TIdentifier)
// 	name := ast.LocRef{Loc: nameLoc, Ref: ast.InvalidRef}

// 	// Generate the namespace object
// 	exportedMembers := p.getOrCreateExportedNamespaceMembers(nameText, opts.isExport)
// 	tsNamespace := &ast.TSNamespaceScope{
// 		ExportedMembers: exportedMembers,
// 		ArgRef:          ast.InvalidRef,
// 		IsEnumScope:     true,
// 	}
// 	enumMemberData := &ast.TSNamespaceMemberNamespace{
// 		ExportedMembers: exportedMembers,
// 	}

// 	// Declare the enum and create the scope
// 	name.Ref = p.declareSymbol(ast.SymbolTSEnum, nameLoc, nameText)
// 	p.pushScopeForParsePass(ast.ScopeEntry, loc)
// 	p.currentScope.TSNamespace = tsNamespace
// 	p.refToTSNamespaceMemberData[name.Ref] = enumMemberData

// 	p.lexer.Expect(lexer.TOpenBrace)
// 	values := []ast.EnumValue{}

// 	oldFnOrArrowData := p.fnOrArrowDataParse
// 	p.fnOrArrowDataParse = fnOrArrowDataParse{
// 		isThisDisallowed: true,
// 		needsAsyncLoc:    location.Loc{Start: -1},
// 	}

// 	// Parse the body
// 	for p.lexer.Token != lexer.TCloseBrace {
// 		nameRange := p.lexer.Range()
// 		value := ast.EnumValue{
// 			Loc: nameRange.Loc,
// 			Ref: ast.InvalidRef,
// 		}

// 		// Parse the name
// 		var nameText string
// 		if p.lexer.Token == lexer.TStringLiteral {
// 			value.Name = p.lexer.StringLiteral()
// 			nameText = util.UTF16ToString(value.Name)
// 		} else if p.lexer.IsIdentifierOrKeyword() {
// 			nameText = p.lexer.Identifier.String
// 			value.Name = util.StringToUTF16(nameText)
// 		} else {
// 			p.lexer.Expect(lexer.TIdentifier)
// 		}
// 		p.lexer.Next()

// 		// Identifiers can be referenced by other values
// 		if !opts.isTypeScriptDeclare && lexer.IsIdentifierUTF16(value.Name) {
// 			value.Ref = p.declareSymbol(ast.SymbolOther, value.Loc, util.UTF16ToString(value.Name))
// 		}

// 		// Parse the initializer
// 		if p.lexer.Token == lexer.TEquals {
// 			p.lexer.Next()
// 			value.ValueOrNil = p.parseExpr(ast.LComma)
// 		}

// 		values = append(values, value)

// 		// Add this enum value as a member of the enum's namespace
// 		exportedMembers[nameText] = ast.TSNamespaceMember{
// 			Loc:         value.Loc,
// 			Data:        &ast.TSNamespaceMemberProperty{},
// 			IsEnumValue: true,
// 		}

// 		if p.lexer.Token != lexer.TComma && p.lexer.Token != lexer.TSemicolon {
// 			if p.lexer.IsIdentifierOrKeyword() || p.lexer.Token == lexer.TStringLiteral {
// 				var errorLoc location.Loc
// 				var errorText string

// 				if value.ValueOrNil.Data == nil {
// 					errorLoc = location.Loc{Start: nameRange.End()}
// 					errorText = fmt.Sprintf("Expected \",\" after %q in enum", nameText)
// 				} else {
// 					var nextName string
// 					if p.lexer.Token == lexer.TStringLiteral {
// 						nextName = util.UTF16ToString(p.lexer.StringLiteral())
// 					} else {
// 						nextName = p.lexer.Identifier.String
// 					}
// 					errorLoc = p.lexer.Loc()
// 					errorText = fmt.Sprintf("Expected \",\" before %q in enum", nextName)
// 				}

// 				data := p.tracker.MsgData(location.Range{Loc: errorLoc}, errorText)
// 				data.Location.Suggestion = ","
// 				// p.log.AddMsg(location.Msg{Kind: location.Error, Data: data})
// 				panic(lexer.LexerPanic{})
// 			}
// 			break
// 		}
// 		p.lexer.Next()
// 	}

// 	p.fnOrArrowDataParse = oldFnOrArrowData

// 	if !opts.isTypeScriptDeclare {
// 		// Avoid a collision with the enum closure argument variable if the
// 		// enum exports a symbol with the same name as the enum itself:
// 		//
// 		//   enum foo {
// 		//     foo = 123,
// 		//     bar = foo,
// 		//   }
// 		//
// 		// TypeScript generates the following code in this case:
// 		//
// 		//   var foo;
// 		//   (function (foo) {
// 		//     foo[foo["foo"] = 123] = "foo";
// 		//     foo[foo["bar"] = 123] = "bar";
// 		//   })(foo || (foo = {}));
// 		//
// 		// Whereas in this case:
// 		//
// 		//   enum foo {
// 		//     bar = foo as any,
// 		//   }
// 		//
// 		// TypeScript generates the following code:
// 		//
// 		//   var foo;
// 		//   (function (foo) {
// 		//     foo[foo["bar"] = foo] = "bar";
// 		//   })(foo || (foo = {}));
// 		//
// 		if _, ok := p.currentScope.Members[nameText]; ok {
// 			// Add a "_" to make tests easier to read, since non-bundler tests don't
// 			// run the renamer. For external-facing things the renamer will avoid
// 			// collisions automatically so this isn't important for correctness.
// 			tsNamespace.ArgRef = p.newSymbol(ast.SymbolHoisted, "_"+nameText)
// 			p.currentScope.Generated = append(p.currentScope.Generated, tsNamespace.ArgRef)
// 		} else {
// 			tsNamespace.ArgRef = p.declareSymbol(ast.SymbolHoisted, nameLoc, nameText)
// 		}
// 		p.refToTSNamespaceMemberData[tsNamespace.ArgRef] = enumMemberData

// 		p.popScope()
// 	}

// 	p.lexer.Expect(lexer.TCloseBrace)

// 	if opts.isTypeScriptDeclare {
// 		if opts.isNamespaceScope && opts.isExport {
// 			p.hasNonLocalExportDeclareInsideNamespace = true
// 		}

// 		return ast.Stmt{Loc: loc, Data: &ast.STypeScript{}}
// 	}

// 	return ast.Stmt{Loc: loc, Data: &ast.SEnum{
// 		Name:     name,
// 		Arg:      tsNamespace.ArgRef,
// 		Values:   values,
// 		IsExport: opts.isExport,
// 	}}
// }

// // This assumes the caller has already parsed the "import" token
// func (p *parser) parseTypeScriptImportEqualsStmt(loc location.Loc, opts parseStmtOpts, defaultNameLoc location.Loc, defaultName string) ast.Stmt {
// 	p.lexer.Expect(lexer.TEquals)

// 	kind := p.selectLocalKind(ast.LocalConst)
// 	name := p.lexer.Identifier
// 	value := ast.Expr{Loc: p.lexer.Loc(), Data: &ast.EIdentifier{Ref: p.storeNameInRef(name)}}
// 	p.lexer.Expect(lexer.TIdentifier)

// 	if name.String == "require" && p.lexer.Token == lexer.TOpenParen {
// 		// "import ns = require('x')"
// 		p.lexer.Next()
// 		path := ast.Expr{Loc: p.lexer.Loc(), Data: &ast.EString{Value: p.lexer.StringLiteral()}}
// 		p.lexer.Expect(lexer.TStringLiteral)
// 		p.lexer.Expect(lexer.TCloseParen)
// 		value.Data = &ast.ECall{
// 			Target: value,
// 			Args:   []ast.Expr{path},
// 		}
// 	} else {
// 		// "import Foo = Bar"
// 		// "import Foo = Bar.Baz"
// 		for p.lexer.Token == lexer.TDot {
// 			p.lexer.Next()
// 			value.Data = &ast.EDot{
// 				Target:  value,
// 				Name:    p.lexer.Identifier.String,
// 				NameLoc: p.lexer.Loc(),
// 			}
// 			p.lexer.Expect(lexer.TIdentifier)
// 		}
// 	}

// 	p.lexer.ExpectOrInsertSemicolon()

// 	if opts.isTypeScriptDeclare {
// 		// "import type foo = require('bar');"
// 		// "import type foo = bar.baz;"
// 		return ast.Stmt{Loc: loc, Data: &ast.STypeScript{}}
// 	}

// 	ref := p.declareSymbol(ast.SymbolConst, defaultNameLoc, defaultName)
// 	decls := []ast.Decl{{
// 		Binding:    ast.Binding{Loc: defaultNameLoc, Data: &ast.BIdentifier{Ref: ref}},
// 		ValueOrNil: value,
// 	}}

// 	return ast.Stmt{Loc: loc, Data: &ast.SLocal{
// 		Kind:              kind,
// 		Decls:             decls,
// 		IsExport:          opts.isExport,
// 		WasTSImportEquals: true,
// 	}}
// }

type tsTypeIdentifierKind uint8

const (
	tsTypeIdentifierNormal tsTypeIdentifierKind = iota
	tsTypeIdentifierUnique
	tsTypeIdentifierAbstract
	tsTypeIdentifierAsserts
	tsTypeIdentifierPrefix
	tsTypeIdentifierPrimitive
)

// Use a map to improve lookup speed
var tsTypeIdentifierMap = map[string]tsTypeIdentifierKind{
	"unique":   tsTypeIdentifierUnique,
	"abstract": tsTypeIdentifierAbstract,
	"asserts":  tsTypeIdentifierAsserts,

	"keyof":    tsTypeIdentifierPrefix,
	"readonly": tsTypeIdentifierPrefix,
	"infer":    tsTypeIdentifierPrefix,

	"any":       tsTypeIdentifierPrimitive,
	"never":     tsTypeIdentifierPrimitive,
	"unknown":   tsTypeIdentifierPrimitive,
	"undefined": tsTypeIdentifierPrimitive,
	"object":    tsTypeIdentifierPrimitive,
	"number":    tsTypeIdentifierPrimitive,
	"string":    tsTypeIdentifierPrimitive,
	"boolean":   tsTypeIdentifierPrimitive,
	"bigint":    tsTypeIdentifierPrimitive,
	"symbol":    tsTypeIdentifierPrimitive,
}