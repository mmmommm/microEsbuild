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

// func Parse(source location.Source) (result *ast.AST) {
// 	p := newParser(source, lexer.NewLexer(source))
// }
