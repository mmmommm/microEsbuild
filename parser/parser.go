package parser

import (
	// "github.com/mmmommm/microEsbuild/ast"
	"github.com/mmmommm/microEsbuild/lexer"
	"github.com/mmmommm/microEsbuild/location"
)

type parser struct {
	source location.Source
	lexer lexer.Lexer
}

func ConvertStringToSource(contents string) location.Source {
	return location.Source{
		Index:          0,
		// KeyPath:        logger.Path{Text: "<stdin>"},
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
		source:            source,
		lexer:             lexer,
	}
	return p
}

// func Parse(source location.Source) (result *ast.AST) {
// 	p := newParser(source, lexer.NewLexer(source))
// }