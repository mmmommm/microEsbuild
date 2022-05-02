package util

import (
	"github.com/mmmommm/microEsbuild/lexer"
	"github.com/mmmommm/microEsbuild/location"
)

func convertStringToSource(contents string) location.Source {
	return location.Source{
		Index: 0,
		// KeyPath:        logger.Path{Text: "<stdin>"},
		// PrettyPath:     "<stdin>",
		Contents:       contents,
		IdentifierName: "stdin",
	}
}

func CheckLexToken(contents string) lexer.Lexer {
	lexer := lexer.NewLexer(convertStringToSource(contents))
	return lexer
}
