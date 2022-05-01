package util

import (
	"github.com/mmmommm/microEsbuild/location"
	"github.com/mmmommm/microEsbuild/lexer"
)

func convertStringToSource(contents string) location.Source {
	return location.Source{
		Index:          0,
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