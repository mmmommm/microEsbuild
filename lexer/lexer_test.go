package lexer

import (
	"testing"

	"github.com/mmmommm/microEsbuild/location"
)

func ConvertStringToSource(t *testing.T, contents string) location.Source {
	return location.Source{
		Index: 0,
		// KeyPath:        logger.Path{Text: "<stdin>"},
		// PrettyPath:     "<stdin>",
		Contents:       contents,
		IdentifierName: "stdin",
	}
}

func CheckLexToken(t *testing.T, contents string) T {
	lexer := NewLexer(ConvertStringToSource(t, contents))
	return lexer.Token
}

func AssertEqual(t *testing.T, observed interface{}, expected interface{}) {
	t.Helper()
	if observed != expected {
		t.Fatalf("%s != %s", observed, expected)
	}
}

func TestToken(t *testing.T) {
	expected := []struct {
		contents string
		token    T
	}{
		{"", TEndOfFile},
		{"\x00", TSyntaxError},

		// "#!/usr/bin/env node"
		{"#!", THashbang},

		// Punctuation
		{"(", TOpenParen},
		{")", TCloseParen},
		{"[", TOpenBracket},
		{"]", TCloseBracket},
		{"{", TOpenBrace},
		{"}", TCloseBrace},

		// Reserved words
		{"break", TBreak},
		{"case", TCase},
		{"catch", TCatch},
		{"class", TClass},
		{"const", TConst},
		{"continue", TContinue},
		{"debugger", TDebugger},
		{"default", TDefault},
		{"delete", TDelete},
		{"do", TDo},
		{"else", TElse},
		{"enum", TEnum},
		{"export", TExport},
		{"extends", TExtends},
		{"false", TFalse},
		{"finally", TFinally},
		{"for", TFor},
		{"function", TFunction},
		{"if", TIf},
		{"import", TImport},
		{"in", TIn},
		{"instanceof", TInstanceof},
		{"new", TNew},
		{"null", TNull},
		{"return", TReturn},
		{"super", TSuper},
		{"switch", TSwitch},
		{"this", TThis},
		{"throw", TThrow},
		{"true", TTrue},
		{"try", TTry},
		{"typeof", TTypeof},
		{"var", TVar},
		{"void", TVoid},
		{"while", TWhile},
		{"with", TWith},
	}

	for _, it := range expected {
		contents := it.contents
		token := it.token
		t.Run(contents, func(t *testing.T) {
			AssertEqual(t, CheckLexToken(t, contents), token)
		})
	}
}
