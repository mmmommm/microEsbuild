package parser

import (
	"testing"

	"github.com/mmmommm/microEsbuild/ast"
	"github.com/mmmommm/microEsbuild/renamer"
	"github.com/mmmommm/microEsbuild/util"
)

func expectPrintedCommon(t *testing.T, contents string, expected string) {
	t.Helper()
	t.Run(contents, func(t *testing.T) {
		t.Helper()
		text := ""
		tree := Parse(util.ConvertStringToSource(contents))
		util.AssertEqualWithDiff(t, text, "")
		symbols := ast.NewSymbolMap(1)
		symbols.SymbolsForSource[0] = tree.Symbols
		r := renamer.NewNoOpRenamer(symbols)
		js := printer.Print(tree, symbols, r).JS
		util.AssertEqualWithDiff(t, string(js), expected)
	})
}

func expectPrinted(t *testing.T, contents string, expected string) {
	t.Helper()
	expectPrintedCommon(t, contents, expected)
}