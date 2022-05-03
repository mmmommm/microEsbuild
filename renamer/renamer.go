package renamer

import (
	"github.com/mmmommm/microEsbuild/ast"
	"github.com/mmmommm/microEsbuild/lexer"
)

func ComputeReservedNames(moduleScopes []*ast.Scope, symbols ast.SymbolMap) map[string]uint32 {
	names := make(map[string]uint32)

	// All keywords and strict mode reserved words are reserved names
	for k := range lexer.Keywords {
		names[k] = 1
	}
	for k := range lexer.StrictModeReservedWords {
		names[k] = 1
	}

	// All unbound symbols must be reserved names
	for _, scope := range moduleScopes {
		computeReservedNamesForScope(scope, symbols, names)
	}

	return names
}

func computeReservedNamesForScope(scope *ast.Scope, symbols ast.SymbolMap, names map[string]uint32) {
	for _, member := range scope.Members {
		symbol := symbols.Get(member.Ref)
		if symbol.Kind == ast.SymbolUnbound || symbol.Flags.Has(ast.MustNotBeRenamed) {
			names[symbol.OriginalName] = 1
		}
	}
	for _, ref := range scope.Generated {
		symbol := symbols.Get(ref)
		if symbol.Kind == ast.SymbolUnbound || symbol.Flags.Has(ast.MustNotBeRenamed) {
			names[symbol.OriginalName] = 1
		}
	}

	// If there's a direct "eval" somewhere inside the current scope, continue
	// traversing down the scope tree until we find it to get all reserved names
	if scope.ContainsDirectEval {
		for _, child := range scope.Children {
			if child.ContainsDirectEval {
				computeReservedNamesForScope(child, symbols, names)
			}
		}
	}
}

type Renamer interface {
	NameForSymbol(ref ast.Ref) string
}

type noOpRenamer struct {
	symbols ast.SymbolMap
}

func NewNoOpRenamer(symbols ast.SymbolMap) Renamer {
	return &noOpRenamer{
		symbols: symbols,
	}
}

func (r *noOpRenamer) NameForSymbol(ref ast.Ref) string {
	ref = ast.FollowSymbols(r.symbols, ref)
	return r.symbols.Get(ref).OriginalName
}