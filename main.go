package main

import (
	"fmt"
	"os"

	"github.com/mmmommm/microEsbuild/util"
)

func main() {
	input := os.Args[1]
	res := util.CheckLexToken(input)
	// ast, ok := parser.Parse(input)
	// if !ok {
	// 	return
	// }
	fmt.Printf("parsed: %v\n", res)
}
