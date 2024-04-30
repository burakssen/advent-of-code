package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args[1:]
	if len(args) < 1 {
		fmt.Println("Usage: go run main.go <input_file>")
		return
	}
	file, err := os.Open(args[0])
	if err != nil {
		fmt.Println(err)
		return
	}

}
