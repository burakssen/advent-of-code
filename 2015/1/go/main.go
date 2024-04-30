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

	// loop line by line
	count := 0
	for {
		var line string
		_, err := fmt.Fscanln(file, &line)
		if err != nil {
			break
		}

		// loop char by char
		for _, c := range line {
			if c == '(' {
				count++
			} else if c == ')' {
				count--
			}
		}
	}

	fmt.Println(count)
}
