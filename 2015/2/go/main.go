package main

import (
	"fmt"
	"os"
)

func main() {
	// get command line arguments
	args := os.Args[1:]

	// check if there are any arguments
	if len(args) < 1 {
		fmt.Println("Usage: go run main.go <input_file>")
		return
	}

	// get the input file

	// read the input file
	file, err := os.Open(args[0])
	if err != nil {
		fmt.Println(err)
		return
	}

	total := 0
	for {
		var line string
		_, err := fmt.Fscanln(file, &line)
		if err != nil {
			break
		}

		var a, b, c int
		_, err = fmt.Sscanf(line, "%dx%dx%d", &a, &b, &c)
		if err != nil {
			fmt.Println(err)
			return
		}

		area := 2*a*b + 2*b*c + 2*c*a

		smallest := a * b

		if b*c < smallest {
			smallest = b * c
		}

		if c*a < smallest {
			smallest = c * a
		}

		area += smallest
		total += area
	}

	fmt.Println(total)
}
