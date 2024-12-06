package main

import (
	"fmt"
	"os"
)

type Pos struct {
	x, y int
}

func main() {

	// get command line arguments
	if len(os.Args) < 2 {
		panic("Usage: go run main.go <input_file>")
	}

	var input = os.Args[1]

	// read input file
	file, err := os.Open(input)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var currentPos Pos = Pos{0, 0}
	var grid = make(map[int]map[int]int)
	grid[currentPos.x] = make(map[int]int)
	grid[currentPos.x][currentPos.y] = 1

	// parse input
	for {
		var line string
		_, err := fmt.Fscanln(file, &line)
		if err != nil {
			break
		}

		for _, c := range line {
			switch c {
			case '^':
				currentPos.y++
			case 'v':
				currentPos.y--
			case '>':
				currentPos.x++
			case '<':
				currentPos.x--
			default:
				panic("Invalid character")
			}

			if _, ok := grid[currentPos.x]; !ok {
				grid[currentPos.x] = make(map[int]int)
			}

			grid[currentPos.x][currentPos.y]++
		}
	}

	count := 0
	for _, row := range grid {
		count += len(row)
	}

	fmt.Println("Part 1:", count)
}
