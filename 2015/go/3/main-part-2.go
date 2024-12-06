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

	var santa Pos = Pos{0, 0}
	var robo_santa Pos = Pos{0, 0}
	var grid = make(map[int]map[int]int)
	grid[santa.x] = make(map[int]int)
	grid[santa.x][santa.y] = 1
	grid[robo_santa.x][robo_santa.y] += 1

	var santaTurn = true
	for {
		var line string
		_, err := fmt.Fscanln(file, &line)
		if err != nil {
			break
		}

		for _, c := range line {
			var current_santa *Pos
			if santaTurn {
				current_santa = &santa
			} else {
				current_santa = &robo_santa
			}

			switch c {
			case '^':
				current_santa.y++
			case 'v':
				current_santa.y--
			case '>':
				current_santa.x++
			case '<':
				current_santa.x--
			default:
				panic("Invalid character")
			}

			if _, ok := grid[current_santa.x]; !ok {
				grid[current_santa.x] = make(map[int]int)
			}

			grid[current_santa.x][current_santa.y]++
			santaTurn = !santaTurn
		}
	}

	var total = 0
	for _, row := range grid {
		for _, _ = range row {
			total++
		}
	}

	fmt.Println("Part 2:", total)

}
