package main

import (
	"fmt"
	"os"
	"strings"
)

const (
	openGround = '.'
	trees      = '|'
	lumberyard = '#'
)

type point struct{ x, y int }

var directions = []point{
	{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1},
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		os.Exit(1)
	}
	content, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Printf("Error reading file: %v\n", err)
		os.Exit(1)
	}

	lines := strings.Split(strings.TrimSpace(string(content)), "\n")
	grid := make([][]rune, len(lines))
	for i, line := range lines {
		grid[i] = []rune(line)
	}

	part1 := simulateForMinutes(grid, 10)
	part2 := simulateForLargeMinutes(grid, 1000000000)

	fmt.Printf("Part 1: %d\n", resourceValue(part1))
	fmt.Printf("Part 2: %d\n", resourceValue(part2))
}

func simulate(grid [][]rune) [][]rune {
	nextGrid := make([][]rune, len(grid))
	for i := range grid {
		nextGrid[i] = make([]rune, len(grid[i]))
		for j := range grid[i] {
			nextGrid[i][j] = nextState(grid, i, j)
		}
	}
	return nextGrid
}

func nextState(grid [][]rune, x, y int) rune {
	adjacent := countAdjacent(grid, x, y)
	current := grid[x][y];
	switch  current {
	case openGround:
		if adjacent[trees] >= 3 { return trees }
	case trees:
		if adjacent[lumberyard] >= 3 { return lumberyard }
	case lumberyard:
		if adjacent[lumberyard] >= 1 && adjacent[trees] >= 1 { return lumberyard }
		return openGround
	}
	return current
}

func countAdjacent(grid [][]rune, x, y int) map[rune]int {
	counts := map[rune]int{openGround: 0, trees: 0, lumberyard: 0}
	for _, dir := range directions {
		if nx, ny := x+dir.x, y+dir.y; nx >= 0 && ny >= 0 && nx < len(grid) && ny < len(grid[0]) {
			counts[grid[nx][ny]]++
		}
	}
	return counts
}

func countResources(grid [][]rune) (wooded, lumberyards int) {
	for _, row := range grid {
		for _, cell := range row {
			if cell == trees { wooded++ }
			if cell == lumberyard { lumberyards++ }
		}
	}
	return
}

func resourceValue(grid [][]rune) int {
	wooded, lumberyards := countResources(grid)
	return wooded * lumberyards
}

func simulateForMinutes(grid [][]rune, minutes int) [][]rune {
	for minute := 0; minute < minutes; minute++ {
		grid = simulate(grid)
	}
	return grid
}

func simulateForLargeMinutes(grid [][]rune, targetMinutes int) [][]rune {
	seen := make(map[string]int)
	for minute := 0; minute < targetMinutes; minute++ {
		grid = simulate(grid)
		if value, ok := seen[gridToString(grid)]; ok {
			loopSize := minute - value
			targetMinute := (targetMinutes - minute - 1) % loopSize
			for i := 0; i < targetMinute; i++ {
				grid = simulate(grid)
			}
			break
		}
		seen[gridToString(grid)] = minute
	}
	return grid
}

func gridToString(grid [][]rune) string {
	var sb strings.Builder
	for _, row := range grid {
		sb.WriteString(string(row))
		sb.WriteRune('\n')
	}
	return sb.String()
}
