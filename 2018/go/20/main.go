package main

import (
	"fmt"
	"os"
	"strings"
)

type Position struct{ x, y int }

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		os.Exit(1)
	}

	content, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Println("Error reading file:", err)
		os.Exit(1)
	}

	input := strings.TrimSpace(string(content))
	maxDoors, roomsOver1000 := solveMaze(input)
	fmt.Printf("Part 1: %d\n", maxDoors)
	fmt.Printf("Part 2: %d\n", roomsOver1000)
}

func solveMaze(regex string) (maxDoors, roomsOver1000 int) {
	distances := map[Position]int{{0, 0}: 0}
	pos, stack := Position{0, 0}, []Position{}
	distance := 0

	for _, ch := range regex {
		switch ch {
		case 'N', 'S', 'E', 'W':
			pos, distance = move(pos, ch, distance)
			updateDistance(distances, pos, distance)
		case '(':
			stack = append(stack, pos)
		case ')', '|':
			pos, distance = stack[len(stack)-1], distances[stack[len(stack)-1]]
			if ch == ')' {
				stack = stack[:len(stack)-1]
			}
		}
	}

	for _, dist := range distances {
		if dist > maxDoors {
			maxDoors = dist
		}
		if dist >= 1000 {
			roomsOver1000++
		}
	}

	return maxDoors, roomsOver1000
}

func move(pos Position, direction rune, distance int) (Position, int) {
	switch direction {
	case 'N':
		pos.y++
	case 'S':
		pos.y--
	case 'E':
		pos.x++
	case 'W':
		pos.x--
	}
	return pos, distance + 1
}

func updateDistance(distances map[Position]int, pos Position, distance int) {
	if d, exists := distances[pos]; !exists || distance < d {
		distances[pos] = distance
	}
}
