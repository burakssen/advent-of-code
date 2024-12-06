package main

import (
	"fmt"
	"os"
)

const gridSize = 300

func calculatePowerLevel(x, y, serialNumber int) int {
	rackID := x + 10
	power := ((rackID*y + serialNumber) * rackID / 100) % 10
	return power - 5
}

func createPowerGrid(serialNumber int) [][]int {
	grid := make([][]int, gridSize+1)
	for i := range grid {
		grid[i] = make([]int, gridSize+1)
	}

	for y := 1; y <= gridSize; y++ {
		for x := 1; x <= gridSize; x++ {
			grid[y][x] = calculatePowerLevel(x, y, serialNumber) +
				grid[y-1][x] + grid[y][x-1] - grid[y-1][x-1]
		}
	}

	return grid
}

func findLargestPowerSquare3x3(grid [][]int) (int, int) {
	maxPower, maxX, maxY := -1000000, 0, 0
	for y := 1; y <= gridSize-2; y++ {
		for x := 1; x <= gridSize-2; x++ {
			power := grid[y+2][x+2] - grid[y-1][x+2] - grid[y+2][x-1] + grid[y-1][x-1]
			if power > maxPower {
				maxPower, maxX, maxY = power, x, y
			}
		}
	}
	return maxX, maxY
}

func findLargestPowerSquareAnySize(grid [][]int) (int, int, int) {
	maxPower, maxX, maxY, maxSize := -1000000, 0, 0, 0

	for size := 1; size <= gridSize; size++ {
		for y := 1; y <= gridSize-size+1; y++ {
			for x := 1; x <= gridSize-size+1; x++ {
				power := grid[y+size-1][x+size-1] - grid[y-1][x+size-1] - grid[y+size-1][x-1] + grid[y-1][x-1]
				if power > maxPower {
					maxPower, maxX, maxY, maxSize = power, x, y, size
				}
			}
		}
	}

	return maxX, maxY, maxSize
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		return
	}

	data, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Println("Error reading file:", err)
		os.Exit(1)
	}

	var serialNumber int
	fmt.Sscanf(string(data), "%d", &serialNumber)

	grid := createPowerGrid(serialNumber)
	x, y := findLargestPowerSquare3x3(grid)
	fmt.Printf("Part 1: %d,%d\n", x, y)

	x, y, size := findLargestPowerSquareAnySize(grid)
	fmt.Printf("Part 2: %d,%d,%d\n", x, y, size)
}
