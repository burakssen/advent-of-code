package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type Point struct{ x, y int }

const MaxTotalDistance = 10000

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		return
	}

	coordinates := readCoordinates(os.Args[1])
	if len(coordinates) == 0 {
		fmt.Println("No coordinates found in the input file")
		return
	}

	fmt.Printf("Part 1: %d\n", solvePart1(coordinates))
	fmt.Printf("Part 2: %d\n", solvePart2(coordinates))
}

func solvePart1(coords []Point) int {
	minX, minY, maxX, maxY := getBoundingBox(coords)
	grid, infinite := make(map[Point]int), make(map[int]bool)

	for y := minY; y <= maxY; y++ {
		for x := minX; x <= maxX; x++ {
			if idx, isTie := findClosestCoordinate(x, y, coords); !isTie {
				grid[Point{x, y}] = idx
				if x == minX || x == maxX || y == minY || y == maxY {
					infinite[idx] = true
				}
			}
		}
	}

	areas := make(map[int]int)
	for _, idx := range grid {
		if !infinite[idx] {
			areas[idx]++
		}
	}

	largestArea := 0
	for _, area := range areas {
		if area > largestArea {
			largestArea = area
		}
	}
	return largestArea
}

func solvePart2(coords []Point) int {
	minX, minY, maxX, maxY := getBoundingBox(coords)
	expansion := MaxTotalDistance / len(coords)
	minX, minY, maxX, maxY = minX-expansion, minY-expansion, maxX+expansion, maxY+expansion

	regionSize := 0
	for y := minY; y <= maxY; y++ {
		for x := minX; x <= maxX; x++ {
			totalDist := 0
			for _, p := range coords {
				totalDist += manhattanDistance(x, y, p.x, p.y)
				if totalDist >= MaxTotalDistance {
					break
				}
			}
			if totalDist < MaxTotalDistance {
				regionSize++
			}
		}
	}
	return regionSize
}

func readCoordinates(filename string) []Point {
	file, err := os.Open(filename)
	if err != nil {
		fmt.Println("Error opening file:", err)
		return nil
	}
	defer file.Close()

	var coords []Point
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Split(scanner.Text(), ",")
		x, _ := strconv.Atoi(strings.TrimSpace(parts[0]))
		y, _ := strconv.Atoi(strings.TrimSpace(parts[1]))
		coords = append(coords, Point{x, y})
	}
	return coords
}

func getBoundingBox(coords []Point) (int, int, int, int) {
	minX, minY, maxX, maxY := math.MaxInt32, math.MaxInt32, math.MinInt32, math.MinInt32
	for _, p := range coords {
		if p.x < minX {
			minX = p.x
		}
		if p.y < minY {
			minY = p.y
		}
		if p.x > maxX {
			maxX = p.x
		}
		if p.y > maxY {
			maxY = p.y
		}
	}
	return minX, minY, maxX, maxY
}

func findClosestCoordinate(x, y int, coords []Point) (int, bool) {
	minDist, closestIdx, isTie := math.MaxInt32, -1, false
	for i, p := range coords {
		if dist := manhattanDistance(x, y, p.x, p.y); dist < minDist {
			minDist, closestIdx, isTie = dist, i, false
		} else if dist == minDist {
			isTie = true
		}
	}
	return closestIdx, isTie
}

func manhattanDistance(x1, y1, x2, y2 int) int {
	return abs(x1-x2) + abs(y1-y2)
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}
