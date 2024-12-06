package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
)

type Point struct{ x, y, z int }
type NanoBot struct {
	pos Point
	r   int
}

func manhattan(p1, p2 Point) int {
	return int(math.Abs(float64(p1.x-p2.x)) + math.Abs(float64(p1.y-p2.y)) + math.Abs(float64(p1.z-p2.z)))
}



func main() {
	if len(os.Args) != 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		os.Exit(1)
	}

	filename := os.Args[1]
	file, err := os.Open(filename)
	if err != nil {
		fmt.Println("Error opening file:", err)
		os.Exit(1)
	}
	defer file.Close()

	var nanobots []NanoBot
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		var nb NanoBot
		fmt.Sscanf(scanner.Text(), "pos=<%d,%d,%d>, r=%d", &nb.pos.x, &nb.pos.y, &nb.pos.z, &nb.r)
		nanobots = append(nanobots, nb)
	}
	fmt.Printf("Part 1: %d\n", solvePart1(nanobots))
	fmt.Printf("Part 2: %d\n", solvePart2(nanobots))
}

func solvePart1(nanobots []NanoBot) int {
	strongest := nanobots[0]
	for _, nb := range nanobots[1:] {
		if nb.r > strongest.r {
			strongest = nb
		}
	}

	inRange := 0
	for _, nb := range nanobots {
		if manhattan(strongest.pos, nb.pos) <= strongest.r {
			inRange++
		}
	}
	return inRange
}

func solvePart2(nanobots []NanoBot) int {
	minX, minY, minZ := math.MaxInt32, math.MaxInt32, math.MaxInt32
	maxX, maxY, maxZ := math.MinInt32, math.MinInt32, math.MinInt32

	for _, nb := range nanobots {
		minX, maxX = min(minX, nb.pos.x-nb.r), max(maxX, nb.pos.x+nb.r)
		minY, maxY = min(minY, nb.pos.y-nb.r), max(maxY, nb.pos.y+nb.r)
		minZ, maxZ = min(minZ, nb.pos.z-nb.r), max(maxZ, nb.pos.z+nb.r)
	}

	gridSize := max(max(maxX-minX, maxY-minY), maxZ-minZ) / 10
	bestPoint := Point{0, 0, 0}
	bestCount, bestDistance := 0, math.MaxInt32

	for gridSize > 0 {
		for x := minX; x <= maxX; x += gridSize {
			for y := minY; y <= maxY; y += gridSize {
				for z := minZ; z <= maxZ; z += gridSize {
					p := Point{x, y, z}
					count := countInRange(p, nanobots)
					distance := manhattan(Point{0, 0, 0}, p)
					if count > bestCount || (count == bestCount && distance < bestDistance) {
						bestCount, bestDistance, bestPoint = count, distance, p
					}
				}
			}
		}
		minX, maxX = bestPoint.x-gridSize, bestPoint.x+gridSize
		minY, maxY = bestPoint.y-gridSize, bestPoint.y+gridSize
		minZ, maxZ = bestPoint.z-gridSize, bestPoint.z+gridSize
		gridSize /= 2
	}
	return manhattan(Point{0, 0, 0}, bestPoint)
}

func countInRange(p Point, nanobots []NanoBot) int {
	count := 0
	for _, nb := range nanobots {
		if manhattan(p, nb.pos) <= nb.r {
			count++
		}
	}
	return count
}
