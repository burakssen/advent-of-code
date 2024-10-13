package main

import (
	"bytes"
	"fmt"
	"math"
	"os"
)

type Position struct{ x, y int }
type Velocity struct{ x, y int }
type Point struct {
	position Position
	velocity Velocity
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

	var points []Point
	for _, line := range bytes.Split(data, []byte("\n")) {
		if len(line) > 0 {
			var p Position
			var v Velocity
			fmt.Sscanf(string(line), "position=<%d, %d> velocity=<%d, %d>", &p.x, &p.y, &v.x, &v.y)
			points = append(points, Point{p, v})
		}
	}

	for second := 0; ; second++ {
		minX, maxX, minY, maxY := getBounds(points)
		if maxX-minX+1 <= 100 && maxY-minY+1 <= 10 {
			fmt.Println("Part 1:")
			printMessage(points, minX, maxX, minY, maxY)
			fmt.Println("Part 2:", second)
			break
		}
		for i := range points {
			points[i].position.x += points[i].velocity.x
			points[i].position.y += points[i].velocity.y
		}
	}
}

func getBounds(points []Point) (minX, maxX, minY, maxY int) {
	minX, minY = math.MaxInt32, math.MaxInt32
	maxX, maxY = math.MinInt32, math.MinInt32
	for _, p := range points {
		if p.position.x < minX {
			minX = p.position.x
		}
		if p.position.x > maxX {
			maxX = p.position.x
		}
		if p.position.y < minY {
			minY = p.position.y
		}
		if p.position.y > maxY {
			maxY = p.position.y
		}
	}
	return
}

func printMessage(points []Point, minX, maxX, minY, maxY int) {
	grid := make([][]byte, maxY-minY+1)
	for i := range grid {
		grid[i] = bytes.Repeat([]byte{'.'}, maxX-minX+1)
	}
	for _, p := range points {
		grid[p.position.y-minY][p.position.x-minX] = '#'
	}
	for _, row := range grid {
		fmt.Println(string(row))
	}
}
