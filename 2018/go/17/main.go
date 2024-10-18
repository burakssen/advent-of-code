package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	x, y int
}

type Grid struct {
	clay    map[Point]bool
	settled map[Point]bool
	flowing map[Point]bool
	ymin    int
	ymax    int
}

func NewGrid() *Grid {
	return &Grid{
		clay:    make(map[Point]bool),
		settled: make(map[Point]bool),
		flowing: make(map[Point]bool),
	}
}

func (g *Grid) parseLine(line string) {
	parts := strings.Split(line, ",")
	a := strings.TrimSpace(parts[0])
	brange := strings.TrimSpace(parts[1])

	isXFirst := a[0] == 'x'
	fixed, _ := strconv.Atoi(strings.Split(a, "=")[1])
	rangeStr := strings.Split(strings.Split(brange, "=")[1], "..")
	start, _ := strconv.Atoi(rangeStr[0])
	end, _ := strconv.Atoi(rangeStr[1])

	for i := start; i <= end; i++ {
		if isXFirst {
			g.clay[Point{fixed, i}] = true
		} else {
			g.clay[Point{i, fixed}] = true
		}
	}
}

func (g *Grid) calculateBounds() {
	g.ymin = int(^uint(0) >> 1)
	g.ymax = -g.ymin - 1
	for p := range g.clay {
		if p.y < g.ymin {
			g.ymin = p.y
		}
		if p.y > g.ymax {
			g.ymax = p.y
		}
	}
}

func (g *Grid) fill(pt Point) bool {
	g.flowing[pt] = true
	below := Point{pt.x, pt.y + 1}

	// Check if water can flow down
	if !g.clay[below] && !g.flowing[below] && below.y <= g.ymax {
		g.fill(below)
	}
	if !g.clay[below] && !g.settled[below] {
		return false
	}

	// Check horizontal flow
	left := Point{pt.x - 1, pt.y}
	right := Point{pt.x + 1, pt.y}
	leftFilled := g.clay[left] || (!g.flowing[left] && g.fill(left))
	rightFilled := g.clay[right] || (!g.flowing[right] && g.fill(right))

	// Handle settling water
	if leftFilled && rightFilled {
		g.settled[pt] = true
		g.settleHorizontally(left, right)
	}

	return leftFilled || rightFilled
}

func (g *Grid) settleHorizontally(left, right Point) {
	for g.flowing[left] {
		g.settled[left] = true
		left = Point{left.x - 1, left.y}
	}
	for g.flowing[right] {
		g.settled[right] = true
		right = Point{right.x + 1, right.y}
	}
}

func (g *Grid) countWater() (flowing, settled int) {
	for pt := range g.flowing {
		if g.ymin <= pt.y && pt.y <= g.ymax {
			flowing++
		}
	}
	for pt := range g.settled {
		if g.ymin <= pt.y && pt.y <= g.ymax {
			settled++
		}
	}
	return
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		return
	}

	content, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Printf("Error reading file: %v\n", err)
		return
	}

	grid := NewGrid()
	for _, line := range strings.Split(string(content), "\n") {
		if line != "" {
			grid.parseLine(line)
		}
	}

	grid.calculateBounds()
	grid.fill(Point{500, 0})
	flowing, settled := grid.countWater()

	fmt.Println("part 1:", flowing)
	fmt.Println("part 2:", settled)
}
