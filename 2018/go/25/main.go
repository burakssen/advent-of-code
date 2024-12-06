package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Point4D struct {
	x, y, z, w int
}

// Manhattan distance between two 4D points
func manhattanDistance(p1, p2 Point4D) int {
	return abs(p1.x-p2.x) + abs(p1.y-p2.y) + abs(p1.z-p2.z) + abs(p1.w-p2.w)
}

// Absolute value of an integer
func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

// Union-Find structure
type UnionFind struct {
	parent, rank []int
}

// Initialize a new Union-Find structure
func newUnionFind(size int) *UnionFind {
	uf := &UnionFind{
		parent: make([]int, size),
		rank:   make([]int, size),
	}
	for i := range uf.parent {
		uf.parent[i] = i
	}
	return uf
}

// Find with path compression
func (uf *UnionFind) find(x int) int {
	if uf.parent[x] != x {
		uf.parent[x] = uf.find(uf.parent[x])
	}
	return uf.parent[x]
}

// Union by rank
func (uf *UnionFind) union(x, y int) {
	rootX, rootY := uf.find(x), uf.find(y)
	if rootX != rootY {
		switch {
		case uf.rank[rootX] > uf.rank[rootY]:
			uf.parent[rootY] = rootX
		case uf.rank[rootX] < uf.rank[rootY]:
			uf.parent[rootX] = rootY
		default:
			uf.parent[rootY] = rootX
			uf.rank[rootX]++
		}
	}
}

func parseInput(lines []string) ([]Point4D, error) {
	points := make([]Point4D, len(lines))
	for i, line := range lines {
		coords := strings.Split(line, ",")
		if len(coords) != 4 {
			return nil, fmt.Errorf("invalid input format at line %d", i+1)
		}
		x, err1 := strconv.Atoi(coords[0])
		y, err2 := strconv.Atoi(coords[1])
		z, err3 := strconv.Atoi(coords[2])
		w, err4 := strconv.Atoi(coords[3])
		if err := checkErrors(err1, err2, err3, err4); err != nil {
			return nil, err
		}
		points[i] = Point4D{x, y, z, w}
	}
	return points, nil
}

// Check multiple errors and return the first one encountered
func checkErrors(errors ...error) error {
	for _, err := range errors {
		if err != nil {
			return err
		}
	}
	return nil
}

func countConstellations(points []Point4D) int {
	uf := newUnionFind(len(points))
	for i := 0; i < len(points); i++ {
		for j := i + 1; j < len(points); j++ {
			if manhattanDistance(points[i], points[j]) <= 3 {
				uf.union(i, j)
			}
		}
	}
	rootSet := make(map[int]struct{})
	for i := range points {
		rootSet[uf.find(i)] = struct{}{}
	}
	return len(rootSet)
}

func main() {
	if len(os.Args) != 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		return
	}

	content, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	lines := strings.Split(strings.TrimSpace(string(content)), "\n")
	points, err := parseInput(lines)
	if err != nil {
		fmt.Println("Error parsing input:", err)
		return
	}

	fmt.Println("Part 1:", countConstellations(points))
}
