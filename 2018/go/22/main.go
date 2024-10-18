package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

// Define constants for region types and tools
const (
	rocky  = 0
	wet    = 1
	narrow = 2

	torch   = 0
	gear    = 1
	neither = 2
)

var (
	validItems = map[int][]int{
		rocky:  {torch, gear},
		wet:    {gear, neither},
		narrow: {torch, neither},
	}

	validRegions = map[int][]int{
		torch:   {rocky, narrow},
		gear:    {rocky, wet},
		neither: {wet, narrow},
	}
)

// PQItem represents a priority queue item
type PQItem struct {
	pos   [2]int // Position (x, y)
	tool  int    // Current tool (torch, gear, or neither)
	time  int    // Current time
	index int    // Index in the heap
}

// PriorityQueue implements a min-heap for PQItem
type PriorityQueue []*PQItem

func (pq PriorityQueue) Len() int           { return len(pq) }
func (pq PriorityQueue) Less(i, j int) bool { return pq[i].time < pq[j].time }
func (pq PriorityQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].index = i
	pq[j].index = j
}
func (pq *PriorityQueue) Push(x interface{}) {
	item := x.(*PQItem)
	item.index = len(*pq)
	*pq = append(*pq, item)
}
func (pq *PriorityQueue) Pop() interface{} {
	old := *pq
	n := len(old)
	item := old[n-1]
	item.index = -1 // for safety
	*pq = old[0 : n-1]
	return item
}

// ReadCaveInput reads the input file and extracts depth and target coordinates
func ReadCaveInput(file string) (int, [2]int) {
	f, err := os.Open(file)
	if err != nil {
		fmt.Println("Error opening file:", err)
		os.Exit(1)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	scanner.Scan()
	depth, _ := strconv.Atoi(scanner.Text()[len("depth: "):])
	scanner.Scan()
	targetStr := strings.Split(scanner.Text()[len("target: "):], ",")
	targetX, _ := strconv.Atoi(targetStr[0])
	targetY, _ := strconv.Atoi(targetStr[1])
	target := [2]int{targetX, targetY}

	return depth, target
}

// GenerateGrid creates a grid of regions with geological and erosion levels
func GenerateGrid(depth int, corner, target [2]int) map[[2]int][3]int {
	grid := make(map[[2]int][3]int)

	for y := 0; y <= corner[1]; y++ {
		for x := 0; x <= corner[0]; x++ {
			geo := calculateGeologicalIndex(x, y, target, grid)
			ero := (geo + depth) % 20183
			risk := ero % 3
			grid[[2]int{x, y}] = [3]int{geo, ero, risk}
		}
	}
	return grid
}

// calculateGeologicalIndex computes the geological index for the given position
func calculateGeologicalIndex(x, y int, target [2]int, grid map[[2]int][3]int) int {
	if (x == 0 && y == 0) || (x == target[0] && y == target[1]) {
		return 0
	} else if x == 0 {
		return y * 48271
	} else if y == 0 {
		return x * 16807
	}
	return grid[[2]int{x - 1, y}][1] * grid[[2]int{x, y - 1}][1]
}

// Dijkstra finds the shortest time to reach the target
func Dijkstra(grid map[[2]int]int, corner, target [2]int) int {
	pq := make(PriorityQueue, 0)
	heap.Init(&pq)
	heap.Push(&pq, &PQItem{pos: [2]int{0, 0}, tool: torch, time: 0})

	visited := make(map[[3]int]bool)
	dist := make(map[[3]int]int)
	dist[[3]int{0, 0, torch}] = 0

	for pq.Len() > 0 {
		curr := heap.Pop(&pq).(*PQItem)

		if visited[[3]int{curr.pos[0], curr.pos[1], curr.tool}] {
			continue
		}
		visited[[3]int{curr.pos[0], curr.pos[1], curr.tool}] = true

		if curr.pos == target && curr.tool == torch {
			return curr.time
		}

		exploreNeighbors(curr, &pq, dist, grid, corner)
		switchTools(curr, &pq, dist, grid)
	}

	return math.MaxInt32 // No path found
}

// exploreNeighbors explores valid neighboring positions
func exploreNeighbors(curr *PQItem, pq *PriorityQueue, dist map[[3]int]int, grid map[[2]int]int, corner [2]int) {
	directions := [][2]int{
		{0, 1},  // Down
		{0, -1}, // Up
		{1, 0},  // Right
		{-1, 0}, // Left
	}

	for _, dir := range directions {
		newPos := [2]int{curr.pos[0] + dir[0], curr.pos[1] + dir[1]}

		if newPos[0] < 0 || newPos[1] < 0 || newPos[0] > corner[0] || newPos[1] > corner[1] {
			continue
		}

		region := grid[newPos]
		if !contains(validRegions[curr.tool], region) {
			continue
		}

		newState := [3]int{newPos[0], newPos[1], curr.tool}
		newTime := curr.time + 1
		if dist[newState] == 0 || newTime < dist[newState] {
			dist[newState] = newTime
			heap.Push(pq, &PQItem{pos: newPos, tool: curr.tool, time: newTime})
		}
	}
}

// switchTools attempts to switch tools at the current position
func switchTools(curr *PQItem, pq *PriorityQueue, dist map[[3]int]int, grid map[[2]int]int) {
	for _, newTool := range validItems[grid[curr.pos]] {
		if newTool != curr.tool {
			newState := [3]int{curr.pos[0], curr.pos[1], newTool}
			newTime := curr.time + 7
			if dist[newState] == 0 || newTime < dist[newState] {
				dist[newState] = newTime
				heap.Push(pq, &PQItem{pos: curr.pos, tool: newTool, time: newTime})
			}
		}
	}
}

// contains checks if a value is present in an array
func contains(arr []int, val int) bool {
	for _, a := range arr {
		if a == val {
			return true
		}
	}
	return false
}

// main function to execute the program
func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		os.Exit(1)
	}

	file := os.Args[1]
	depth, target := ReadCaveInput(file)

	grid := GenerateGrid(depth, target, target)
	sumRisk := calculateTotalRisk(grid)
	fmt.Println("Part 1:", sumRisk)

	corner := [2]int{target[0] + 100, target[1] + 100}
	grid2 := GenerateGrid(depth, corner, target)
	riskGrid := make(map[[2]int]int)
	for coord, value := range grid2 {
		riskGrid[coord] = value[2]
	}
	fmt.Println("Part 2:", Dijkstra(riskGrid, corner, target))
}

// calculateTotalRisk computes the total risk level of the grid
func calculateTotalRisk(grid map[[2]int][3]int) int {
	sumRisk := 0
	for _, v := range grid {
		sumRisk += v[2]
	}
	return sumRisk
}
