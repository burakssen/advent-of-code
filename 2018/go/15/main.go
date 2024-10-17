package main

import (
	"fmt"
	"math"
	"os"
	"sort"
	"strings"
)

type Point struct {
	x, y int
}

type Unit struct {
	position Point
	hp       int
	attack   int
	isElf    bool
}

type Game struct {
	grid   [][]rune
	units  []*Unit
	width  int
	height int
	initialGrid [][]rune
}

func newGame(input string) *Game {
	lines := strings.Split(strings.TrimSpace(input), "\n")
	height, width := len(lines), len(lines[0])
	grid := make([][]rune, height)
	units := make([]*Unit, 0, width*height/10) // Pre-allocate with an estimated capacity

	for y, line := range lines {
		grid[y] = []rune(line)
		for x, ch := range grid[y] {
			if ch == 'E' || ch == 'G' {
				units = append(units, &Unit{Point{x, y}, 200, 3, ch == 'E'})
				grid[y][x] = '.'
			}
		}
	}

	return &Game{grid: grid, units: units, width: width, height: height, initialGrid: grid}
}

func (g *Game) round() bool {
	sort.Slice(g.units, func(i, j int) bool {
		return g.units[i].position.y*g.width+g.units[i].position.x < g.units[j].position.y*g.width+g.units[j].position.x
	})

	for i := 0; i < len(g.units); i++ {
		if g.units[i].hp <= 0 {
			continue
		}
		if !g.unitTurn(g.units[i]) || !g.hasTargets(g.units[i]) {
			return false
		}
	}

	g.units = filter(g.units, func(u *Unit) bool { return u.hp > 0 })
	return true
}

func (g *Game) hasTargets(unit *Unit) bool {
	for _, u := range g.units {
		if u.hp > 0 && u.isElf != unit.isElf {
			return true
		}
	}
	return false
}

func (g *Game) unitTurn(unit *Unit) bool {
	if !g.isAdjacentToTarget(unit) {
		g.moveUnit(unit, g.findTargets(unit))
	}
	if g.isAdjacentToTarget(unit) {
		g.attack(unit)
	}
	return true
}


func (g *Game) findTargets(unit *Unit) []*Unit {
	targets := make([]*Unit, 0, len(g.units)/2) // Pre-allocate with an estimated capacity
	for _, u := range g.units {
		if u.isElf != unit.isElf && u.hp > 0 {
			targets = append(targets, u)
		}
	}
	return targets
}

func (g *Game) isAdjacentToTarget(unit *Unit) bool {
	for _, u := range g.units {
		if u.isElf != unit.isElf && u.hp > 0 && adjacent(unit.position, u.position) {
			return true
		}
	}
	return false
}

func (g *Game) moveUnit(unit *Unit, targets []*Unit) {
	inRange := make(map[Point]bool)
	for _, t := range targets {
		for _, p := range adjacentPoints(t.position) {
			if g.grid[p.y][p.x] == '.' && !g.isOccupied(p) {
				inRange[p] = true
			}
		}
	}

	if len(inRange) == 0 {
		return
	}

	start := unit.position
	queue := []Point{start}
	visited := make(map[Point]bool)
	parent := make(map[Point]Point)
	distance := make(map[Point]int)
	visited[start] = true
	distance[start] = 0

	var targetPoint Point
	targetDistance := math.MaxInt32

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		if inRange[current] && (distance[current] < targetDistance || (distance[current] == targetDistance && isReadingOrderFirst(current, targetPoint))) {
			targetPoint, targetDistance = current, distance[current]
			continue
		}

		for _, next := range adjacentPoints(current) {
			if g.grid[next.y][next.x] == '.' && !g.isOccupied(next) && !visited[next] {
				visited[next], parent[next], distance[next] = true, current, distance[current]+1
				queue = append(queue, next)
			}
		}
	}

	if targetDistance != math.MaxInt32 {
		for targetPoint != start {
			if parent[targetPoint] == start {
				unit.position = targetPoint
				break
			}
			targetPoint = parent[targetPoint]
		}
	}
}

func (g *Game) attack(unit *Unit) {
	var target *Unit
	for _, u := range g.units {
		if u.isElf != unit.isElf && u.hp > 0 && adjacent(unit.position, u.position) {
			if target == nil || u.hp < target.hp || (u.hp == target.hp && isReadingOrderFirst(u.position, target.position)) {
				target = u
			}
		}
	}

	if target != nil {
		target.hp -= unit.attack
	}
}

func (g *Game) isOccupied(p Point) bool {
	for _, u := range g.units {
		if u.hp > 0 && u.position == p {
			return true
		}
	}
	return false
}

func adjacent(a, b Point) bool {
	return (a.x == b.x && math.Abs(float64(a.y-b.y)) == 1) || (a.y == b.y && math.Abs(float64(a.x-b.x)) == 1)
}

func adjacentPoints(p Point) [4]Point {
	return [4]Point{{p.x, p.y - 1}, {p.x - 1, p.y}, {p.x + 1, p.y}, {p.x, p.y + 1}}
}

func filter(units []*Unit, condition func(*Unit) bool) []*Unit {
	alive := make([]*Unit, 0, len(units))
	for _, u := range units {
		if condition(u) {
			alive = append(alive, u)
		}
	}
	return alive
}

func isReadingOrderFirst(a, b Point) bool {
	return a.y < b.y || (a.y == b.y && a.x < b.x)
}

func (g *Game) clone() *Game {
	newGrid := make([][]rune, len(g.grid))
	for i := range g.grid {
		newGrid[i] = make([]rune, len(g.grid[i]))
		copy(newGrid[i], g.grid[i])
	}

	newUnits := make([]*Unit, len(g.units))
	for i, u := range g.units {
		newUnits[i] = &Unit{u.position, u.hp, u.attack, u.isElf}
	}

	return &Game{grid: newGrid, units: newUnits, width: g.width, height: g.height}
}

func simulateCombat(game *Game) (int, int) {
	rounds := 0
	for game.round() {
		rounds++
	}

	totalHP := 0
	for _, unit := range game.units {
		if unit.hp > 0 {
			totalHP += unit.hp
		}
	}

	return rounds, totalHP
}

func simulateCombatWithEarlyExit(game *Game, initialElfCount int) (int, int) {
	rounds, survivingElves := 0, initialElfCount

	for game.round() {
		currentElfCount := 0
		for _, unit := range game.units {
			if unit.isElf && unit.hp > 0 {
				currentElfCount++
			}
		}

		if currentElfCount < survivingElves {
			return 0, 0
		}

		survivingElves = currentElfCount
		rounds++
	}

	totalHP := 0
	for _, unit := range game.units {
		if unit.hp > 0 {
			totalHP += unit.hp
		}
	}

	return rounds, totalHP
}

func part1(game *Game) int {
	rounds, totalHP := simulateCombat(game.clone())
	return rounds * totalHP
}

func part2(initialGame *Game) int {
	low, high := 4, 200
	result := 0

	for low <= high {
		mid := (low + high) / 2
		game := initialGame.clone()
		initialElfCount := 0

		for _, u := range game.units {
			if u.isElf {
				u.attack = mid
				initialElfCount++
			}
		}

		rounds, totalHP := simulateCombatWithEarlyExit(game, initialElfCount)
		finalElfCount := len(filter(game.units, func(u *Unit) bool { return u.isElf && u.hp > 0 }))

		if finalElfCount == initialElfCount { // All elves survived
			result = rounds * totalHP
			high = mid - 1
		} else {
			low = mid + 1
		}
	}

	return result
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		return
	}

	filename := os.Args[1]
	content, err := os.ReadFile(filename)
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	game := newGame(string(content))

	// Part 1
	fmt.Printf("Part 1: %d\n", part1(game))

	// Part 2
	fmt.Printf("Part 2: %d\n", part2(game))
}
