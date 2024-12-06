package main

import (
	"fmt"
	"os"
	"regexp"
	"strings"
)

// Simulation holds the state and behavior for the plant growth simulation.
type Simulation struct {
	pots        map[int]string
	mapping     map[string]string
	seen        map[string]int
	generations int
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

	re := regexp.MustCompile(`[.#]+`)
	matches := re.FindAllString(string(data), -1)
	initial := matches[0]
	mapping := make(map[string]string)
	for i := 1; i < len(matches); i += 2 {
		mapping[matches[i]] = matches[i+1]
	}
	pots := initPots(initial)
	sim := Simulation{
		pots:        pots,
		mapping:     mapping,
		seen:        make(map[string]int),
		generations: 1000,
	}
	sim.Run()
}

// Run executes the plant growth simulation.
func (s *Simulation) Run() {
	var previousSum int
	var delta int

	for n := 1; n <= s.generations; n++ {
		s.pots = s.evolvePots()

		if n == 20 {
			fmt.Println("Part 1:", s.sumPots())
		}

		currentSum := s.sumPots()

		// Detect repeating pattern by checking if we've seen this pattern before
		pattern := s.getPattern()
		if _, found := s.seen[pattern]; found {
			delta = currentSum - previousSum
			remainingGens := 50000000000 - n
			finalSum := currentSum + remainingGens*delta
			fmt.Println("Part 2:", finalSum)
			break
		}

		// Store current state for cycle detection
		s.seen[pattern] = n
		previousSum = currentSum
	}
}

// initPots initializes the pots map with the starting configuration.
func initPots(initial string) map[int]string {
	pots := make(map[int]string)
	for i, c := range initial {
		pots[i] = string(c)
	}
	return pots
}

// evolvePots creates the next generation of pots based on the current state.
func (s *Simulation) evolvePots() map[int]string {
	newPots := make(map[int]string)
	for i := minKey(s.pots) - 2; i <= maxKey(s.pots)+2; i++ {
		window := s.buildWindow(i)
		newPots[i] = s.mapping[window]
		if newPots[i] == "" {
			newPots[i] = "."
		}
	}
	return newPots
}

// buildWindow constructs the string for a given pot's neighborhood.
func (s *Simulation) buildWindow(i int) string {
	var window strings.Builder
	for j := -2; j <= 2; j++ {
		if val, ok := s.pots[i+j]; ok {
			window.WriteString(val)
		} else {
			window.WriteString(".")
		}
	}
	return window.String()
}

// sumPots calculates the sum of the pot indices that contain a plant ('#').
func (s *Simulation) sumPots() int {
	sum := 0
	for i, v := range s.pots {
		if v == "#" {
			sum += i
		}
	}
	return sum
}

// getPattern retrieves the string representation of the current pots.
func (s *Simulation) getPattern() string {
	var pattern strings.Builder
	for i := minKey(s.pots); i <= maxKey(s.pots); i++ {
		if val, ok := s.pots[i]; ok {
			pattern.WriteString(val)
		} else {
			pattern.WriteString(".")
		}
	}
	return strings.Trim(pattern.String(), ".")
}

// minKey and maxKey helpers find the minimum and maximum keys in the pots map.
func minKey(m map[int]string) int {
	min := int(^uint(0) >> 1)
	for k := range m {
		if k < min {
			min = k
		}
	}
	return min
}

func maxKey(m map[int]string) int {
	max := -int(^uint(0)>>1) - 1
	for k := range m {
		if k > max {
			max = k
		}
	}
	return max
}
