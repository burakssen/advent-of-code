package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
)

type Step struct {
	ID       string
	Duration int
	Deps     map[string]bool
}

func newStep(id string) *Step {
	return &Step{
		ID:       id,
		Duration: 61 + int(id[0]-'A'),
		Deps:     make(map[string]bool),
	}
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		return
	}

	steps := parseInput(os.Args[1])

	fmt.Println("Part 1:", strings.Join(topologicalSort(steps), ""))
	fmt.Println("Part 2:", simulateWork(steps, 5))
}

func parseInput(filename string) map[string]*Step {
	file, err := os.Open(filename)
	if err != nil {
		fmt.Println("Error opening file:", err)
		os.Exit(1)
	}
	defer file.Close()

	steps := make(map[string]*Step)
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		before, after := parts[1], parts[7]

		if steps[before] == nil {
			steps[before] = newStep(before)
		}
		if steps[after] == nil {
			steps[after] = newStep(after)
		}
		steps[after].Deps[before] = true
	}
	return steps
}

func topologicalSort(steps map[string]*Step) []string {
	var order []string
	inDegree := make(map[string]int)
	queue := make([]string, 0)

	for _, step := range steps {
		for range step.Deps {
			inDegree[step.ID]++
		}
	}

	for id := range steps {
		if inDegree[id] == 0 {
			queue = append(queue, id)
		}
	}

	for len(queue) > 0 {
		sort.Strings(queue)
		id := queue[0]
		queue = queue[1:]
		order = append(order, id)

		for adjID := range steps {
			if steps[adjID].Deps[id] {
				inDegree[adjID]--
				if inDegree[adjID] == 0 {
					queue = append(queue, adjID)
				}
			}
		}
	}

	return order
}

func simulateWork(steps map[string]*Step, workers int) int {
	inProgress := make(map[string]int)
	completed := make(map[string]int)
	var time int

	for len(completed) < len(steps) {
		for id, remaining := range inProgress {
			if remaining == 0 {
				completed[id] = 1
				delete(inProgress, id)
			}
		}

		available := getAvailableSteps(steps, completed, inProgress)
		for _, id := range available {
			if len(inProgress) < workers {
				inProgress[id] = steps[id].Duration
			}
		}

		if len(inProgress) > 0 {
			time++
			for id := range inProgress {
				inProgress[id]--
			}
		}
	}

	return time
}

func getAvailableSteps(steps map[string]*Step, completed, inProgress map[string]int) []string {
	var available []string

	for id, step := range steps {
		if completed[id] > 0 || inProgress[id] > 0 {
			continue
		}

		allDepsCompleted := true
		for dep := range step.Deps {
			if _, ok := completed[dep]; !ok {
				allDepsCompleted = false
				break
			}
		}

		if allDepsCompleted {
			available = append(available, id)
		}
	}

	sort.Strings(available)
	return available
}
