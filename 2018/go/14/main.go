package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		return
	}
	filename := os.Args[1]
	if _, err := os.Stat(filename); os.IsNotExist(err) {
		fmt.Println("File not found:", filename)
		os.Exit(1)
	}
	input, err := os.ReadFile(filename)
	if err != nil {
		fmt.Println("Error reading file:", err)
		os.Exit(1)
	}

	inputStr := strings.TrimSpace(string(input))

	// Part 1
	numRecipes, err := strconv.Atoi(inputStr)
	if err != nil {
		fmt.Println("Invalid input for Part 1. Expected a number.")
		os.Exit(1)
	}

	scores := generateRecipes(numRecipes + 10)
	part1 := getScoresAfter(scores, numRecipes)
	fmt.Println("Part 1:", part1)

	// Part 2
	part2 := findSequence(inputStr)
	fmt.Println("Part 2:", part2)
}

func generateRecipes(target int) []int {
	scores := []int{3, 7}
	elf1, elf2 := 0, 1

	for len(scores) < target {
		sum := scores[elf1] + scores[elf2]
		if sum >= 10 {
			scores = append(scores, sum/10)
		}
		scores = append(scores, sum%10)

		elf1 = (elf1 + 1 + scores[elf1]) % len(scores)
		elf2 = (elf2 + 1 + scores[elf2]) % len(scores)
	}

	return scores
}

func getScoresAfter(scores []int, start int) string {
	result := ""
	for i := start; i < start+10; i++ {
		result += strconv.Itoa(scores[i])
	}
	return result
}

func findSequence(target string) int {
	scores := []int{3, 7}
	elf1, elf2 := 0, 1
	targetLen := len(target)
	targetInts := make([]int, targetLen)
	for i, char := range target {
		targetInts[i] = int(char - '0')
	}

	for {
		sum := scores[elf1] + scores[elf2]
		if sum >= 10 {
			scores = append(scores, sum/10)
			if checkSequence(scores, targetInts) {
				return len(scores) - targetLen
			}
		}
		scores = append(scores, sum%10)
		if checkSequence(scores, targetInts) {
			return len(scores) - targetLen
		}

		elf1 = (elf1 + 1 + scores[elf1]) % len(scores)
		elf2 = (elf2 + 1 + scores[elf2]) % len(scores)
	}
}

func checkSequence(scores []int, target []int) bool {
	if len(scores) < len(target) {
		return false
	}
	start := len(scores) - len(target)
	for i, v := range target {
		if scores[start+i] != v {
			return false
		}
	}
	return true
}
