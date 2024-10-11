package main

import (
	"bufio"
	"fmt"
	"os"
	"unicode"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		return
	}

	file, err := os.Open(os.Args[1])
	if err != nil {
		fmt.Println("Error opening file:", err)
		os.Exit(1)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	scanner.Scan()
	polymer := scanner.Text()

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		os.Exit(1)
	}

	fmt.Printf("Part 1: %d\n", len(reducePolymer(polymer)))

	minLength := len(polymer)
	for unit := 'a'; unit <= 'z'; unit++ {
		length := len(reducePolymer(removeUnit(polymer, unit)))
		if length < minLength {
			minLength = length
		}
	}
	fmt.Printf("Part 2: %d\n", minLength)
}

func reducePolymer(polymer string) string {
	stack := make([]rune, 0, len(polymer))
	for _, unit := range polymer {
		if len(stack) > 0 && areOpposite(stack[len(stack)-1], unit) {
			stack = stack[:len(stack)-1]
		} else {
			stack = append(stack, unit)
		}
	}
	return string(stack)
}

func areOpposite(a, b rune) bool {
	return unicode.ToLower(a) == unicode.ToLower(b) && a != b
}

func removeUnit(polymer string, unit rune) string {
	result := make([]rune, 0, len(polymer))
	for _, u := range polymer {
		if unicode.ToLower(u) != unit {
			result = append(result, u)
		}
	}
	return string(result)
}
