package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	// Check that an input file is provided
	args := os.Args[1:]
	if len(args) < 1 {
		fmt.Println("Usage: go run main.go <input_file>")
		return
	}

	// Open the input file
	file, err := os.Open(args[0])
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	twoCount := 0
	threeCount := 0
	var boxIDs []string

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		id := scanner.Text()
		boxIDs = append(boxIDs, id)

		counts := make(map[rune]int)

		// Count the occurrences of each letter
		for _, letter := range id {
			counts[letter]++
		}

		// Check if the counts match exactly two or three
		hasTwo := false
		hasThree := false
		for _, count := range counts {
			if count == 2 {
				hasTwo = true
			} else if count == 3 {
				hasThree = true
			}
		}

		if hasTwo {
			twoCount++
		}
		if hasThree {
			threeCount++
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	// Calculate checksum
	checksum := twoCount * threeCount
	fmt.Println("Part 1:", checksum)

	// Part 2 - Finding the two box IDs that differ by exactly one character
	var commonLetters string
outer:
	for i := 0; i < len(boxIDs); i++ {
		for j := i + 1; j < len(boxIDs); j++ {
			if differByOne(boxIDs[i], boxIDs[j]) {
				// Found the IDs that differ by exactly one character
				commonLetters = getCommonLetters(boxIDs[i], boxIDs[j])
				break outer
			}
		}
	}

	if commonLetters != "" {
		fmt.Println("Part 2:", commonLetters)
	}
}

// differByOne checks if two strings differ by exactly one character
func differByOne(id1, id2 string) bool {
	if len(id1) != len(id2) {
		return false
	}

	differenceCount := 0
	for i := range id1 {
		if id1[i] != id2[i] {
			differenceCount++
		}
		if differenceCount > 1 {
			return false
		}
	}
	return differenceCount == 1
}

// getCommonLetters returns the common letters between two box IDs
func getCommonLetters(id1, id2 string) string {
	var common []rune
	for i := range id1 {
		if id1[i] == id2[i] {
			common = append(common, rune(id1[i]))
		}
	}
	return string(common)
}
