package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
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

	// Initialize the starting frequency for Part 1
	frequency := 0

	// Read the file line by line to store frequency changes
	var changes []int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		// Parse the line to an integer
		change, err := strconv.Atoi(scanner.Text())
		if err != nil {
			fmt.Println("Error parsing line:", err)
			return
		}
		// Store the change for later use in Part 2
		changes = append(changes, change)
		// Apply the change to the frequency for Part 1
		frequency += change
	}

	// Check for any errors in reading the file
	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	// Print the resulting frequency for Part 1
	fmt.Println("Part 1:", frequency)

	// Part 2: Find the first frequency reached twice
	frequency = 0
	seen := make(map[int]bool)
	seen[frequency] = true // Start with the initial frequency

	for {
		for _, change := range changes {
			frequency += change
			if seen[frequency] {
				fmt.Println("Part 2:", frequency)
				return
			}
			seen[frequency] = true
		}
	}
}
