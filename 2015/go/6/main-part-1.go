package main

import (
	"bufio"
	"fmt"
	"os"
)

func get_action(line string, action *string, startx *int, starty *int, endx *int, endy *int) {
	if line[0:2] == "tu" {
		fmt.Sscanf(line, "turn %s %d,%d through %d,%d", action, startx, starty, endx, endy)
	} else {
		fmt.Sscanf(line, "%s %d,%d through %d,%d", action, startx, starty, endx, endy)
	}
}

func main() {
	args := os.Args[1:]

	if len(args) < 1 {
		fmt.Println("Usage: go run main.go <input.txt>")
		return
	}

	var filename string = args[0]

	file, err := os.Open(filename)

	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}

	defer file.Close()

	var lights [1000 * 1000]bool

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()

		var action string
		var startx, starty, endx, endy int
		get_action(line, &action, &startx, &starty, &endx, &endy)

		for x := startx; x <= endx; x++ {
			for y := starty; y <= endy; y++ {
				index := x*1000 + y
				if action == "on" {
					lights[index] = true
				} else if action == "off" {
					lights[index] = false
				} else if action == "toggle" {
					lights[index] = !lights[index]
				}
			}
		}
	}

	// Count the number of lights that are on
	count := 0
	for i := 0; i < 1000*1000; i++ {
		if lights[i] {
			count++
		}
	}

	fmt.Println("Part 1:", count)

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
	}
}
