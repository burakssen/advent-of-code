package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args[1:]

	if len(args) < 1 {
		fmt.Println("Usage: go run main.go <input.txt>")
		return
	}

	file, err := os.Open(args[0])
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}

	defer file.Close()

	part1_count := 0
	part2_count := 0

	// Read the file
	for {
		var line string
		n, err := fmt.Fscanln(file, &line)
		if n == 0 || err != nil {
			break
		}

		if check_good_string_part1(line) {
			part1_count++
		}

		if check_good_string_part2(line) {
			part2_count++
		}
	}

	fmt.Println("Part 1:", part1_count)
	fmt.Println("Part 2:", part2_count)

}

func check_good_string_part1(s string) bool {
	vowels := 0
	double_letter := false
	bad_string := false

	for i := 0; i < len(s); i++ {
		if s[i] == 'a' || s[i] == 'e' || s[i] == 'i' || s[i] == 'o' || s[i] == 'u' {
			vowels++
		}

		if i < len(s)-1 && s[i] == s[i+1] {
			double_letter = true
		}

		if i < len(s)-1 && (s[i] == 'a' && s[i+1] == 'b' || s[i] == 'c' && s[i+1] == 'd' || s[i] == 'p' && s[i+1] == 'q' || s[i] == 'x' && s[i+1] == 'y') {
			bad_string = true
		}
	}

	if vowels >= 3 && double_letter && !bad_string {
		return true
	}

	return false
}

func check_good_string_part2(s string) bool {
	pair := false
	repeat := false

	for i := 0; i < len(s)-1; i++ {

		if i < len(s)-2 && s[i] == s[i+2] {
			repeat = true
		}

		for j := i + 2; j < len(s)-1; j++ {
			if s[i] == s[j] && s[i+1] == s[j+1] {
				pair = true
			}
		}
	}

	return pair && repeat
}
