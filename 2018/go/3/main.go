package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

type Claim struct {
	ID     int
	Left   int
	Top    int
	Width  int
	Height int
}

const fabricSize = 1000

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		return
	}

	file, err := os.Open(os.Args[1])
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	fabric := make([]int, fabricSize*fabricSize)
	re := regexp.MustCompile(`#(\d+) @ (\d+),(\d+): (\d+)x(\d+)`)
	var claims []Claim

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		matches := re.FindStringSubmatch(line)
		if len(matches) != 6 {
			continue
		}

		id, left, top, width, height := atoi(matches[1]), atoi(matches[2]), atoi(matches[3]), atoi(matches[4]), atoi(matches[5])
		claims = append(claims, Claim{id, left, top, width, height})

		for i := top; i < top+height; i++ {
			for j := left; j < left+width; j++ {
				fabric[i*fabricSize+j]++
			}
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	// Part 1: Count overlapping square inches
	overlapCount := 0
	for _, v := range fabric {
		if v >= 2 {
			overlapCount++
		}
	}
	fmt.Println("Part 1:", overlapCount)

	// Part 2: Find the ID of the only non-overlapping claim
	for _, claim := range claims {
		overlaps := false
		for i := claim.Top; i < claim.Top+claim.Height; i++ {
			for j := claim.Left; j < claim.Left+claim.Width; j++ {
				if fabric[i*fabricSize+j] > 1 {
					overlaps = true
					break
				}
			}
			if overlaps {
				break
			}
		}
		if !overlaps {
			fmt.Println("Part 2:", claim.ID)
			break
		}
	}
}

// Helper function to convert string to int
func atoi(s string) int {
	n, _ := strconv.Atoi(s)
	return n
}
