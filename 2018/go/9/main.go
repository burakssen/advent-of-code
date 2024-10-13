package main

import (
	"fmt"
	"os"
)

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

	var numOfPlayers, lastMarble int
	fmt.Sscanf(string(data), "%d players; last marble is worth %d points", &numOfPlayers, &lastMarble)

	fmt.Println("Part 1:", playGame(numOfPlayers, lastMarble))
	fmt.Println("Part 2:", playGame(numOfPlayers, lastMarble*100))
}

type Marble struct {
	Value      int
	Prev, Next *Marble
}

func playGame(numOfPlayers, lastMarble int) int {
	scores := make([]int, numOfPlayers)
	current := &Marble{0, nil, nil}
	current.Prev, current.Next = current, current

	for marble := 1; marble <= lastMarble; marble++ {
		if marble%23 == 0 {
			player := marble % numOfPlayers
			scores[player] += marble
			for i := 0; i < 7; i++ {
				current = current.Prev
			}
			scores[player] += current.Value
			current.Prev.Next, current.Next.Prev = current.Next, current.Prev
			current = current.Next
		} else {
			current = current.Next
			m := &Marble{marble, current, current.Next}
			current.Next.Prev, current.Next = m, m
			current = m
		}
	}
	return max(scores)
}

func max(slice []int) int {
	maxValue := slice[0]
	for _, value := range slice[1:] {
		if value > maxValue {
			maxValue = value
		}
	}
	return maxValue
}
