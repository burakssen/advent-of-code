package main

import (
	"fmt"
	"os"
	"sort"
	"strings"
)

// Cart represents a cart's position, direction, turn state, and crash status.
type Cart struct {
	x, y      int
	direction byte
	nextTurn  int
	crashed   bool
}

// isCart checks if the character represents a cart.
func isCart(char byte) bool {
	return char == '^' || char == 'v' || char == '<' || char == '>'
}

// trackUnderCart returns the track type under the cart based on its direction.
func trackUnderCart(direction byte) byte {
	if direction == '^' || direction == 'v' {
		return '|'
	}
	return '-'
}

// moveCart updates the cart's position and changes direction based on track.
func moveCart(cart *Cart, grid [][]byte) {
	switch cart.direction {
	case '^':
		cart.y--
	case 'v':
		cart.y++
	case '<':
		cart.x--
	case '>':
		cart.x++
	}

	track := grid[cart.y][cart.x]
	switch track {
	case '/', '\\':
		cart.direction = turn(cart.direction, track)
	case '+':
		cart.direction = turnAtIntersection(cart.direction, cart.nextTurn)
		cart.nextTurn = (cart.nextTurn + 1) % 3
	}
}

// turn adjusts the cart's direction based on the type of curve track.
func turn(direction, track byte) byte {
	switch {
	case track == '/' && (direction == '^' || direction == 'v'):
		return turnRight(direction)
	case track == '/' && (direction == '<' || direction == '>'):
		return turnLeft(direction)
	case track == '\\' && (direction == '^' || direction == 'v'):
		return turnLeft(direction)
	default:
		return turnRight(direction)
	}
}

// turnAtIntersection changes direction at intersections based on next turn type.
func turnAtIntersection(direction byte, turnType int) byte {
	if turnType == 0 {
		return turnLeft(direction)
	} else if turnType == 2 {
		return turnRight(direction)
	}
	return direction
}

// turnLeft and turnRight rotate the cart's direction.
func turnLeft(direction byte) byte {
	return map[byte]byte{'^': '<', 'v': '>', '<': 'v', '>': '^'}[direction]
}

func turnRight(direction byte) byte {
	return map[byte]byte{'^': '>', 'v': '<', '<': '^', '>': 'v'}[direction]
}

// detectCollision checks for collisions with other carts and marks both as crashed.
func detectCollision(carts []*Cart, movedCart *Cart) bool {
	for _, cart := range carts {
		if cart != movedCart && !cart.crashed && cart.x == movedCart.x && cart.y == movedCart.y {
			cart.crashed, movedCart.crashed = true, true
			return true
		}
	}
	return false
}

// sortCarts sorts carts based on their position for ordered movement.
func sortCarts(carts []*Cart) {
	sort.Slice(carts, func(i, j int) bool {
		return carts[i].y < carts[j].y || (carts[i].y == carts[j].y && carts[i].x < carts[j].x)
	})
}

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

	data, err := os.ReadFile(filename)
	if err != nil {
		fmt.Println("Error reading file:", err)
		os.Exit(1)
	}

	lines := strings.Split(string(data), "\n")
	grid, carts := make([][]byte, len(lines)), []*Cart{}
	for y, line := range lines {
		grid[y] = []byte(line)
		for x, char := range line {
			if isCart(byte(char)) {
				carts = append(carts, &Cart{x: x, y: y, direction: byte(char)})
				grid[y][x] = trackUnderCart(byte(char))
			}
		}
	}

	var firstCrash bool

	for {
		sortCarts(carts)
		for _, cart := range carts {
			if cart.crashed {
				continue
			}
			moveCart(cart, grid)
			if detectCollision(carts, cart) && !firstCrash {
				fmt.Printf("Part 1: %d,%d\n", cart.x, cart.y)
				firstCrash = true
			}
		}

		// Filter out crashed carts and check for the last remaining cart
		activeCarts := carts[:0]
		for _, cart := range carts {
			if !cart.crashed {
				activeCarts = append(activeCarts, cart)
			}
		}

		if len(activeCarts) == 1 {
			fmt.Printf("Part 2: %d,%d\n", activeCarts[0].x, activeCarts[0].y)
			break
		}
		carts = activeCarts
	}
}
