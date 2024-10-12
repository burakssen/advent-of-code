package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Node struct {
	children []*Node
	metadata []int
}

func parseNode(nums *[]int) *Node {
	if len(*nums) < 2 {
		return nil
	}
	n := &Node{}
	childCount, metaCount := (*nums)[0], (*nums)[1]
	*nums = (*nums)[2:]

	for i := 0; i < childCount; i++ {
		n.children = append(n.children, parseNode(nums))
	}
	n.metadata = (*nums)[:metaCount]
	*nums = (*nums)[metaCount:]
	return n
}

func sumMetadata(n *Node) int {
	sum := 0
	for _, m := range n.metadata {
		sum += m
	}
	for _, child := range n.children {
		sum += sumMetadata(child)
	}
	return sum
}

func nodeValue(n *Node) int {
	if len(n.children) == 0 {
		return sumMetadata(n)
	}
	value := 0
	for _, m := range n.metadata {
		if m > 0 && m <= len(n.children) {
			value += nodeValue(n.children[m-1])
		}
	}
	return value
}

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

	var nums []int
	for _, s := range strings.Fields(string(data)) {
		n, _ := strconv.Atoi(s)
		nums = append(nums, n)
	}

	root := parseNode(&nums)
	fmt.Printf("Part 1: %d\n", sumMetadata(root))
	fmt.Printf("Part 2: %d\n", nodeValue(root))
}
