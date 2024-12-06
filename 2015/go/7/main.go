package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type InstructionType int

const (
	AND InstructionType = iota
	OR
	LSHIFT
	RSHIFT
	NOT
	ASSIGN
)

type Instruction struct {
	operation InstructionType
	input1    string
	input2    string
	output    string
}

var instructions []Instruction
var hashmap map[string]int

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

	instructions = make([]Instruction, 0)
	hashmap = make(map[string]int)

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		InsertInstruction(line)
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	// Part 1
	a_val := Evaluate("a")
	fmt.Println("Part 1:", a_val)

	// Part 2
	hashmap = make(map[string]int)
	hashmap["b"] = a_val
	a_val = Evaluate("a")
	fmt.Println("Part 2:", a_val)
}

func InsertInstruction(line string) {
	var instruction Instruction

	if strings.Contains(line, "AND") {
		instruction.operation = AND
		fmt.Sscanf(line, "%s AND %s -> %s", &instruction.input1, &instruction.input2, &instruction.output)
	} else if strings.Contains(line, "OR") {
		instruction.operation = OR
		fmt.Sscanf(line, "%s OR %s -> %s", &instruction.input1, &instruction.input2, &instruction.output)
	} else if strings.Contains(line, "LSHIFT") {
		instruction.operation = LSHIFT
		fmt.Sscanf(line, "%s LSHIFT %s -> %s", &instruction.input1, &instruction.input2, &instruction.output)
	} else if strings.Contains(line, "RSHIFT") {
		instruction.operation = RSHIFT
		fmt.Sscanf(line, "%s RSHIFT %s -> %s", &instruction.input1, &instruction.input2, &instruction.output)
	} else if strings.Contains(line, "NOT") {
		instruction.operation = NOT
		fmt.Sscanf(line, "NOT %s -> %s", &instruction.input1, &instruction.output)
	} else {
		instruction.operation = ASSIGN
		fmt.Sscanf(line, "%s -> %s", &instruction.input1, &instruction.output)
	}

	instructions = append(instructions, instruction)
}

func Evaluate(find_target string) int {
	// check if the value is already in the hashmap
	if val, ok := hashmap[find_target]; ok {
		return val
	}

	var instruction Instruction

	//find the instruction that has the target as output
	for _, inst := range instructions {
		if inst.output == find_target {
			instruction = inst
			break
		}
	}

	var input1_val, input2_val int = 0, 0

	if instruction.input1 != "" {
		if _, err := fmt.Sscanf(instruction.input1, "%d", &input1_val); err != nil {
			input1_val = Evaluate(instruction.input1)
		}
	}

	if instruction.input2 != "" {
		if _, err := fmt.Sscanf(instruction.input2, "%d", &input2_val); err != nil {
			input2_val = Evaluate(instruction.input2)
		}
	}

	switch instruction.operation {
	case InstructionType(AND):
		hashmap[find_target] = input1_val & input2_val
		break
	case InstructionType(OR):
		hashmap[find_target] = input1_val | input2_val
		break
	case InstructionType(LSHIFT):
		hashmap[find_target] = input1_val << input2_val
		break
	case InstructionType(RSHIFT):
		hashmap[find_target] = input1_val >> input2_val
		break
	case InstructionType(NOT):
		hashmap[find_target] = ^input1_val
		break
	case InstructionType(ASSIGN):
		hashmap[find_target] = input1_val
		break
	}

	return hashmap[find_target]
}
