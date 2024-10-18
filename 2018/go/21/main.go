package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

type Op int

const (
	addr Op = iota
	addi
	mulr
	muli
	banr
	bani
	borr
	bori
	setr
	seti
	gtir
	gtri
	gtrr
	eqir
	eqri
	eqrr
)

type Instruction struct {
	op   Op
	data [3]int64
}

var opMap = map[string]Op{
	"addr": addr, "addi": addi, "mulr": mulr, "muli": muli,
	"banr": banr, "bani": bani, "borr": borr, "bori": bori,
	"setr": setr, "seti": seti, "gtir": gtir, "gtri": gtri,
	"gtrr": gtrr, "eqir": eqir, "eqri": eqri, "eqrr": eqrr,
}

func parseInstruction(line string) Instruction {
	var opName string
	var data [3]int64
	fmt.Sscanf(line, "%s %d %d %d", &opName, &data[0], &data[1], &data[2])
	return Instruction{op: opMap[opName], data: data}
}

func applyOp(registers *[6]int64, instruction *Instruction) {
	input := &instruction.data
	switch instruction.op {
	case addr:
		registers[input[2]] = registers[input[0]] + registers[input[1]]
	case addi:
		registers[input[2]] = registers[input[0]] + input[1]
	case mulr:
		registers[input[2]] = registers[input[0]] * registers[input[1]]
	case muli:
		registers[input[2]] = registers[input[0]] * input[1]
	case banr:
		registers[input[2]] = registers[input[0]] & registers[input[1]]
	case bani:
		registers[input[2]] = registers[input[0]] & input[1]
	case borr:
		registers[input[2]] = registers[input[0]] | registers[input[1]]
	case bori:
		registers[input[2]] = registers[input[0]] | input[1]
	case setr:
		registers[input[2]] = registers[input[0]]
	case seti:
		registers[input[2]] = input[0]
	case gtir:
		if input[0] > registers[input[1]] {
			registers[input[2]] = 1
		} else {
			registers[input[2]] = 0
		}
	case gtri:
		if registers[input[0]] > input[1] {
			registers[input[2]] = 1
		} else {
			registers[input[2]] = 0
		}
	case gtrr:
		if registers[input[0]] > registers[input[1]] {
			registers[input[2]] = 1
		} else {
			registers[input[2]] = 0
		}
	case eqir:
		if input[0] == registers[input[1]] {
			registers[input[2]] = 1
		} else {
			registers[input[2]] = 0
		}
	case eqri:
		if registers[input[0]] == input[1] {
			registers[input[2]] = 1
		} else {
			registers[input[2]] = 0
		}
	case eqrr:
		if registers[input[0]] == registers[input[1]] {
			registers[input[2]] = 1
		} else {
			registers[input[2]] = 0
		}
	}
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		os.Exit(1)
	}

	file, err := os.Open(os.Args[1])
	if err != nil {
		fmt.Println("Error opening file:", err)
		os.Exit(1)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	// Read IP register
	scanner.Scan()
	line := scanner.Text()
	ip, _ := strconv.ParseInt(line[4:], 10, 64)

	// Read instructions
	instructions := make([]Instruction, 0, 64)
	for scanner.Scan() {
		instructions = append(instructions, parseInstruction(scanner.Text()))
	}

	stoppingValues := make(map[int64]struct{})
	var lastStop int64

	registers := [6]int64{}

	for {
		applyOp(&registers, &instructions[registers[ip]])
		registers[ip]++
		// Check if we've hit instruction 28 to capture the value of register 5
		if registers[ip] == 28 {
			if _, exists := stoppingValues[registers[5]]; !exists {
				if len(stoppingValues) == 0 {
					fmt.Println("Part 1:", registers[5])
				}
				stoppingValues[registers[5]] = struct{}{}
				lastStop = registers[5]
			} else {
				break
			}
		}
	}

	fmt.Println("Part 2:", lastStop)
}
