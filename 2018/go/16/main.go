package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type (
	RegisterState [4]int
	Instruction struct {
		opcode, a, b, c int
	}
	Sample struct {
		before RegisterState
		inst   Instruction
		after  RegisterState
	}
)

var operations = map[int]func(regs RegisterState, a, b int) int{
	0:  func(r RegisterState, a, b int) int { return r[a] + r[b] },    // addr
	1:  func(r RegisterState, a, b int) int { return r[a] + b },       // addi
	2:  func(r RegisterState, a, b int) int { return r[a] * r[b] },    // mulr
	3:  func(r RegisterState, a, b int) int { return r[a] * b },       // muli
	4:  func(r RegisterState, a, b int) int { return r[a] & r[b] },    // banr
	5:  func(r RegisterState, a, b int) int { return r[a] & b },       // bani
	6:  func(r RegisterState, a, b int) int { return r[a] | r[b] },    // borr
	7:  func(r RegisterState, a, b int) int { return r[a] | b },       // bori
	8:  func(r RegisterState, a, b int) int { return r[a] },           // setr
	9:  func(r RegisterState, a, b int) int { return a },              // seti
	10: func(r RegisterState, a, b int) int { return boolToInt(a > r[b]) },    // gtir
	11: func(r RegisterState, a, b int) int { return boolToInt(r[a] > b) },    // gtri
	12: func(r RegisterState, a, b int) int { return boolToInt(r[a] > r[b]) }, // gtrr
	13: func(r RegisterState, a, b int) int { return boolToInt(a == r[b]) },   // eqir
	14: func(r RegisterState, a, b int) int { return boolToInt(r[a] == b) },   // eqri
	15: func(r RegisterState, a, b int) int { return boolToInt(r[a] == r[b]) }, // eqrr
}

func boolToInt(b bool) int {
	if b {
		return 1
	}
	return 0
}

func parseRegisters(line string) (r RegisterState) {
	fmt.Sscanf(strings.Trim(line, "[]"), "%d, %d, %d, %d", &r[0], &r[1], &r[2], &r[3])
	return
}

func parseInstruction(line string) (i Instruction) {
	fmt.Sscanf(line, "%d %d %d %d", &i.opcode, &i.a, &i.b, &i.c)
	return
}

func parseSamples(content []byte) []Sample {
	var samples []Sample
	scanner := bufio.NewScanner(strings.NewReader(string(content)))

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		if !strings.HasPrefix(line, "Before:") {
			break
		}

		scanner.Scan()
		sample := Sample{
			before: parseRegisters(strings.TrimPrefix(line, "Before: ")),
			inst:   parseInstruction(scanner.Text()),
		}
		scanner.Scan()
		sample.after = parseRegisters(strings.TrimPrefix(scanner.Text(), "After:  "))
		samples = append(samples, sample)
		scanner.Scan()
	}
	return samples
}

func executeOp(opcode int, regs RegisterState, a, b, c int) RegisterState {
	result := regs
	result[c] = operations[opcode](regs, a, b)
	return result
}

func findOpcodeMappings(samples []Sample) map[int]int {
	possible := make(map[int]map[int]bool)
	for i := 0; i < 16; i++ {
		possible[i] = make(map[int]bool)
		for j := 0; j < 16; j++ {
			possible[i][j] = true
		}
	}

	for _, s := range samples {
		for op := 0; op < 16; op++ {
			if executeOp(op, s.before, s.inst.a, s.inst.b, s.inst.c) != s.after {
				delete(possible[s.inst.opcode], op)
			}
		}
	}

	mapping := make(map[int]int)
	for len(mapping) < 16 {
		for inst := 0; inst < 16; inst++ {
			if len(possible[inst]) == 1 {
				var op int
				for k := range possible[inst] {
					op = k
					break
				}
				mapping[inst] = op
				for other := 0; other < 16; other++ {
					if other != inst {
						delete(possible[other], op)
					}
				}
			}
		}
	}
	return mapping
}

func main() {
	if len(os.Args) < 3 {
		fmt.Println("Usage: go run main.go <input_file_1> <input_file_2>")
		return
	}

	content1, err := os.ReadFile(os.Args[1])
	if err != nil {
		fmt.Printf("Error reading file 1: %v\n", err)
		return
	}

	samples := parseSamples(content1)
	count := 0
	for _, s := range samples {
		matching := 0
		for op := 0; op < 16; op++ {
			if executeOp(op, s.before, s.inst.a, s.inst.b, s.inst.c) == s.after {
				matching++
			}
		}
		if matching >= 3 {
			count++
		}
	}
	fmt.Printf("Part 1: %d\n", count)

	content2, err := os.ReadFile(os.Args[2])
	if err != nil {
		fmt.Printf("Error reading file 2: %v\n", err)
		return
	}

	var program []Instruction
	scanner := bufio.NewScanner(strings.NewReader(string(content2)))
	for scanner.Scan() {
		if line := scanner.Text(); line != "" {
			program = append(program, parseInstruction(line))
		}
	}

	regs := RegisterState{0, 0, 0, 0}
	opcodeMap := findOpcodeMappings(samples)
	for _, inst := range program {
		regs = executeOp(opcodeMap[inst.opcode], regs, inst.a, inst.b, inst.c)
	}
	fmt.Printf("Part 2: %d\n", regs[0])
}
