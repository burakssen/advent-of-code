package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Word uint32
type Regs [6]Word

type Instruction struct {
	insn Insn
	args [3]int
}

type Insn int

const (
	addr Insn = iota
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

var instructionMap = map[string]Insn{
	"addr": addr, "addi": addi, "mulr": mulr, "muli": muli,
	"banr": banr, "bani": bani, "borr": borr, "bori": bori,
	"setr": setr, "seti": seti, "gtir": gtir, "gtri": gtri,
	"gtrr": gtrr, "eqir": eqir, "eqri": eqri, "eqrr": eqrr,
}

func parseInsn(s string) (Insn, error) {
	if insn, ok := instructionMap[s]; ok {
		return insn, nil
	}
	return 0, fmt.Errorf("unknown instruction: %s", s)
}

func parseArgs(args []string) [3]int {
	var parsedArgs [3]int
	for i, arg := range args {
		parsedArgs[i], _ = strconv.Atoi(arg)
	}
	return parsedArgs
}

func execute(instr *Instruction, regs *Regs) {
	a, b, c := Word(instr.args[0]), Word(instr.args[1]), instr.args[2]
	switch instr.insn {
	case addr:
		regs[c] = regs[a] + regs[b]
	case addi:
		regs[c] = regs[a] + Word(b)
	case mulr:
		regs[c] = regs[a] * regs[b]
	case muli:
		regs[c] = regs[a] * Word(b)
	case banr:
		regs[c] = regs[a] & regs[b]
	case bani:
		regs[c] = regs[a] & Word(b)
	case borr:
		regs[c] = regs[a] | regs[b]
	case bori:
		regs[c] = regs[a] | Word(b)
	case setr:
		regs[c] = regs[a]
	case seti:
		regs[c] = Word(a)
	case gtir:
		regs[c] = boolToWord(a > regs[b])
	case gtri:
		regs[c] = boolToWord(regs[a] > Word(b))
	case gtrr:
		regs[c] = boolToWord(regs[a] > regs[b])
	case eqir:
		regs[c] = boolToWord(a == regs[b])
	case eqri:
		regs[c] = boolToWord(regs[a] == Word(b))
	case eqrr:
		regs[c] = boolToWord(regs[a] == regs[b])
	}
}

func boolToWord(b bool) Word {
	if b {
		return 1
	}
	return 0
}

func solve(ipr int, opcodes []Instruction, r0 Word) Word {
	regs := Regs{r0}
	for regs[ipr] != 1 {
		execute(&opcodes[regs[ipr]], &regs)
		regs[ipr]++
	}
	return sumDivisors(max(regs[:]...))
}

func max(values ...Word) Word {
	var maxVal Word
	for _, v := range values {
		if v > maxVal {
			maxVal = v
		}
	}
	return maxVal
}

func sumDivisors(n Word) Word {
	var total Word
	for i := Word(1); i <= n; i++ {
		if n%i == 0 {
			total += i
		}
	}
	return total
}

func readInput(filePath string) (string, error) {
	content, err := os.ReadFile(filePath)
	if err != nil {
		return "", fmt.Errorf("error reading file: %w", err)
	}
	return string(content), nil
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: go run main.go <input_file>")
		os.Exit(1)
	}

	content, err := readInput(os.Args[1])
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	lines := strings.Split(strings.TrimSpace(content), "\n")
	ipr, _ := strconv.Atoi(strings.TrimPrefix(lines[0], "#ip "))
	instrs := make([]Instruction, len(lines)-1)

	for i, line := range lines[1:] {
		words := strings.Fields(line)
		insn, _ := parseInsn(words[0])
		instrs[i] = Instruction{insn, parseArgs(words[1:])}
	}

	fmt.Printf("Part 1: %d\n", solve(ipr, instrs, 0))
	fmt.Printf("Part 2: %d\n", solve(ipr, instrs, 1))
}
