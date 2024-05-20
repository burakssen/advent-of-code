#!/usr/bin/env python3

import sys
import os
from enum import Enum

class InstructionType(Enum):
    AND, OR, NOT, LSHIFT, RSHIFT, ASSIGN = range(6)


class Instruction:
    def __init__(self, op, out, in1, in2 = ""):
        self.op = op
        self.out = out
        self.in1 = in1
        self.in2 = in2


instructions = []
hash_table = {}

def eval(target):
    global hash_table
    if hash_table.get(target) is not None:
        return hash_table[target]
    
    instruction = None
    for i in instructions:
        if i.out == target:
            instruction = i
            break

    if instruction is None:
        return int(target)
    
    # ternary operato
    op1 = int(instruction.in1) if instruction.in1.isnumeric() else eval(instruction.in1)
    op2 = 0
    if instruction.in2 != "":
        op2 = int(instruction.in2) if instruction.in2.isnumeric() else eval(instruction.in2)

    result = 0
    if instruction.op == InstructionType.AND:
        result = op1 & op2
    elif instruction.op == InstructionType.OR:
        result = op1 | op2
    elif instruction.op == InstructionType.NOT:
        result = ~op1
    elif instruction.op == InstructionType.LSHIFT:
        result = op1 << op2
    elif instruction.op == InstructionType.RSHIFT:
        result = op1 >> op2
    elif instruction.op == InstructionType.ASSIGN:
        result = op1

    hash_table[target] = result
    return result

def main():
    global instructions
    # get command line arguments
    args = sys.argv[1:]
    if len(args) == 0:
        print("Usage: python3 main.py <input.txt>")
        sys.exit(1)

    filename = args[0]
    if not os.path.exists(filename):
        print(f"Error: file '{filename}' not found")
        sys.exit(1)

    # read file
    lines = []
    with open(filename, 'r') as file:
        lines = file.readlines()

    # process lines
    for line in lines:
        words = line.split()
        if len(words) == 3:
            instructions.append(Instruction(InstructionType.ASSIGN, words[2], words[0]))
        elif len(words) == 4:
            instructions.append(Instruction(InstructionType.NOT, words[3], words[1]))
        elif len(words) == 5:
            if words[1] == "AND":
                instructions.append(Instruction(InstructionType.AND, words[4], words[0], words[2]))
            elif words[1] == "OR":
                instructions.append(Instruction(InstructionType.OR, words[4], words[0], words[2]))
            elif words[1] == "LSHIFT":
                instructions.append(Instruction(InstructionType.LSHIFT, words[4], words[0], words[2]))
            elif words[1] == "RSHIFT":
                instructions.append(Instruction(InstructionType.RSHIFT, words[4], words[0], words[2]))
            else:
                print(f"Error: unknown instruction '{line}'")
                sys.exit(1)
    
    # part 1
    a = eval("a")
    print(f"Part 1: {a}")

    # part 2
    hash_table.clear()
    hash_table["b"] = a
    a = eval("a")
    print(f"Part 2: {a}")
            



if __name__ == '__main__':
    main()