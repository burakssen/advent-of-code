import { PathLike } from "fs";
import { existsSync, readFileSync } from "fs";
import { isNumberObject } from "util/types";

enum InstructionType {
  AND = "AND",
  OR = "OR",
  LSHIFT = "LSHIFT",
  RSHIFT = "RSHIFT",
  NOT = "NOT",
  ASSIGN = "ASSIGN",
}

interface Instruction {
  type: InstructionType;
  input1: string;
  input2: string | null;
  output: string;
}

var instructions: Instruction[] = [];
var wires: Map<string, number> = new Map<string, number>();

function evaluate(target: string): number {
  if (wires.has(target)) {
    return wires.get(target) as number;
  }

  const instruction: Instruction | undefined = instructions.find(
    (instruction) => instruction.output === target
  );

  if (instruction === undefined) {
    return NaN;
  }

  let op1: number = !isNaN(Number(instruction.input1))
    ? Number(instruction.input1)
    : evaluate(instruction.input1);

  let op2: number = 0;
  if (instruction.input2 !== null) {
    op2 = !isNaN(Number(instruction.input2))
      ? Number(instruction.input2)
      : evaluate(instruction.input2);
  }

  let result: number = NaN;
  switch (instruction.type) {
    case InstructionType.ASSIGN:
      result = op1;
      break;
    case InstructionType.NOT:
      result = ~op1;
      break;
    case InstructionType.AND:
      result = op1 & op2;
      break;
    case InstructionType.OR:
      result = op1 | op2;
      break;
    case InstructionType.LSHIFT:
      result = op1 << op2;
      break;
    case InstructionType.RSHIFT:
      result = op1 >> op2;
      break;
  }

  wires.set(target, result);
  return result;
}

function main() {
  // get command line arguments
  const args: string[] = process.argv.slice(1);

  if (args.length < 2) {
    console.log("Usage: node main.js <input.txt>");
    return;
  }

  const filename: string = args[1];

  if (filename === undefined) {
    console.log("Filename is undefined");
    return;
  }

  // read file
  if (!existsSync(filename as PathLike)) {
    console.log("File does not exist");
    return;
  }

  const data: string = readFileSync(filename as PathLike, "utf8");

  const lines: string[] = data.split("\n");

  for (let line of lines) {
    // pattern match line
    const words: string[] = line.split(" ");
    if (words.length === 3) {
      // assign
      const input1: string = words[0];
      const output: string = words[2];
      instructions.push({
        type: InstructionType.ASSIGN,
        input1,
        input2: null,
        output,
      });
    }
    if (words.length === 4) {
      // not
      const input1: string = words[1];
      const output: string = words[3];
      instructions.push({
        type: InstructionType.NOT,
        input1,
        input2: null,
        output,
      });
    }

    if (words.length === 5) {
      const input1: string = words[0];
      const type: InstructionType = words[1] as InstructionType;
      const input2: string = words[2];
      const output: string = words[4];

      instructions.push({
        type,
        input1,
        input2,
        output,
      });
    }
  }

  let a = evaluate("a");
  console.log("Part 1: " + a);
  wires.clear();
  wires.set("b", a);
  a = evaluate("a");
  console.log("Part 2: " + a);
}

main();
