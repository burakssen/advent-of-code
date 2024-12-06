import { readFileSync, existsSync } from "fs";

type Instruction = {
  type: "mul" | "do" | "dont";
  index: number;
  value?: [number, number];
};

// Parses content and extracts instructions with type and value
function parseInstructions(content: string): Instruction[] {
  const patterns = [
    { regex: /mul\((\d+),(\d+)\)/g, type: "mul" },
    { regex: /do\(\)/g, type: "do" },
    { regex: /don't\(\)/g, type: "dont" },
  ];

  const instructions: Instruction[] = [];
  for (const { regex, type } of patterns) {
    let match: RegExpExecArray | null;
    while ((match = regex.exec(content)) !== null) {
      instructions.push({
        type: type as Instruction["type"],
        index: match.index,
        value:
          type === "mul" ? [parseInt(match[1]), parseInt(match[2])] : undefined,
      });
    }
  }

  return instructions.sort((a, b) => a.index - b.index);
}

// Solves Part 1: Sum of all mul results
function solvePartOne(content: string): number {
  const mulRegex = /mul\((\d+),(\d+)\)/g;
  let result = 0;
  let match: RegExpExecArray | null;

  while ((match = mulRegex.exec(content)) !== null) {
    result += parseInt(match[1]) * parseInt(match[2]);
  }

  return result;
}

// Solves Part 2: Processes instructions in order
function solvePartTwo(content: string): number {
  const instructions = parseInstructions(content);
  let result = 0;
  let mulEnabled = true;

  for (const { type, value } of instructions) {
    if (type === "do") mulEnabled = true;
    else if (type === "dont") mulEnabled = false;
    else if (type === "mul" && mulEnabled && value) {
      result += value[0] * value[1];
    }
  }

  return result;
}

// Main function: Reads input and prints solutions
function main(): void {
  const [filename] = process.argv.slice(2);

  if (!filename || !existsSync(filename)) {
    console.error(
      filename
        ? `File not found: ${filename}`
        : "Usage: node main.js <input_file>"
    );
    process.exit(1);
  }

  const content = readFileSync(filename, "utf8");
  console.log("Part 1:", solvePartOne(content));
  console.log("Part 2:", solvePartTwo(content));
}

main();
