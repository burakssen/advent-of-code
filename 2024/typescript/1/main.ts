import { readFileSync, existsSync } from "fs";

function main() {
  // get command line arguments
  const args: string[] = process.argv.slice(2);
  if (args.length < 1) {
    console.log("Usage: node main.js <input_file>");
    process.exit(1);
  }

  const filename: string = args[0];

  if (!existsSync(filename)) {
    console.log(`File not found: ${filename}`);
    process.exit(1);
  }

  const data: string = readFileSync(filename, "utf8");
  const lines: string[] = data.split("\n").filter((line) => line.trim() !== "");

  const leftList: number[] = new Array(lines.length);
  const rightList: number[] = new Array(lines.length);

  // Single pass parsing with direct array population
  for (let i = 0; i < lines.length; i++) {
    const [left, right] = lines[i].split("   ").map(Number);
    leftList[i] = left;
    rightList[i] = right;
  }

  // sort the lists
  leftList.sort((a, b) => a - b);
  rightList.sort((a, b) => a - b);

  // calculate the total distance
  const totalDistance: number = leftList.reduce((sum, left, i) => {
    return sum + Math.abs(left - rightList[i]);
  }, 0);

  console.log(`Part 1: ${totalDistance}`);

  // calculate the similarity score
  const countRightNumbers = new Map<number, number>();
  for (const num of rightList) {
    countRightNumbers.set(num, (countRightNumbers.get(num) || 0) + 1);
  }

  const similarityScore: number = leftList.reduce((sum, left) => {
    const count = countRightNumbers.get(left) || 0;
    return sum + (count > 0 ? left * count : 0);
  }, 0);

  console.log(`Part 2: ${similarityScore}`);
}

main();
