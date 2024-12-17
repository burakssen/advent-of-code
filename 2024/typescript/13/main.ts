import { readFileSync, existsSync } from "fs";

const main = () => {
  const [inputFilename] = process.argv.slice(2);
  if (!inputFilename || !existsSync(inputFilename)) {
    console.error(
      `Usage: node main.js <input_file>${
        inputFilename ? `\nFile not found: ${inputFilename}` : ""
      }`
    );
    process.exit(1);
  }

  const lines = readFileSync(inputFilename, "utf-8")
    .trim()
    .split("\n")
    .map((line) => line.trim());
  console.log("Part 1:", solve(lines));
  console.log("Part 2:", solve(lines, true));
};

main();

function solve(lines: string[], part2: boolean = false): number {
  let tokens = 0;
  const add = part2 ? 1e13 : 0;
  let [x1, y1, x2, y2] = [0, 0, 0, 0];

  for (const line of lines) {
    if (line.startsWith("Button")) {
      const [, btn, pos1, pos2] = line.split(" ");
      const [x, y] = [pos1.slice(2, -1), pos2.slice(2)].map(Number);
      if (btn.startsWith("A:")) [x1, y1] = [x, y];
      else [x2, y2] = [x, y];
    } else if (line.startsWith("Prize")) {
      const [, pos1, pos2] = line.split(" ");
      const [c, d] = [pos1.slice(2, -1), pos2.slice(2)].map(
        (v) => parseInt(v) + add
      );
      const denom = x1 * y2 - y1 * x2;
      const [a, b] = [(c * y2 - d * x2) / denom, (d * x1 - c * y1) / denom];
      if (Number.isInteger(a) && Number.isInteger(b)) tokens += 3 * a + b;
    }
  }

  return tokens;
}
