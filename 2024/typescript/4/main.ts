import { readFileSync, existsSync } from "fs";

function solvePart1(grid: string[][]): number {
  const target = "XMAS";
  const directions = [
    [0, 1], // Right
    [1, 0], // Down
    [1, 1], // Down-Right
    [1, -1], // Down-Left
  ];
  const targetReversed = target.split("").reverse().join("");
  const targets = [target, targetReversed];

  let count = 0;
  for (let r = 0; r < grid.length; r++) {
    for (let c = 0; c < grid[0].length; c++) {
      for (const [dr, dc] of directions) {
        for (const str of targets) {
          if (
            str.split("").every((ch, k) => {
              const nr = r + k * dr,
                nc = c + k * dc;
              return (
                nr >= 0 &&
                nc >= 0 &&
                nr < grid.length &&
                nc < grid[0].length &&
                grid[nr][nc] === ch
              );
            })
          )
            count++;
        }
      }
    }
  }
  return count;
}

function solvePart2(grid: string[][]): number {
  const patterns = [
    [
      ["M", ".", "S"],
      [".", "A", "."],
      ["M", ".", "S"],
    ],
    [
      ["M", ".", "M"],
      [".", "A", "."],
      ["S", ".", "S"],
    ],
    [
      ["S", ".", "M"],
      [".", "A", "."],
      ["S", ".", "M"],
    ],
    [
      ["S", ".", "S"],
      [".", "A", "."],
      ["M", ".", "M"],
    ],
  ];

  let count = 0;
  for (let r = 0; r <= grid.length - 3; r++) {
    for (let c = 0; c <= grid[0].length - 3; c++) {
      for (const pattern of patterns) {
        if (
          pattern.every((row, i) =>
            row.every((ch, j) => ch === "." || grid[r + i][c + j] === ch)
          )
        )
          count++;
      }
    }
  }
  return count;
}

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

  const grid = readFileSync(filename, "utf8")
    .split("\n")
    .map((line) => line.split(""));
  console.log("Part 1:", solvePart1(grid));
  console.log("Part 2:", solvePart2(grid));
}

main();
