import { existsSync, readFileSync, PathLike } from "fs";

type Action = {
  act: string;
  start: number[];
  end: number[];
};

function get_action(line: string): Action {
  let parts: string[] = line.split(" ");
  if (parts[0] == "turn") {
    let act: string = parts[1];
    let start: number[] = parts[2].split(",").map(Number);
    let end: number[] = parts[4].split(",").map(Number);
    return { act: act, start: start, end: end };
  } else {
    let act: string = parts[0];
    let start: number[] = parts[1].split(",").map(Number);
    let end: number[] = parts[3].split(",").map(Number);
    return { act: act, start: start, end: end };
  }
}

function main() {
  let args: string[] = process.argv.slice(1);

  if (args.length < 2) {
    console.log("Usage: node main-part1.js <input.txt>");
    return;
  }

  let filename: string = args[1];
  if (!existsSync(filename)) {
    console.log(`File not found: ${filename}`);
    return;
  }

  let input: string = readFileSync(filename, "utf-8");

  let lines: string[] = input.split("\n");

  let grid: boolean[][] = new Array(1000)
    .fill(false)
    .map(() => new Array(1000).fill(false));

  for (let line of lines) {
    let action: Action = get_action(line);
    let act: string = action.act;
    let start: number[] = action.start;
    let end: number[] = action.end;

    for (let i = start[0]; i <= end[0]; i++) {
      for (let j = start[1]; j <= end[1]; j++) {
        if (act == "on") {
          grid[i][j] = true;
        } else if (act == "off") {
          grid[i][j] = false;
        } else {
          grid[i][j] = !grid[i][j];
        }
      }
    }
  }

  let count: number = 0;
  for (let i = 0; i < 1000; i++) {
    for (let j = 0; j < 1000; j++) {
      if (grid[i][j]) {
        count++;
      }
    }
  }

  console.log("Part 1:", count);
}

main();
