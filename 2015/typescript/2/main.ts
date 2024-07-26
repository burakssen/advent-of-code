import { readFileSync, existsSync, PathLike } from "fs";

function main() {
  // get command line arguments
  const args: String[] = process.argv.slice(1);

  if (args.length < 2) {
    console.log("Build: tsc main.ts");
    console.log("Usage: main main.js <input_file>");
    return;
  }

  const input_file: String = args[1];

  if (!existsSync(input_file as PathLike)) {
    console.log("File not found: ", input_file);
    return;
  }

  const input = readFileSync(input_file as PathLike, "utf8");

  const lines: String[] = input.split("\n");

  let paper_size: number = 0;
  let ribbon_length: number = 0;
  for (let line of lines) {
    // split line by x
    const parts: String[] = line.split("x");

    if (parts.length != 3) {
      console.log("Invalid input: ", line);
      continue;
    }

    const l: number = parseInt(parts[0] as string);
    const w: number = parseInt(parts[1] as string);
    const h: number = parseInt(parts[2] as string);

    let sides: number[] = [l, w, h];
    sides.sort((a, b) => a - b);
    // calculate ribbon length
    ribbon_length += 2 * sides[0] + 2 * sides[1] + l * w * h;
    // calculate paper size
    const min_side: number = sides[0] * sides[1];

    paper_size += 2 * l * w + 2 * w * h + 2 * h * l + min_side;
  }

  console.log("Part 1:", paper_size);
  console.log("Part 2:", ribbon_length);
}

main();
