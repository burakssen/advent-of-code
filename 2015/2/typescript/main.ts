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

  let total = 0;
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

    const sides: number[] = [l * w, w * h, h * l];
    const min_side: number = Math.min(...sides);

    total += 2 * sides.reduce((a, b) => a + b) + min_side;
  }

  console.log(total);
}

main();
