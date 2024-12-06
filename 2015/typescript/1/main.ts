import { readFileSync, existsSync } from "fs";

function main() {
  // get command line arguments
  const args: string[] = process.argv.slice(1);

  if (args.length < 1) {
    console.log("Usage: node main.js <input_file>");
    process.exit(1);
  }

  const filename: string = args[1];

  if (!existsSync(filename)) {
    console.log(`File not found: ${filename}`);
    process.exit(1);
  }

  const data: string = readFileSync(filename, "utf8");

  const lines: string[] = data.split("\n");
  // loop each character
  var floor: number = 0;
  var count: number = 0;
  var basement: number = 0;

  for (let i = 0; i < lines.length; i++) {
    const line: string = lines[i];
    for (let j = 0; j < line.length; j++) {
      const c: string = line[j];
      if (c === "(") {
        floor++;
      } else if (c === ")") {
        floor--;
      }

      count++;
      if (floor === -1 && basement === 0) {
        basement = count;
      }
    }
  }

  console.log(`Part 1: ${floor}`);
  console.log(`Part 2: ${basement}`);
}

main();
