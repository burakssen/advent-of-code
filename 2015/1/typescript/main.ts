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
}

main();
