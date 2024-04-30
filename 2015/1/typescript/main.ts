import { readFileSync, existsSync } from "fs";

function main() {
  // get command line arguments
  const args: string[] = process.argv.slice(1);

  if (args.length < 1) {
    console.log("Usage: node main.js <input_file>");
    process.exit(1);
  }
}

main();
