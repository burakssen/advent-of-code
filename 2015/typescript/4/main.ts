import { PathLike, existsSync, readFileSync } from "fs";
import { createHash } from "crypto";

function main() {
  const args: String[] = process.argv.slice(1);
  if (args.length < 2) {
    console.log("Usage: node main.js <input.txt>");
    return;
  }

  const filename: String = args[1];

  if (!existsSync(filename as PathLike)) {
    console.log("File not found:", filename);
  }

  const prefix = readFileSync(filename as PathLike, "utf-8");

  let i = 0;
  let leading_5_found = false;

  while (true) {
    const input = prefix + i;
    const hash = createHash("md5");
    hash.update(input);
    const hash_hex = hash.digest("hex");

    if (hash_hex.startsWith("00000") && !leading_5_found) {
      console.log("Part 1:", i);
      leading_5_found = true;
    }

    if (hash_hex.startsWith("000000")) {
      console.log("Part 2:", i);
      break;
    }
    i += 1;
  }
}

main();
