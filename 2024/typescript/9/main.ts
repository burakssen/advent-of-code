import { readFileSync, existsSync } from "fs";

function solve(content: number[], part2 = false): number {
  const expanded = expandDiskMap(content);
  return generateChecksum(part2 ? compactBlocks(expanded) : compact(expanded));
}

function expandDiskMap(rs: number[]): number[] {
  const expanded = [];
  for (let ix = 0; ix < rs.length; ix++) {
    const value = ix % 2 === 0 ? Math.floor(ix / 2) : -1;
    expanded.push(...Array(rs[ix]).fill(value));
  }
  return expanded;
}

function compact(rs: number[]): number[] {
  let lIx = 0,
    rIx = rs.length - 1;

  while (lIx < rIx) {
    while (lIx < rIx && rs[lIx] !== -1) lIx++;
    while (lIx < rIx && rs[rIx] === -1) rIx--;
    if (lIx < rIx) {
      rs[lIx] = rs[rIx];
      rs[rIx] = -1;
    }
  }
  return rs;
}

function compactBlocks(rs: number[]): number[] {
  const n = rs.length;
  const result = [...rs]; // Copy to avoid modifying input in-place

  for (let rIx = n - 1; rIx >= 0; ) {
    const rValue = result[rIx];
    if (rValue === -1) {
      rIx--;
      continue;
    }

    // Find the start of the block
    let blockStart = rIx;
    while (blockStart > 0 && result[blockStart - 1] === rValue) {
      blockStart--;
    }
    const blockSize = rIx - blockStart + 1;

    // Find first available space for the block
    for (let lIx = 0; lIx <= blockStart - blockSize; lIx++) {
      let canMove = true;
      for (let i = 0; i < blockSize; i++) {
        if (result[lIx + i] !== -1) {
          canMove = false;
          break;
        }
      }

      if (canMove) {
        // Move block
        for (let i = 0; i < blockSize; i++) {
          result[lIx + i] = result[blockStart + i];
          result[blockStart + i] = -1;
        }
        break;
      }
    }

    rIx = blockStart - 1;
  }

  return result;
}

function generateChecksum(rs: number[]): number {
  let checksum = 0;
  for (let i = 0; i < rs.length; i++) {
    if (rs[i] !== -1) {
      checksum += i * rs[i];
    }
  }
  return checksum;
}

function main() {
  const [filename] = process.argv.slice(2);
  if (!filename || !existsSync(filename)) {
    console.error(
      filename
        ? `File not found: ${filename}`
        : "Usage: node main.js <input_file>"
    );
    process.exit(1);
  }

  const content = readFileSync(filename, "utf-8").split("").map(Number);

  console.log("Part 1:", solve(content));
  console.log("Part 2:", solve(content, true));
}

main();
