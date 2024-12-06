import { PathLike, existsSync, readFileSync } from "fs";

function check_good_string_part1(s: string): boolean {
  let vowels: number = 0;
  let double_letter: boolean = false;
  let bad_strings: boolean = false;

  for (let i = 0; i < s.length; i++) {
    if ("aeiou".includes(s[i])) {
      vowels++;
    }

    if (i > 0 && s[i] === s[i - 1]) {
      double_letter = true;
    }

    if (["ab", "cd", "pq", "xy"].includes(s.slice(i, i + 2))) {
      bad_strings = true;
    }
  }

  return vowels >= 3 && double_letter && !bad_strings;
}

function check_good_string_part2(s: string): boolean {
  let double_pair: boolean = false;
  let repeat_with_gap: boolean = false;

  for (let i = 0; i < s.length - 1; i++) {
    if (s.slice(i + 2).includes(s.slice(i, i + 2))) {
      double_pair = true;
    }

    if (s[i] === s[i + 2]) {
      repeat_with_gap = true;
    }
  }

  return double_pair && repeat_with_gap;
}

function main() {
  // get command line arguments
  const args: string[] = process.argv.slice(1);

  if (args.length < 2) {
    console.log("Usage: node main.js <input.txt>");
    return;
  }

  const filename: string = args[1];

  if (!existsSync(filename as PathLike)) {
    console.log(`File ${filename} does not exist`);
    return;
  }

  const data: string = readFileSync(filename as PathLike, "utf8");

  const lines: string[] = data.split("\n");

  let part1_count: number = 0;
  let part2_count: number = 0;

  for (let line of lines) {
    if (check_good_string_part1(line)) {
      part1_count++;
    }

    if (check_good_string_part2(line)) {
      part2_count++;
    }
  }

  console.log("Part 1:", part1_count);
  console.log("Part 2:", part2_count);
}

main();
