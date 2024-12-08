import { readFileSync, existsSync } from "fs";

const main = (): void => {
  const [filename] = process.argv.slice(2);
  if (!filename || !existsSync(filename)) {
    console.error(
      filename
        ? `File not found: ${filename}`
        : "Usage: node main.js <input_file>"
    );
    process.exit(1);
  }

  const content = readFileSync(filename, "utf-8").trim().split("\n");
  let part1_result = 0;
  let part2_result = 0;

  content.forEach((line) => {
    const [rawTestValue, rawValues] = line.split(":");
    if (!rawValues) return;

    const test_value = parseInt(rawTestValue, 10);
    const values = rawValues.trim().split(" ").map(Number);

    if (evaluate(test_value, values, operators1)) part1_result += test_value;
    if (evaluate(test_value, values, operators2)) part2_result += test_value;
  });

  console.log("Part 1:", part1_result);
  console.log("Part 2:", part2_result);
};

const operators1 = [
  (a: number, b: number) => a * b,
  (a: number, b: number) => a + b,
];
const operators2 = [
  ...operators1,
  (a: number, b: number) => parseInt(`${a}${b}`, 10),
];

const evaluate = (
  target: number,
  values: number[],
  operators: ((a: number, b: number) => number)[]
): boolean => {
  const n = values.length;
  const combinations = Math.pow(operators.length, n - 1);

  for (let mask = 0; mask < combinations; mask++) {
    let result = values[0];
    let currentMask = mask;

    for (let j = 0; j < n - 1; j++) {
      const op = operators[currentMask % operators.length];
      currentMask = Math.floor(currentMask / operators.length);
      result = op(result, values[j + 1]);
    }

    if (result === target) return true;
  }

  return false;
};

main();
