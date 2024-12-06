import { readFileSync, existsSync } from "fs";

type Report = number[];

function main() {
  const [filename] = process.argv.slice(2);

  if (!filename || !existsSync(filename)) {
    console.log(
      filename
        ? `File not found: ${filename}`
        : "Usage: node main.js <input_file>"
    );
    process.exit(1);
  }

  const lines = readFileSync(filename, "utf8")
    .split("\n")
    .filter((line) => line.trim() !== "");

  const safeReportsPart1 = lines.filter((line) =>
    isSafePart1(line.split(" ").map((x) => parseInt(x)))
  ).length;

  const safeReportsPart2 = lines.filter((line) =>
    isSafePart2(line.split(" ").map((x) => parseInt(x)))
  ).length;

  console.log("Part 1:", safeReportsPart1);
  console.log("Part 2:", safeReportsPart2);
}

function isSafePart1(report: Report): boolean {
  const differences = report.slice(1).map((num, idx) => num - report[idx]);

  return (
    (differences.every((diff) => diff > 0) &&
      differences.every((diff) => diff <= 3)) ||
    (differences.every((diff) => diff < 0) &&
      differences.every((diff) => diff >= -3))
  );
}

function isSafePart2(report: Report): boolean {
  return report.some((_, i) => {
    const newReport = [...report.slice(0, i), ...report.slice(i + 1)];
    return isSafePart1(newReport);
  });
}

main();
