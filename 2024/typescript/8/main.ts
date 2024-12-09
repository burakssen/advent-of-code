import { readFileSync, existsSync } from "fs";

interface Point {
  x: number;
  y: number;
  frequency: string;
}

const parseAntennas = (input: string): Point[] =>
  input
    .trim()
    .split("\n")
    .flatMap(
      (line, y) =>
        line
          .split("")
          .map((char, x) => (char !== "." ? { x, y, frequency: char } : null))
          .filter(Boolean) as Point[]
    );

const isValidPoint = (
  x: number,
  y: number,
  width: number,
  height: number
): boolean => x >= 0 && x < width && y >= 0 && y < height;

const isCollinear = (
  x1: number,
  y1: number,
  x2: number,
  y2: number,
  x: number,
  y: number
): boolean => (y2 - y1) * (x - x1) === (y - y1) * (x2 - x1);

const findAntinodes = (input: string, part2 = false): number => {
  const lines = input.trim().split("\n");
  const antennas = parseAntennas(input);
  const antinodes = new Set<string>();
  const [width, height] = [lines[0].length, lines.length];

  for (let i = 0; i < antennas.length; i++) {
    for (let j = i + 1; j < antennas.length; j++) {
      const { x: x1, y: y1, frequency: freq1 } = antennas[i];
      const { x: x2, y: y2, frequency: freq2 } = antennas[j];

      if (freq1 !== freq2) continue;

      if (!part2) {
        const candidatePoints = [
          { x: x1 - (x2 - x1), y: y1 - (y2 - y1) },
          { x: x2 + (x2 - x1), y: y2 + (y2 - y1) },
        ];

        candidatePoints.forEach(({ x, y }) => {
          if (isValidPoint(x, y, width, height)) antinodes.add(`${x},${y}`);
        });
      } else {
        for (let y = 0; y < height; y++) {
          for (let x = 0; x < width; x++) {
            if (isCollinear(x1, y1, x2, y2, x, y)) antinodes.add(`${x},${y}`);
          }
        }
      }
    }
  }

  if (part2) {
    const freqCount = antennas.reduce(
      (map, { frequency }) => map.set(frequency, (map.get(frequency) || 0) + 1),
      new Map<string, number>()
    );

    antennas.forEach(({ x, y, frequency }) => {
      if ((freqCount.get(frequency) || 0) > 1) antinodes.add(`${x},${y}`);
    });
  }

  return antinodes.size;
};

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

  const input = readFileSync(filename, "utf-8");
  const results = [false, true].map((part2) => findAntinodes(input, part2));

  console.log("Part 1:", results[0]);
  console.log("Part 2:", results[1]);
};

main();
