import { readFileSync, existsSync } from "fs";

type Robot = [[number, number], [number, number]];

const TILES_X = 101;
const TILES_Y = 103;

/** Compute the modular multiplicative inverse of `a` modulo `m`. */
const modInverse = (a: number, m: number): number => {
  let [m0, x0, x1] = [m, 0, 1];
  while (a > 1) {
    const q = Math.floor(a / m);
    [a, m] = [m, a % m];
    [x0, x1] = [x1 - q * x0, x0];
  }
  return x1 < 0 ? x1 + m0 : x1;
};

/** Parse robots from the input file. */
const parseRobots = (content: string[]): Robot[] =>
  content.map(
    (line) =>
      line.split(" ").map((coord) => {
        const [x, y] = coord.slice(2).split(",").map(Number);
        return [x, y] as [number, number];
      }) as Robot
  );

/** Compute safety factors for each time step. */
const computeSafetyFactors = (robots: Robot[], maxTime: number) =>
  Array.from({ length: maxTime }, (_, t) => {
    const quads = [0, 0, 0, 0];
    for (const [[px, py], [vx, vy]] of robots) {
      const x = (((px + vx * t) % TILES_X) + TILES_X) % TILES_X;
      const y = (((py + vy * t) % TILES_Y) + TILES_Y) % TILES_Y;
      if (x !== Math.floor(TILES_X / 2) && y !== Math.floor(TILES_Y / 2)) {
        const quadX = Math.floor(x / (Math.floor(TILES_X / 2) + 1));
        const quadY = Math.floor(y / (Math.floor(TILES_Y / 2) + 1));
        quads[quadY * 2 + quadX]++;
      }
    }
    return { factor: quads.reduce((acc, q) => acc * q, 1), time: t, quads };
  });

/** Determine the minimum safety times for Part 2 calculation. */
const findSafetyTimes = (
  sortedFactors: { time: number; quads: number[] }[],
  robotCount: number
) => {
  let [tx, ty] = [0, 0];
  for (const { time, quads } of sortedFactors) {
    const balancedQuads =
      (quads[0] > robotCount / 4 && quads[2] > robotCount / 4) ||
      (quads[1] > robotCount / 4 && quads[3] > robotCount / 4);
    balancedQuads ? (tx = time) : (ty = time);
  }
  return { tx, ty };
};

const main = () => {
  const [inputPath] = process.argv.slice(2);
  if (!inputPath || !existsSync(inputPath)) {
    console.error(
      `Usage: node main.js <input_file>` +
        (inputPath ? `\nFile not found: ${inputPath}` : "")
    );
    process.exit(1);
  }

  const content = readFileSync(inputPath, "utf-8").trim().split("\n");

  const robots = parseRobots(content);

  const safetyFactors = computeSafetyFactors(
    robots,
    Math.max(TILES_X, TILES_Y)
  );

  const sortedFactors = safetyFactors
    .slice()
    .sort((a, b) => a.factor - b.factor)
    .slice(0, 2);

  const { tx, ty } = findSafetyTimes(sortedFactors, robots.length);

  const part1 = safetyFactors[100].factor;
  const part2 =
    tx +
    ((((modInverse(TILES_X, TILES_Y) * ((ty - tx) % TILES_Y)) % TILES_Y) +
      TILES_Y) %
      TILES_Y) *
      TILES_X;

  console.log("Part 1:", part1);
  console.log("Part 2:", part2);
};

main();
