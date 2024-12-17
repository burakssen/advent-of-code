import { readFileSync, existsSync } from "fs";

const step = (r: Record<number, number>, x: number, n: number) => {
  const add = (key: number) => (r[key] = (r[key] || 0) + n);

  if (x === 0) return add(1);

  const s = x.toString();
  const h = Math.floor(s.length / 2);

  s.length % 2 === 0
    ? [s.slice(0, h), s.slice(h)].forEach((part) => add(parseInt(part, 10)))
    : add(x * 2024);
};

const solve = (d: Record<number, number>, k: number) => {
  for (let i = 0; i < k; i++) {
    const r = {};
    for (const [key, value] of Object.entries(d)) step(r, +key, value);
    d = r;
  }
  return Object.values(d).reduce((acc, count) => acc + count, 0);
};

const main = () => {
  const [filename] = process.argv.slice(2);

  if (!filename || !existsSync(filename)) {
    console.error(
      filename
        ? `File not found: ${filename}`
        : "Usage: node main.js <input_file>"
    );
    process.exit(1);
  }

  const content = readFileSync(filename, "utf-8").trim().split(" ").map(Number);
  const d = content.reduce(
    (acc, x) => ({ ...acc, [x]: (acc[x] || 0) + 1 }),
    {}
  );

  console.log("Part 1:", solve({ ...d }, 25));
  console.log("Part 2:", solve({ ...d }, 75));
};

main();
