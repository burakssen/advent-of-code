import { readFileSync, existsSync } from "fs";

class HikingTrailSolver {
  private map: number[][];

  constructor(input: string[]) {
    this.map = input.map((row) => row.split("").map(Number));
  }

  // Validate trail movement constraints
  private isValidTrail(path: [number, number][]): boolean {
    return path.every((curr, i) => {
      if (i === path.length - 1) return true;
      const [currY, currX] = curr;
      const [nextY, nextX] = path[i + 1];

      // Check height difference and orthogonal movement
      return (
        this.map[nextY][nextX] - this.map[currY][currX] === 1 &&
        ((Math.abs(nextY - currY) === 1 && nextX === currX) ||
          (Math.abs(nextX - currX) === 1 && nextY === currY))
      );
    });
  }

  // Generic trail finding method with configurable trail validation
  private findTrails(
    startY: number,
    startX: number,
    validator: (path: [number, number][]) => boolean,
    pathProcessor: (path: [number, number][]) => void
  ): void {
    const queue: { path: [number, number][]; current: [number, number] }[] = [
      { path: [[startY, startX]], current: [startY, startX] },
    ];

    while (queue.length > 0) {
      const { path, current } = queue.shift()!;
      const [currY, currX] = current;

      // Orthogonal movement directions
      const directions = [
        [currY - 1, currX],
        [currY, currX + 1],
        [currY + 1, currX],
        [currY, currX - 1],
      ];

      for (const [nextY, nextX] of directions) {
        // Check map bounds
        if (
          nextY >= 0 &&
          nextY < this.map.length &&
          nextX >= 0 &&
          nextX < this.map[0].length
        ) {
          const newPath = [...path, [nextY, nextX] as [number, number]];

          if (validator(newPath)) {
            pathProcessor(newPath);
            queue.push({ path: newPath, current: [nextY, nextX] });
          }
        }
      }
    }
  }

  // Part 1: Calculate total trailhead scores
  calculateTrailheadScores(): number {
    let totalScore = 0;

    for (let y = 0; y < this.map.length; y++) {
      for (let x = 0; x < this.map[y].length; x++) {
        if (this.map[y][x] === 0) {
          const destinations = new Set<string>();

          this.findTrails(y, x, this.isValidTrail.bind(this), (path) => {
            const [lastY, lastX] = path[path.length - 1];
            if (this.map[lastY][lastX] === 9) {
              destinations.add(`${lastY},${lastX}`);
            }
          });

          totalScore += destinations.size;
        }
      }
    }
    return totalScore;
  }

  // Part 2: Calculate trailhead ratings
  calculateTrailheadRatings(): number {
    let totalRating = 0;

    for (let y = 0; y < this.map.length; y++) {
      for (let x = 0; x < this.map[y].length; x++) {
        if (this.map[y][x] === 0) {
          const distinctTrails = new Set<string>();

          this.findTrails(y, x, this.isValidTrail.bind(this), (path) => {
            const [lastY, lastX] = path[path.length - 1];
            if (this.map[lastY][lastX] === 9) {
              const trailKey = path.map(([y, x]) => `${y},${x}`).join("|");
              distinctTrails.add(trailKey);
            }
          });

          totalRating += distinctTrails.size;
        }
      }
    }
    return totalRating;
  }
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

  const content = readFileSync(filename, "utf-8").trim().split("\n");
  const solver = new HikingTrailSolver(content);

  console.log("Part 1:", solver.calculateTrailheadScores());
  console.log("Part 2:", solver.calculateTrailheadRatings());
}

main();
