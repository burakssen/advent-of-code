import { readFileSync, existsSync } from "fs";

type Grid = string[][];
type MetricCalculator = (
  row: number,
  col: number,
  grid: Grid,
  originalGrid: Grid
) => number;

const DIRECTIONS: [number, number][] = [
  [-1, 0], // UP
  [0, 1], // RIGHT
  [1, 0], // DOWN
  [0, -1], // LEFT
];

const isWithinBounds = (row: number, col: number, grid: Grid): boolean =>
  row >= 0 && row < grid.length && col >= 0 && col < grid[row].length;

const calculateEdgeCount = (
  row: number,
  col: number,
  grid: Grid,
  originalGrid: Grid
): number => {
  const plantType = grid[row][col];
  return DIRECTIONS.reduce((count, [dr, dc]) => {
    const [nr, nc] = [row + dr, col + dc];
    return (
      count +
      (!isWithinBounds(nr, nc, originalGrid) || grid[nr][nc] !== plantType
        ? 1
        : 0)
    );
  }, 0);
};

const calculateRegionMetric = (
  startRow: number,
  startCol: number,
  grid: Grid,
  metricCalculator: MetricCalculator,
  originalGrid: Grid
): number => {
  let regionArea = 1;
  let totalMetric = metricCalculator(startRow, startCol, grid, originalGrid);
  const plantType = originalGrid[startRow][startCol];
  const queue: [number, number][] = [[startRow, startCol]];
  originalGrid[startRow][startCol] = "#";

  while (queue.length) {
    const [row, col] = queue.shift()!;
    for (const [dr, dc] of DIRECTIONS) {
      const [nr, nc] = [row + dr, col + dc];
      if (
        isWithinBounds(nr, nc, originalGrid) &&
        originalGrid[nr][nc] === plantType
      ) {
        regionArea++;
        totalMetric += metricCalculator(nr, nc, grid, originalGrid);
        originalGrid[nr][nc] = "#";
        queue.push([nr, nc]);
      }
    }
  }
  return regionArea * totalMetric;
};

const computeTotalPrice = (
  grid: Grid,
  metricCalculator: MetricCalculator,
  originalGrid: Grid
): number => {
  let totalPrice = 0;
  for (let row = 0; row < originalGrid.length; row++) {
    for (let col = 0; col < originalGrid[row].length; col++) {
      if (originalGrid[row][col] !== "#") {
        totalPrice += calculateRegionMetric(
          row,
          col,
          grid,
          metricCalculator,
          originalGrid
        );
      }
    }
  }
  return totalPrice;
};

const calculateConcaveSides = (
  row: number,
  col: number,
  grid: Grid,
  originalGrid: Grid
): number => {
  const plantType = grid[row][col];
  return DIRECTIONS.reduce((count, [dr, dc], i) => {
    const [nr, nc] = [row + dr, col + dc];
    if (!isWithinBounds(nr, nc, originalGrid) || grid[nr][nc] !== plantType) {
      const [pdr, pdc] = DIRECTIONS[(i + 3) % 4]; // Counter-clockwise direction
      const [diagRow, diagCol] = [row + dr + pdr, col + dc + pdc];
      const isConcaveCorner =
        isWithinBounds(diagRow, diagCol, originalGrid) &&
        grid[diagRow][diagCol] === plantType;
      const isOuterEdge =
        !isWithinBounds(row + pdr, col + pdc, originalGrid) ||
        grid[row + pdr][col + pdc] !== plantType;
      return count + (isConcaveCorner || isOuterEdge ? 1 : 0);
    }
    return count;
  }, 0);
};

const main = (): void => {
  const [inputFilename] = process.argv.slice(2);
  if (!inputFilename || !existsSync(inputFilename)) {
    console.error(
      inputFilename
        ? `File not found: ${inputFilename}`
        : "Usage: node main.js <input_file>"
    );
    process.exit(1);
  }

  const fileContent = readFileSync(inputFilename, "utf-8").trim().split("\n");
  const originalGrid: Grid = fileContent.map((line) => line.split(""));
  const workingGrid: Grid = fileContent.map((line) => line.split(""));

  // Part 1: Calculate total edge price
  const part1Result = computeTotalPrice(
    workingGrid,
    calculateEdgeCount,
    originalGrid
  );
  console.log("Part 1 answer:", part1Result);

  // Reset the grid to the original state
  originalGrid.forEach((row, rowIndex) => {
    row.forEach((_, colIndex) => {
      originalGrid[rowIndex][colIndex] = workingGrid[rowIndex][colIndex];
    });
  });

  // Part 2: Calculate total concave side price
  const part2Result = computeTotalPrice(
    workingGrid,
    calculateConcaveSides,
    originalGrid
  );
  console.log("Part 2 answer:", part2Result);
};

main();
