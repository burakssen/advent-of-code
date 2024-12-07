import { readFileSync, existsSync } from "fs";

type Vector2 = { x: number; y: number };

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

  const grid: string[][] = readFileSync(filename, "utf-8")
    .trim()
    .split("\n")
    .map((line) => line.split(""));

  const { guardPos, guardDir, guardPath, distinctPos } =
    calculateGuardPath(grid);

  console.log("Part 1:", distinctPos.size);
  console.log(
    "Part 2:",
    findBlockingPositions(grid, guardPos, guardDir, guardPath)
  );
};

function calculateGuardPath(grid: string[][]): {
  guardPos: Vector2;
  guardDir: Vector2;
  guardPath: { pos: Vector2; dir: Vector2 }[];
  distinctPos: Set<string>;
} {
  let guardPos: Vector2 = { x: 0, y: 0 };
  let guardDir: Vector2 = { x: -1, y: 0 };
  const distinctPos: Set<string> = new Set();

  // Find guard's initial position
  for (let i = 0; i < grid.length; i++) {
    for (let j = 0; j < grid[i].length; j++) {
      if (grid[i][j] === "^") {
        guardPos = { x: i, y: j };
        grid[i][j] = ".";
        break;
      }
    }
  }

  distinctPos.add(`${guardPos.x},${guardPos.y}`);
  const guardPath: { pos: Vector2; dir: Vector2 }[] = [
    { pos: { ...guardPos }, dir: { ...guardDir } },
  ];

  let currentPos = { ...guardPos };
  let currentDir = { ...guardDir };

  while (!isOnEdge(currentPos, grid.length)) {
    const nextPos = moveForward(currentPos, currentDir, grid);
    if (nextPos.x === currentPos.x && nextPos.y === currentPos.y) {
      currentDir = rotateRight(currentDir);
    } else {
      currentPos = nextPos;
      distinctPos.add(`${currentPos.x},${currentPos.y}`);
    }
    guardPath.push({ pos: { ...currentPos }, dir: { ...currentDir } });
  }

  return { guardPos, guardDir, guardPath, distinctPos };
}

function findBlockingPositions(
  grid: string[][],
  guardPos: Vector2,
  guardDir: Vector2,
  guardPath: { pos: Vector2; dir: Vector2 }[]
): number {
  const blockingPositions: Set<string> = new Set();

  for (let i = 0; i < grid.length; i++) {
    for (let j = 0; j < grid[i].length; j++) {
      // Skip starting position and wall tiles
      if ((i === guardPos.x && j === guardPos.y) || grid[i][j] === "#")
        continue;

      if (isBlockingPosition(grid, guardPos, guardDir, guardPath, i, j)) {
        blockingPositions.add(`${i},${j}`);
      }
    }
  }

  return blockingPositions.size;
}

function isBlockingPosition(
  grid: string[][],
  guardPos: Vector2,
  guardDir: Vector2,
  guardPath: { pos: Vector2; dir: Vector2 }[],
  blockX: number,
  blockY: number
): boolean {
  const testGrid = grid.map((row) => [...row]);
  testGrid[blockX][blockY] = "#";

  let testPos = { ...guardPos };
  let testDir = { ...guardDir };
  const visitedStates: Set<string> = new Set();

  for (let step = 0; step < guardPath.length * 3; step++) {
    const nextPos = moveForward(testPos, testDir, testGrid);
    if (nextPos.x === testPos.x && nextPos.y === testPos.y) {
      testDir = rotateRight(testDir);
    } else {
      testPos = nextPos;
    }

    const stateKey = `${testPos.x},${testPos.y},${testDir.x},${testDir.y}`;
    if (visitedStates.has(stateKey)) return true;
    visitedStates.add(stateKey);

    if (isOnEdge(testPos, grid.length)) break;
  }

  return false;
}

function moveForward(
  pos: Vector2,
  dir: Vector2,
  currentGrid: string[][]
): Vector2 {
  const newPos = { x: pos.x + dir.x, y: pos.y + dir.y };
  return currentGrid[newPos.x]?.[newPos.y] !== "#" ? newPos : pos;
}

function rotateRight(dir: Vector2): Vector2 {
  return { x: dir.y, y: -dir.x };
}

function isOnEdge(pos: Vector2, gridSize: number): boolean {
  return (
    pos.x === 0 ||
    pos.y === 0 ||
    pos.x === gridSize - 1 ||
    pos.y === gridSize - 1
  );
}

main();
