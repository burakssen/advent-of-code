import { PathLike, existsSync, readFileSync } from "fs";

class PositionSet<T> {
  private items: { [key: string]: T } = {};

  add(value: T): this {
    const key = this.getKey(value);
    this.items[key] = value;
    return this;
  }

  has(value: T): boolean {
    const key = this.getKey(value);
    return this.items.hasOwnProperty(key);
  }

  remove(value: T): boolean {
    const key = this.getKey(value);
    const hasValue = this.has(value);
    delete this.items[key];
    return hasValue;
  }

  clear(): void {
    this.items = {};
  }

  get size(): number {
    return Object.keys(this.items).length;
  }

  private getKey(value: T): string {
    return typeof value === "object" ? JSON.stringify(value) : `${value}`;
  }
}

interface IMoveable {
  move(direction: String): void;
}

class Position implements IMoveable {
  x: number;
  y: number;

  constructor(x: number, y: number) {
    this.x = x;
    this.y = y;
  }

  move(direction: String): void {
    switch (direction) {
      case "^":
        this.y++;
        break;
      case ">":
        this.x++;
        break;
      case "v":
        this.y--;
        break;
      case "<":
        this.x--;
        break;
      default:
        break;
    }
  }
}

// Keep only one main function declaration
function main() {
  // get command line arguments
  const args: String[] = process.argv.slice(1);
  if (args.length < 2) {
    console.log("Usage: node main.js <input_file>");
    return;
  }

  // read input file
  const filename: String = args[1];

  if (!existsSync(filename as PathLike)) {
    console.log(`File ${filename} does not exist`);
    return;
  }

  const data: string = readFileSync(filename as PathLike, "utf-8");

  var position = new Position(0, 0);

  var visited = new PositionSet<Position>();
  visited.add(position);

  const lines: string[] = data.split("\n");
  for (let line of lines) {
    for (let char of line) {
      position.move(char);
      visited.add(position);
    }
  }

  console.log("Part 1:", visited.size);
}

main();
