#!/usr/bin/env python3

import sys
import os

class Position:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def move(self, direction):
        if direction == "^":
            self.y += 1
        elif direction == "v":
            self.y -= 1
        elif direction == "<":
            self.x -= 1
        elif direction == ">":
            self.x += 1

    
def main():
    # get command line arguments
    args = sys.argv[1:]
    if len(args) == 0:
        print("Usage: python main.py <input_file>")
        sys.exit(1)

    # get input file
    input_file = args[0]
    if not os.path.exists(input_file):
        print("File not found: {}".format(input_file))
        sys.exit(1)

    # read input file
    with open(input_file, "r") as f:
        lines = f.readlines()
        pos = Position(0, 0)

        grid = {}
        grid[(0, 0)] = 1

        for line in lines:
            for char in line:
                pos.move(char)
                if (pos.x, pos.y) in grid:
                    grid[(pos.x, pos.y)] += 1
                else:
                    grid[(pos.x, pos.y)] = 1

            print("Part 1:", len(grid))

if __name__ == "__main__":
    main()