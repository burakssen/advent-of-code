#!/usr/bin/env python3

import sys
import os

def get_action(line):
    words = line.split()
    if words[0] == "turn":
        return {
            "action": words[1],
            "start": words[2].split(","),
            "end": words[4].split(",")
        }
    else:
        return {
            "action": words[0],
            "start": words[1].split(","),
            "end": words[3].split(",")
        }

def main():
    # get command line arguments
    if len(sys.argv) != 2:
        print("Usage: python3 main-part1.py <input.txt>")
        sys.exit(1)

    # get input file
    filename = sys.argv[1]

    # check if file exists
    if not os.path.exists(filename):
        print(f"Error: File '{filename}' not found")
        sys.exit(1)

    # read file
    lines = []
    with open(filename, 'r') as f:
        lines = f.readlines()

    grid = [[0 for i in range(1000)] for j in range(1000)]
    # get total area
    for line in lines:
        action = get_action(line)
        for i in range(int(action["start"][0]), int(action["end"][0]) + 1):
            for j in range(int(action["start"][1]), int(action["end"][1]) + 1):
                if action["action"] == "on":
                    grid[i][j] += 1
                elif action["action"] == "off":
                    grid[i][j] -= 1
                    if grid[i][j] < 0:
                        grid[i][j] = 0
                else:
                    grid[i][j] += 2

    # count total lights on
    total = 0
    for i in range(1000):
        for j in range(1000):
            total += grid[i][j]

    print("Part 2:", total)

if __name__ == "__main__":
    main()