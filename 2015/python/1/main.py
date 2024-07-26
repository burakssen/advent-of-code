#!/usr/bin/env python3.12.3
import os
import sys

if __name__ == '__main__':
    args = sys.argv

    if len(args) < 2:
        print('Usage: python main.py <file>')
        sys.exit(1)

    file = args[1]

    if not os.path.exists(file):
        print('File not found')
        sys.exit(1)

    file_content = None
    with open(file) as f:
        file_content = f.read()

    floor = 0
    count = 0
    basement = 0
    for i, c in enumerate(file_content):
        if c == '(':
            floor += 1
        elif c == ')':
            floor -= 1

        count += 1
        if floor == -1 and basement == 0:
            basement = count

    print("Part 1:", floor)
    print("Part 2:", basement)