#!/usr/bin/env python3

import sys
import os

def main():
    args = sys.argv[1:]

    if len(args) == 0:
        print("Usage: python3 main.py <input_file>")
        return
    
    input_file = args[0]
    if not os.path.exists(input_file):
        print("File not found")
        return
    
    lines = []
    with open(input_file, "r") as f:
        lines = f.readlines()
    
    paper_size = 0
    ribbon_length = 0
    for line in lines:
        line = line.split('x')
        
        l = int(line[0])
        w = int(line[1])
        h = int(line[2])

        sides = [l, w, h]
        sides.sort()
        ribbon_length += 2*sides[0] + 2*sides[1] + l*w*h
        
        area = 2*l*w + 2*w*h + 2*h*l
        min_side = min(l*w, w*h, h*l)
        paper_size += area + min_side
    
    print("Part 1:", paper_size)
    print("Part 2:", ribbon_length)



if __name__ == "__main__":
    main()