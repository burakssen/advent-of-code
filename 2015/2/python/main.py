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
    
    total = 0
    for line in lines:
        line = line.split('x')
        
        l = int(line[0])
        w = int(line[1])
        h = int(line[2])
        
        area = 2*l*w + 2*w*h + 2*h*l
        min_side = min(l*w, w*h, h*l)
        total += area + min_side
    
    print(total)



if __name__ == "__main__":
    main()