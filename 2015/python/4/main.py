# Day 4: The Ideal Stocking Stuffer
# https://adventofcode.com/2015/day/4

from hashlib import md5
import os
import sys

def main():
    # get command line arguments
    if len(sys.argv) != 2:
        print("Usage: python main.py <input.txt>")
        sys.exit(1)

    file_name = sys.argv[1]

    # read input file
    prefix = ""
    with open(file_name, "r") as file:
        prefix = file.readline().strip()

    i = 0
    leading_5_found = False
    while True:
        input_val = prefix + str(i)
        hash_val = md5(input_val.encode()).hexdigest()
        
        if hash_val[:6] == "000000":
            print("Part 2:", i)
            break
        
        if hash_val[:5] == "00000" and not leading_5_found:
            print("Part 1:", i)
            leading_5_found = True

        i+=1

if __name__ == "__main__":
    main()