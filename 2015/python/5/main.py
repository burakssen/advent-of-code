import os
import sys

def check_good_string_part1(s):
    vowels = 0
    double = False
    bad = False

    for i in range(len(s)):
        if s[i] in "aeiou":
            vowels += 1
        if i > 0 and s[i] == s[i-1]:
            double = True
        if i > 0 and s[i-1:i+1] in ["ab", "cd", "pq", "xy"]:
            bad = True

    return vowels >= 3 and double and not bad

def check_good_string_part2(s):
    double = False
    repeat = False

    for i in range(len(s) - 1):
        if s[i:i+2] in [s[j:j+2] for j in range(i+2, len(s) - 1)]:
            repeat = True
        if i < len(s) - 2 and s[i] == s[i+2]:
            double = True
            
    return double and repeat

def main():
    args = sys.argv

    if len(args) < 2:
        print("Usage: python main.py <input.txt>")
        return

    filename = args[1]
    if not os.path.exists(filename):
        print("File not found: " + filename)
        return

    
    part1_count = 0
    part2_count = 0
    with open(filename, "r") as f:
        for line in f:
            if check_good_string_part1(line.strip()):
                part1_count += 1
            
            if check_good_string_part2(line.strip()):
                part2_count += 1
    
    print("Part 1:", part1_count)
    print("Part 2:", part2_count)

if __name__ == "__main__":
    main()