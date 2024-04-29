import os
import sys

if __name__ == '__main__':
    args = sys.argv

    if len(args) < 2:
        print('Usage: python main.py <file>')
        sys.exit(1)

    file = args[1]
