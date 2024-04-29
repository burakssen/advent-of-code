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
