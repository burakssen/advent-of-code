import sys

def count_chars(line):
    code_chars = len(line)
    string_chars = 0

    i = 1  # Skip the first quote
    while i < len(line) - 1:
        if line[i] == '\\':
            if line[i + 1] in ['\\', '"']:
                string_chars += 1
                i += 1
            elif line[i + 1] == 'x' and i + 3 < len(line) - 1:
                string_chars += 1
                i += 3
            else:
                string_chars += 1
        else:
            string_chars += 1
        i += 1

    return code_chars, string_chars

def encode_string(line):
    encoded = '"' + line.replace('\\', '\\\\').replace('"', '\\"') + '"'
    return len(encoded)

def process_file(filename):
    try:
        with open(filename, 'r') as file:
            lines = [line.strip() for line in file if line.strip()]
    except FileNotFoundError:
        print(f"File not found: {filename}")
        return None

    code_chars_total = 0
    string_chars_total = 0
    encoded_length_total = 0

    for line in lines:
        code_chars, string_chars = count_chars(line)
        encoded_length = encode_string(line)

        code_chars_total += code_chars
        string_chars_total += string_chars
        encoded_length_total += encoded_length

    return code_chars_total, string_chars_total, encoded_length_total

def main():
    if len(sys.argv) < 2:
        print("Usage: python main.py <input.txt>")
        return

    filename = sys.argv[1]
    result = process_file(filename)

    if result:
        code_chars_total, string_chars_total, encoded_length_total = result
        print("Part 1:", code_chars_total - string_chars_total)
        print("Part 2:", encoded_length_total - code_chars_total)

if __name__ == "__main__":
    main()
