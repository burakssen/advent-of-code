#include <iostream>
#include <fstream>
#include <string>

void count_characters(const std::string &input, int &code_chars, int &string_chars)
{
    int len = input.length();
    code_chars += len;

    for (int i = 1; i < len - 1; ++i)
    { // Skip first and last quote
        if (input[i] == '\\')
        {
            if (i + 1 < len - 1)
            {
                switch (input[i + 1])
                {
                case '\\':
                case '"':
                    string_chars++;
                    i++;
                    break;
                case 'x':
                    if (i + 3 < len - 1)
                    {
                        string_chars++;
                        i += 3;
                    }
                    break;
                default:
                    string_chars++;
                }
            }
        }
        else
        {
            string_chars++;
        }
    }
}

int encode_string(const std::string &input, std::string &output)
{
    output = "\"";

    for (char ch : input)
    {
        if (ch == '"' || ch == '\\')
        {
            output += '\\';
        }
        output += ch;
    }

    output += "\"";
    return output.length();
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cerr << "Error: missing input file" << std::endl;
        return EXIT_FAILURE;
    }

    std::string filename = argv[1];
    std::ifstream file(filename);
    if (!file)
    {
        std::cerr << "Error: cannot open file" << std::endl;
        return EXIT_FAILURE;
    }

    int total_code_chars = 0;
    int total_string_chars = 0;

    int total_original_chars = 0;
    int total_encoded_chars = 0;

    std::string buffer;
    std::string encoded;
    while (std::getline(file, buffer))
    {
        // Remove newline character if present
        if (!buffer.empty() && buffer.back() == '\n')
        {
            buffer.pop_back();
        }

        // Part 1: Count characters
        count_characters(buffer, total_code_chars, total_string_chars);

        // Part 2: Encode string
        int original_len = buffer.length();
        total_original_chars += original_len;

        int encoded_len = encode_string(buffer, encoded);
        total_encoded_chars += encoded_len;
    }

    std::cout << "Part 1: " << total_code_chars - total_string_chars << std::endl;
    std::cout << "Part 2: " << total_encoded_chars - total_original_chars << std::endl;

    return EXIT_SUCCESS;
}
