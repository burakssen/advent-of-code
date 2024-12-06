#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <cctype>
#include <stdexcept>
#include <algorithm>

// Function to calculate the decompressed length of a given string (Part One)
size_t calculate_decompressed_length(const std::string &input)
{
    size_t length = 0;
    size_t i = 0;
    while (i < input.size())
    {
        if (input[i] == '(')
        {
            // Find the closing parenthesis
            size_t j = input.find(')', i);
            if (j == std::string::npos)
            {
                throw std::runtime_error("Malformed input: missing closing parenthesis");
            }

            // Extract marker and its parameters
            std::string marker = input.substr(i + 1, j - i - 1);
            size_t x_pos = marker.find('x');
            if (x_pos == std::string::npos)
            {
                throw std::runtime_error("Malformed marker: missing 'x'");
            }

            // Extract the length and repeat count
            size_t length_to_repeat = std::stoul(marker.substr(0, x_pos));
            size_t repeat_count = std::stoul(marker.substr(x_pos + 1));

            // Calculate the length of the repeated section
            length += length_to_repeat * repeat_count;

            // Move past the marker and the data to be repeated
            i = j + 1 + length_to_repeat;
        }
        else
        {
            // Just a regular character
            ++length;
            ++i;
        }
    }
    return length;
}

// Function to calculate the decompressed length considering nested markers (Part Two)
size_t calculate_decompressed_length_v2(const std::string &input, size_t start, size_t end)
{
    size_t length = 0;
    size_t i = start;
    while (i < end)
    {
        if (input[i] == '(')
        {
            // Find the closing parenthesis
            size_t j = input.find(')', i);
            if (j == std::string::npos || j >= end)
            {
                throw std::runtime_error("Malformed input: missing closing parenthesis");
            }

            // Extract marker and its parameters
            std::string marker = input.substr(i + 1, j - i - 1);
            size_t x_pos = marker.find('x');
            if (x_pos == std::string::npos)
            {
                throw std::runtime_error("Malformed marker: missing 'x'");
            }

            // Extract the length and repeat count
            size_t length_to_repeat = std::stoul(marker.substr(0, x_pos));
            size_t repeat_count = std::stoul(marker.substr(x_pos + 1));

            // Calculate the end of the segment to be repeated
            size_t segment_end = j + 1 + length_to_repeat;

            // Recursively calculate the length of the segment to be repeated
            size_t segment_length = calculate_decompressed_length_v2(input, j + 1, segment_end);

            // Add the length of the segment repeated count times
            length += segment_length * repeat_count;

            // Move past the repeated segment
            i = segment_end;
        }
        else
        {
            // Just a regular character
            ++length;
            ++i;
        }
    }
    return length;
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cerr << "Usage: " << argv[0] << " <input_file>\n";
        return EXIT_FAILURE;
    }

    std::ifstream file(argv[1]);
    if (!file)
    {
        std::cerr << "Error: Could not open file " << argv[1] << "\n";
        return EXIT_FAILURE;
    }

    // Read the entire file content
    std::string content((std::istreambuf_iterator<char>(file)),
                        std::istreambuf_iterator<char>());

    // Close the file
    file.close();

    // Remove whitespace from content
    content.erase(std::remove_if(content.begin(), content.end(), ::isspace), content.end());

    // Calculate the decompressed length for Part One
    try
    {
        size_t decompressed_length = calculate_decompressed_length(content);
        std::cout << "Part 1: " << decompressed_length << "\n";

        // Calculate the decompressed length using version 2 format
        size_t decompressed_length_v2 = calculate_decompressed_length_v2(content, 0, content.size());
        std::cout << "Part 2: " << decompressed_length_v2 << "\n";
    }
    catch (const std::runtime_error &e)
    {
        std::cerr << "Error: " << e.what() << "\n";
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
