#include <iostream>
#include <fstream>
#include <string>

// Function to generate the dragon curve data
std::string generate_dragon_curve(const std::string &a, size_t length)
{
    std::string result = a;

    while (result.size() < length)
    {
        std::string b = result;
        // Reverse b
        std::reverse(b.begin(), b.end());
        // Invert b
        for (char &c : b)
        {
            c = (c == '0') ? '1' : '0';
        }
        // Concatenate a, '0', and b
        result += '0' + b;
    }

    return result.substr(0, length); // Only keep the first `length` characters
}

// Function to calculate the checksum
std::string calculate_checksum(const std::string &data)
{
    std::string checksum = data;

    while (checksum.length() % 2 == 0)
    {
        std::string new_checksum;
        for (size_t i = 0; i < checksum.length(); i += 2)
        {
            new_checksum += (checksum[i] == checksum[i + 1]) ? '1' : '0';
        }
        checksum = new_checksum;
    }

    return checksum;
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

    std::string line;
    std::getline(file, line);
    file.close();

    // Specify the disk length
    const size_t disk_length = 272;

    // Generate data
    std::string data = generate_dragon_curve(line, disk_length);

    // Calculate checksum
    std::string checksum = calculate_checksum(data);

    // Output the checksum
    std::cout << "Part 1: " << checksum << std::endl;

    data = generate_dragon_curve(line, 35651584);

    checksum = calculate_checksum(data);

    std::cout << "Part 2: " << checksum << std::endl;

    return EXIT_SUCCESS;
}
