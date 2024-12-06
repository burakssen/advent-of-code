#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>

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

    std::vector<std::map<char, int>> columns(8);
    std::string line;
    while (std::getline(file, line))
    {
        for (int i = 0; i < 8; ++i)
            columns[i][line[i]]++;
    }

    std::string message1 = "";
    std::string message2 = "";

    for (const auto &column : columns)
    {
        char max_char = 0;
        int max_count = 0;
        char min_char = 0;
        int min_count = INT_MAX;

        for (const auto &pair : column)
        {
            if (pair.second > max_count)
            {
                max_char = pair.first;
                max_count = pair.second;
            }

            if (pair.second < min_count)
            {
                min_char = pair.first;
                min_count = pair.second;
            }
        }
        message1 += max_char;
        message2 += min_char;
    }

    std::cout << "Part 1: " << message1 << "\n";
    std::cout << "Part 2: " << message2 << "\n";

    file.close();
    return EXIT_SUCCESS;
}
