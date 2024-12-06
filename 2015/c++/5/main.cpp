#include <iostream>
#include <fstream>
#include <string>

bool check_good_string_part1(std::string line, int len);
bool check_good_string_part2(std::string line, int len);
int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cerr << "Usage: " << argv[0] << " <input.txt>\n";
        return EXIT_FAILURE;
    }

    std::string filename = argv[1];

    std::fstream file(filename, std::ios::in);

    if (!file.is_open())
    {
        std::cerr << "Error: Could not open file " << filename << '\n';
        return EXIT_FAILURE;
    }

    std::string line;
    int part1_count = 0;
    int part2_count = 0;

    while (std::getline(file, line, '\n'))
    {
        int len = line.length();

        if (check_good_string_part1(line, len))
            part1_count++;

        if (check_good_string_part2(line, len))
            part2_count++;
    }

    std::cout << "Part 1: " << part1_count << '\n';
    std::cout << "Part 2: " << part2_count << '\n';
    file.close();

    return EXIT_SUCCESS;
}

bool check_good_string_part1(std::string line, int len)
{
    int vowels = 0;
    bool double_letter = false;
    bool bad_string = false;

    for (int i = 0; i < len; i++)
    {
        if (line[i] == 'a' || line[i] == 'e' || line[i] == 'i' || line[i] == 'o' || line[i] == 'u')
            vowels++;

        if (i > 0 && line[i] == line[i - 1])
            double_letter = true;

        if (i > 0 && (line[i] == 'b' && line[i - 1] == 'a') || (line[i] == 'd' && line[i - 1] == 'c') || (line[i] == 'q' && line[i - 1] == 'p') || (line[i] == 'y' && line[i - 1] == 'x'))
            bad_string = true;
    }

    return vowels >= 3 && double_letter && !bad_string;
}

bool check_good_string_part2(std::string line, int len)
{
    bool pair = false;
    bool repeat = false;

    for (int i = 0; i < len - 1; i++)
    {
        if (line.find(line.substr(i, 2), i + 2) != std::string::npos)
            pair = true;

        if (i < len - 2 && line[i] == line[i + 2])
            repeat = true;
    }

    return pair && repeat;
}