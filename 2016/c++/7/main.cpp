#include <iostream>
#include <fstream>
#include <string>
#include <vector>

bool check_abba(std::string line)
{
    bool inside = false;
    bool outside = false;
    for (size_t i = 0; i < line.size() - 3; ++i)
    {
        if (line[i] == '[')
            inside = true;
        else if (line[i] == ']')
            inside = false;
        else if (line[i] == line[i + 3] && line[i + 1] == line[i + 2] && line[i] != line[i + 1])
        {
            if (inside)
                return false;
            outside = true;
        }
    }
    return outside;
}

bool check_ssl(std::string line)
{
    std::vector<std::string> abas;
    std::vector<std::string> babs;
    bool inside = false;
    for (size_t i = 0; i < line.size() - 2; ++i)
    {
        if (line[i] == '[')
            inside = true;
        else if (line[i] == ']')
            inside = false;
        else if (line[i] == line[i + 2] && line[i] != line[i + 1])
        {
            if (inside)
                babs.push_back(line.substr(i, 3));
            else
                abas.push_back(line.substr(i, 3));
        }
    }

    for (const auto &aba : abas)
    {
        for (const auto &bab : babs)
        {
            if (aba[0] == bab[1] && aba[1] == bab[0])
                return true;
        }
    }
    return false;
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
    int count1 = 0;
    int count2 = 0;
    while (std::getline(file, line))
    {
        if (check_abba(line))
            count1++;

        // Part 2
        if (check_ssl(line))
            count2++;
    }

    std::cout << "Part 1: " << count1 << "\n";
    std::cout << "Part 2: " << count2 << "\n";

    file.close();
    return EXIT_SUCCESS;
}
