#include <iostream>
#include <fstream>
#include <string>
#include <map>

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cout << "Usage: " << argv[0] << " <input_file>" << std::endl;
        return EXIT_FAILURE;
    }

    std::string filename = argv[1];

    std::ifstream file(filename);

    if (!file.is_open())
    {
        std::cout << "Error: Could not open file " << filename << std::endl;
        return EXIT_FAILURE;
    }

    std::map<std::pair<int, int>, int> grid;
    std::pair<int, int> pos = {0, 0};
    grid[pos] = 1;

    char c;
    while (file.get(c))
    {
        switch (c)
        {
        case '^':
            pos.second++;
            break;
        case 'v':
            pos.second--;
            break;
        case '>':
            pos.first++;
            break;
        case '<':
            pos.first--;
            break;
        default:
            break;
        }

        grid[pos]++;
    }

    int count = 0;
    for (auto &p : grid)
    {
        if (p.second > 0)
        {
            count++;
        }
    }

    std::cout << "Part 1: " << count << std::endl;

    return EXIT_SUCCESS;
}