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
    std::pair<int, int> santa = {0, 0};
    std::pair<int, int> robo_santa = {0, 0};
    grid[santa] = 1;
    grid[robo_santa] += 1;

    char c;
    bool santa_turn = true;
    while (file.get(c))
    {
        std::pair<int, int> *current_santa = santa_turn ? &santa : &robo_santa;

        switch (c)
        {
        case '^':
            current_santa->second++;
            break;
        case 'v':
            current_santa->second--;
            break;
        case '>':
            current_santa->first++;
            break;
        case '<':
            current_santa->first--;
            break;
        default:
            break;
        }

        grid[*current_santa]++;
        santa_turn = !santa_turn;
    }

    int count = 0;
    for (auto &p : grid)
    {
        if (p.second > 0)
        {
            count++;
        }
    }

    std::cout << "Part 2: " << count << std::endl;

    return EXIT_SUCCESS;
}