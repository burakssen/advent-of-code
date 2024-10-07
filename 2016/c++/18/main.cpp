#include <iostream>
#include <string>
#include <fstream>

// Function to count safe tiles for a given number of rows
int count_safe_tiles(const std::string &initial_row, int total_rows)
{
    std::string current_row = initial_row;
    int safe_tile_count = 0;

    // Count safe tiles in the initial row
    for (char tile : current_row)
    {
        if (tile == '.')
        {
            safe_tile_count++;
        }
    }

    // Generate each row based on the previous row
    for (int row = 1; row < total_rows; ++row)
    {
        std::string next_row = current_row;

        for (int i = 0; i < current_row.size(); ++i)
        {
            char left = (i > 0) ? current_row[i - 1] : '.';
            char center = current_row[i];
            char right = (i < current_row.size() - 1) ? current_row[i + 1] : '.';

            // Determine if the next tile is a trap or safe
            if ((left == '^' && center == '^' && right == '.') ||
                (center == '^' && right == '^' && left == '.') ||
                (left == '^' && center == '.' && right == '.') ||
                (right == '^' && center == '.' && left == '.'))
            {
                next_row[i] = '^';
            }
            else
            {
                next_row[i] = '.';
                safe_tile_count++;
            }
        }

        current_row = next_row;
    }

    return safe_tile_count;
}

int part1(const std::string &initial_row)
{
    return count_safe_tiles(initial_row, 40);
}

int part2(const std::string &initial_row)
{
    return count_safe_tiles(initial_row, 400000);
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

    std::string initial_row;
    std::getline(file, initial_row);
    file.close();

    int result_part1 = part1(initial_row);
    std::cout << "Part 1: " << result_part1 << "\n";

    int result_part2 = part2(initial_row);
    std::cout << "Part 2: " << result_part2 << "\n";

    return 0;
}
