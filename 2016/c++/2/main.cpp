#include <iostream>
#include <fstream>
#include <vector>
#include <string>

typedef struct Vector2
{
    int x;
    int y;
} Vector2;

std::vector<std::vector<int>> keypad_part1 = {
    {1, 2, 3},
    {4, 5, 6},
    {7, 8, 9}};

std::vector<std::vector<char>> keypad_part2 = {
    {' ', ' ', '1', ' ', ' '},
    {' ', '2', '3', '4', ' '},
    {'5', '6', '7', '8', '9'},
    {' ', 'A', 'B', 'C', ' '},
    {' ', ' ', 'D', ' ', ' '}};

void move_part1(Vector2 &pos, char dir)
{
    switch (dir)
    {
    case 'U':
        pos.y = std::max(0, pos.y - 1);
        break;
    case 'D':
        pos.y = std::min(2, pos.y + 1);
        break;
    case 'L':
        pos.x = std::max(0, pos.x - 1);
        break;
    case 'R':
        pos.x = std::min(2, pos.x + 1);
        break;
    }
}

void move_part2(Vector2 &pos, char dir)
{
    switch (dir)
    {
    case 'U':
        if (pos.y > 0 && keypad_part2[pos.y - 1][pos.x] != ' ')
            pos.y--;
        break;
    case 'D':
        if (pos.y < 4 && keypad_part2[pos.y + 1][pos.x] != ' ')
            pos.y++;
        break;
    case 'L':
        if (pos.x > 0 && keypad_part2[pos.y][pos.x - 1] != ' ')
            pos.x--;
        break;
    case 'R':
        if (pos.x < 4 && keypad_part2[pos.y][pos.x + 1] != ' ')
            pos.x++;
        break;
    }
}

int get_key_part1(Vector2 &pos, std::string line)
{
    for (char dir : line)
        move_part1(pos, dir);

    return keypad_part1[pos.y][pos.x];
}

char get_key_part2(Vector2 &pos, std::string line)
{
    for (char dir : line)
        move_part2(pos, dir);

    return keypad_part2[pos.y][pos.x];
}

int main(int argc, char **argv)
{
    if (argc < 2)
        return std::cerr << "Usage: " << argv[0] << " <input_file>\n", EXIT_FAILURE;

    std::ifstream file(argv[1]);
    if (!file)
        return std::cerr << "Error: Could not open file " << argv[1] << "\n", EXIT_FAILURE;

    std::string line;

    int part1 = 0;
    std::string part2 = "";
    Vector2 part1_pos = {1, 1};
    Vector2 part2_pos = {2, 2};
    while (std::getline(file, line, '\n'))
    {
        part1 = part1 * 10 + get_key_part1(part1_pos, line);
        part2 += get_key_part2(part2_pos, line);
    }

    std::cout << "Part 1: " << part1 << std::endl;
    std::cout << "Part 2: " << part2 << std::endl;
    file.close();

    return EXIT_SUCCESS;
}