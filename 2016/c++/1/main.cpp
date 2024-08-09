#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <cmath>

const int GRID_SIZE = 1000;

struct Position
{
    int x = 0, y = 0;
};

enum class Direction
{
    NORTH,
    EAST,
    SOUTH,
    WEST
};

void turn(Direction &dir, char turn)
{
    int d = static_cast<int>(dir) + (turn == 'R' ? 1 : -1);
    dir = static_cast<Direction>((d + 4) % 4);
}

bool move_and_check(Position &pos, Direction dir, int steps, std::vector<std::vector<bool>> &grid, Position &first_revisit)
{
    int dx = (dir == Direction::EAST) - (dir == Direction::WEST);
    int dy = (dir == Direction::NORTH) - (dir == Direction::SOUTH);

    for (int i = 0; i < steps; ++i)
    {
        pos.x += dx;
        pos.y += dy;
        int grid_x = pos.x + GRID_SIZE / 2;
        int grid_y = pos.y + GRID_SIZE / 2;

        if (grid[grid_x][grid_y])
        {
            if (first_revisit.x == 0 && first_revisit.y == 0)
                first_revisit = pos;
            return true;
        }
        else
        {
            grid[grid_x][grid_y] = true;
        }
    }
    return false;
}

int manhattan_distance(const Position &pos)
{
    return std::abs(pos.x) + std::abs(pos.y);
}

int main(int argc, char **argv)
{
    if (argc < 2)
        return std::cerr << "Usage: " << argv[0] << " <input_file>\n", EXIT_FAILURE;

    std::ifstream file(argv[1]);
    if (!file)
        return std::cerr << "Error: Could not open file " << argv[1] << "\n", EXIT_FAILURE;

    std::vector<std::string> instructions;
    std::string instruction;
    while (std::getline(file, instruction, ','))
    {
        instruction.erase(std::remove_if(instruction.begin(), instruction.end(), ::isspace), instruction.end());
        if (!instruction.empty())
            instructions.push_back(instruction);
    }

    // Part One
    Position pos1;
    Direction dir1 = Direction::NORTH;
    for (const auto &instr : instructions)
    {
        turn(dir1, instr[0]);
        pos1.x += std::stoi(instr.substr(1)) * ((dir1 == Direction::EAST) - (dir1 == Direction::WEST));
        pos1.y += std::stoi(instr.substr(1)) * ((dir1 == Direction::NORTH) - (dir1 == Direction::SOUTH));
    }
    std::cout << "Part 1: " << manhattan_distance(pos1) << '\n';

    // Part Two
    Position pos2, first_revisit;
    Direction dir2 = Direction::NORTH;
    std::vector<std::vector<bool>> grid(GRID_SIZE, std::vector<bool>(GRID_SIZE, false));
    grid[GRID_SIZE / 2][GRID_SIZE / 2] = true;

    for (const auto &instr : instructions)
    {
        turn(dir2, instr[0]);
        if (move_and_check(pos2, dir2, std::stoi(instr.substr(1)), grid, first_revisit))
            break;
    }

    if (first_revisit.x || first_revisit.y)
        std::cout << "Part 2: " << manhattan_distance(first_revisit) << '\n';
    else
        std::cout << "Part 2: No location was visited twice.\n";

    return EXIT_SUCCESS;
}
