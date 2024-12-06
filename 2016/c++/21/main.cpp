#include <iostream>
#include <fstream>
#include <string>
#include <unordered_map>
#include <sstream>
#include <algorithm>

void swap_px_py(std::string &input, int px, int py)
{
    std::swap(input[px], input[py]);
}

void swap_lx_ly(std::string &input, char x, char y)
{
    // If x and y are the same, no need to swap
    if (x == y)
        return;

    std::unordered_map<char, char> swap_map = {
        {x, y},
        {y, x}};

    // Swap all occurrences of x and y in the string
    for (char &c : input)
    {
        auto it = swap_map.find(c);
        if (it != swap_map.end())
        {
            c = it->second; // Use the swap map for replacement
        }
    }
}

void rotateL(std::string &input, int steps)
{
    steps %= input.size();
    std::rotate(input.begin(), input.begin() + steps, input.end());
}

void rotateR(std::string &input, int steps)
{
    steps %= input.size();
    std::rotate(input.rbegin(), input.rbegin() + steps, input.rend());
}

void rotatePos(std::string &input, char x)
{
    int pos = input.find(x);
    if (pos == std::string::npos)
        return;

    int steps = 1 + pos;
    if (pos >= 4)
        ++steps;

    rotateR(input, steps);
}

void reverse(std::string &input, int px, int py)
{
    std::reverse(input.begin() + px, input.begin() + py + 1);
}

void move(std::string &input, int px, int py)
{
    char c = input[px];
    input.erase(input.begin() + px);
    input.insert(input.begin() + py, c);
}

void part1(std::string &input, const std::vector<std::string> &commands)
{
    for (std::string line : commands)
    {
        std::istringstream iss(line);
        std::string command;
        iss >> command;

        if (command == "swap")
        {
            std::string type, tmp;
            char x, y;
            iss >> type >> x >> tmp >> tmp >> y;
            if (type == "letter")
                swap_lx_ly(input, x, y);
            else
                swap_px_py(input, x - '0', y - '0');
        }
        else if (command == "rotate")
        {
            std::string direction, tmp;
            iss >> direction;
            if (direction == "left")
            {
                int steps;
                iss >> steps;
                rotateL(input, steps);
            }
            else if (direction == "right")
            {
                int steps;
                iss >> steps;
                rotateR(input, steps);
            }
            else if (direction == "based")
            {
                char x;
                iss >> tmp >> tmp >> tmp >> tmp >> x;
                rotatePos(input, x);
            }
        }
        else if (command == "reverse")
        {
            std::string tmp;
            int px, py;
            iss >> tmp >> px >> tmp >> py;
            reverse(input, px, py);
        }
        else if (command == "move")
        {
            std::string tmp;
            int px, py;
            iss >> tmp >> px >> tmp >> tmp >> py;
            move(input, px, py);
        }
    }
}

std::string applyOperations(const std::string &start, const std::vector<std::string> &commands)
{
    std::string result = start;
    part1(result, commands);
    return result;
}

void part2(std::string start, std::string &input, const std::vector<std::string> &commands)
{
    std::string target = input;

    // Generate all permutations of the starting string
    do
    {
        std::string result = applyOperations(start, commands);
        if (result == target)
        {
            input = start;
            return;
        }
    } while (std::next_permutation(start.begin(), start.end()));

    // If we reach here, we haven't found a solution
    std::cerr << "No solution found\n";
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

    std::string input = "abcdefgh";
    std::string part1Str = input;
    std::string part2Str = "fbgdceah";

    std::vector<std::string> commands;

    // Read and parse the input ranges
    while (std::getline(file, line))
    {
        commands.emplace_back(line);
    }
    file.close();

    part1(part1Str, commands);
    std::cout << "Part 1: " << part1Str << "\n";

    part2(input, part2Str, commands);
    std::cout << "Part 2: " << part2Str << "\n";

    return 0;
}
