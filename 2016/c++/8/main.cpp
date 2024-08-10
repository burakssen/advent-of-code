#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>

typedef struct Screen
{
    std::vector<std::vector<bool>> pixels = std::vector<std::vector<bool>>(6, std::vector<bool>(50, false));
} Screen;

void rect(Screen &screen, int a, int b)
{
    for (int i = 0; i < b; ++i)
    {
        for (int j = 0; j < a; ++j)
        {
            screen.pixels[i][j] = true;
        }
    }
}

void rotate_row(Screen &screen, int a, int b)
{
    std::vector<bool> row = screen.pixels[a];
    for (int i = 0; i < 50; ++i)
    {
        screen.pixels[a][(i + b) % 50] = row[i];
    }
}

void rotate_column(Screen &screen, int a, int b)
{
    std::vector<bool> column(6);
    for (int i = 0; i < 6; ++i)
    {
        column[i] = screen.pixels[i][a];
    }

    for (int i = 0; i < 6; ++i)
    {
        screen.pixels[(i + b) % 6][a] = column[i];
    }
}

void apply_instruction(Screen &screen, std::string instruction)
{
    int a, b;
    if (instruction.find("rect") != std::string::npos)
    {
        sscanf(instruction.c_str(), "rect %dx%d", &a, &b);
        rect(screen, a, b);
    }
    else if (instruction.find("rotate row") != std::string::npos)
    {
        sscanf(instruction.c_str(), "rotate row y=%d by %d", &a, &b);
        rotate_row(screen, a, b);
    }
    else if (instruction.find("rotate column") != std::string::npos)
    {
        sscanf(instruction.c_str(), "rotate column x=%d by %d", &a, &b);
        rotate_column(screen, a, b);
    }
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
    Screen screen;
    while (std::getline(file, line))
    {
        apply_instruction(screen, line);
    }

    int part1_count = 0;
    for (int i = 0; i < 6; ++i)
    {
        for (int j = 0; j < 50; ++j)
        {
            if (screen.pixels[i][j])
                part1_count++;
        }
    }

    std::cout << "Part 1: " << part1_count << "\n";

    std::cout << "Part 2:\n";
    for (int i = 0; i < 6; ++i)
    {
        for (int j = 0; j < 50; ++j)
        {
            std::cout << (screen.pixels[i][j] ? '#' : ' ');
        }
        std::cout << "\n";
    }

    file.close();
    return EXIT_SUCCESS;
}
