#include <iostream>
#include <fstream>
#include <string>
#include <array>
#include <vector>

struct Line
{
    int x, y, z;
};

bool check_triangle(int a, int b, int c)
{
    return a + b > c && a + c > b && b + c > a;
}

bool check_triangle_part1(const Line &line)
{
    return check_triangle(line.x, line.y, line.z);
}

int check_triangle_part2(const std::array<Line, 3> &lines)
{
    return check_triangle(lines[0].x, lines[1].x, lines[2].x) +
           check_triangle(lines[0].y, lines[1].y, lines[2].y) +
           check_triangle(lines[0].z, lines[1].z, lines[2].z);
}

int main(int argc, char **argv)
{
    if (argc < 2)
        return std::cerr << "Usage: " << argv[0] << " <input_file>\n", EXIT_FAILURE;

    std::ifstream file(argv[1]);
    if (!file)
        return std::cerr << "Error: Could not open file " << argv[1] << "\n", EXIT_FAILURE;

    std::string line;
    int part1_count = 0;
    int part2_count = 0;
    std::vector<Line> lines;

    while (std::getline(file, line))
    {
        Line l;
        sscanf(line.c_str(), "%d %d %d", &l.x, &l.y, &l.z);
        if (check_triangle_part1(l))
            part1_count++;
        lines.push_back(l);

        if (lines.size() == 3)
        {
            std::array<Line, 3> triangle_set = {lines[0], lines[1], lines[2]};
            part2_count += check_triangle_part2(triangle_set);
            lines.clear();
        }
    }

    std::cout << "Part 1: " << part1_count << std::endl;
    std::cout << "Part 2: " << part2_count << std::endl;

    file.close();

    return EXIT_SUCCESS;
}