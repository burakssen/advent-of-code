#include <iostream>
#include <string>
#include <fstream>
#include <algorithm>

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cout << "Usage: " << argv[0] << " <input_file>" << std::endl;
        return 1;
    }

    std::string file_name = argv[1];

    std::ifstream file(file_name);

    if (!file.is_open())
    {
        std::cout << "Error: Could not open file " << file_name << std::endl;
        return 1;
    }

    int paper_size = 0;
    int ribbon_length = 0;
    std::string line;
    while (std::getline(file, line))
    {
        int a, b, c;
        sscanf(line.c_str(), "%dx%dx%d", &a, &b, &c);

        int area = 2 * a * b + 2 * b * c + 2 * c * a;

        int smallest = a * b;
        if (b * c < smallest)
        {
            smallest = b * c;
        }
        if (c * a < smallest)
        {
            smallest = c * a;
        }

        int values[3] = {a, b, c};
        std::sort(values, values + 3);
        int small1 = values[0];
        int small2 = values[1];

        int length = 2 * small1 + 2 * small2 + a * b * c;
        ribbon_length += length;

        area += smallest;
        paper_size += area;
    }

    std::cout << "Part 1: " << paper_size << std::endl;
    std::cout << "Part 2: " << ribbon_length << std::endl;

    file.close();

    return EXIT_SUCCESS;
}