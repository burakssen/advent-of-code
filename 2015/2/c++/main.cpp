#include <iostream>
#include <string>
#include <fstream>

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

    int total = 0;
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

        area += smallest;
        total += area;
    }

    std::cout << total << std::endl;

    file.close();

    return EXIT_SUCCESS;
}