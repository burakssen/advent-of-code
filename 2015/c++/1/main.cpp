#include <iostream>
#include <fstream>

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cout << "Usage: " << argv[0] << " <input_file>" << std::endl;
        return EXIT_FAILURE;
    }

    std::string file_name = argv[1];

    std::ifstream file(file_name);

    if (!file.is_open())
    {
        std::cout << "Error: Could not open file " << file_name << std::endl;
        return EXIT_FAILURE;
    }

    std::string buffer;
    int floor = 0;
    int basement = 0;
    int count = 0;
    // Read the file
    while (std::getline(file, buffer))
    {
        for (int i = 0; i < buffer.size(); i++)
        {
            if (buffer[i] == '(')
            {
                floor++;
            }
            else if (buffer[i] == ')')
            {
                floor--;
            }

            count++;
            if (floor == -1 && basement == 0)
            {
                basement = count;
            }
        }
    }

    std::cout << "Part 1: " << floor << "\n";
    std::cout << "Part 2: " << basement << "\n";
    return EXIT_SUCCESS;
}