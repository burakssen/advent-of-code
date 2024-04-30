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


    return EXIT_SUCCESS;
}