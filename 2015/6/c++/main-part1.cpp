#include <iostream>
#include <fstream>
#include <string>
#include <sstream>

void get_action(std::string line, std::string &action, int &startx, int &starty, int &endx, int &endy)
{
    std::string temp;
    char comma;
    std::stringstream ss(line);
    if (line[0] == 't' && line[1] == 'u')
        ss >> temp >> action >> startx >> comma >> starty >> temp >> endx >> comma >> endy;
    else
        ss >> action >> startx >> comma >> starty >> temp >> endx >> comma >> endy;
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cout << "Usage: " << argv[0] << " <input.txt>" << std::endl;
        return EXIT_FAILURE;
    }

    std::string filename = argv[1];
    std::fstream file(filename, std::ios::in);

    if (!file.is_open())
    {
        std::cout << "Error: Unable to open file " << filename << std::endl;
        return EXIT_FAILURE;
    }

    std::string line;
    bool lights[1000][1000] = {false};

    while (std::getline(file, line))
    {
        std::string action;
        int startx, starty, endx, endy;

        get_action(line, action, startx, starty, endx, endy);

        for (int i = startx; i <= endx; i++)
        {
            for (int j = starty; j <= endy; j++)
            {
                if (action == "on")
                    lights[i][j] = true;
                else if (action == "off")
                    lights[i][j] = false;
                else
                    lights[i][j] = !lights[i][j];
            }
        }
    }

    file.close();

    int count = 0;
    for (int i = 0; i < 1000; i++)
    {
        for (int j = 0; j < 1000; j++)
        {
            if (lights[i][j])
                count++;
        }
    }

    std::cout << count << std::endl;

    return EXIT_SUCCESS;
}