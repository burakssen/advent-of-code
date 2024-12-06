#include <iostream>
#include <vector>
#include <fstream>

int findLastElfPart1(int elfCount)
{
    std::vector<int> nextElf(elfCount);
    for (int i = 0; i < elfCount; ++i)
    {
        nextElf[i] = (i + 1) % elfCount;
    }
    int currentElf = 0;
    int remainingElves = elfCount;
    while (remainingElves > 1)
    {
        int toSteal = nextElf[currentElf];
        nextElf[currentElf] = nextElf[toSteal];
        currentElf = nextElf[currentElf];
        --remainingElves;
    }
    return currentElf + 1; // Return the 1-based index of the last elf
}

int findLastElfPart2(int elfCount)
{
    std::vector<int> prevElf(elfCount);
    std::vector<int> nextElf(elfCount);

    // Initialize the circular linked list
    for (int i = 0; i < elfCount; ++i)
    {
        prevElf[i] = (i - 1 + elfCount) % elfCount;
        nextElf[i] = (i + 1) % elfCount;
    }

    int currentElf = 0;
    int acrossElf = elfCount / 2;
    int remainingElves = elfCount;

    while (remainingElves > 1)
    {
        // Remove the elf across the circle
        nextElf[prevElf[acrossElf]] = nextElf[acrossElf];
        prevElf[nextElf[acrossElf]] = prevElf[acrossElf];

        // Move to the next elf
        currentElf = nextElf[currentElf];

        // Update the across elf position
        if (remainingElves % 2 == 0)
        {
            acrossElf = nextElf[acrossElf];
        }
        else
        {
            acrossElf = nextElf[nextElf[acrossElf]];
        }

        --remainingElves;
    }

    return currentElf + 1; // Return the 1-based index of the last elf
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

    int elfCount;
    file >> elfCount;
    file.close();

    std::cout << "Part 1: " << findLastElfPart1(elfCount) << std::endl;
    std::cout << "Part 2: " << findLastElfPart2(elfCount) << std::endl;

    return 0;
}