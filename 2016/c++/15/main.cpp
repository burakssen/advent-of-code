#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>

struct Disc
{
    int num_positions;
    int start_position;
};

bool canCapsulePass(const std::vector<Disc> &discs, int drop_time)
{
    for (size_t i = 0; i < discs.size(); ++i)
    {
        // The time the capsule reaches the ith disc is drop_time + (i + 1)
        int time_at_disc = drop_time + (i + 1);
        int position_at_time = (discs[i].start_position + time_at_disc) % discs[i].num_positions;
        if (position_at_time != 0)
        {
            return false; // The capsule bounces
        }
    }
    return true; // The capsule passes through all discs
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

    std::vector<Disc> discs;
    std::string line;

    // Parse input
    while (std::getline(file, line))
    {
        int disc_number, num_positions, start_position;
        // Extracting the values using sscanf
        sscanf(line.c_str(), "Disc #%d has %d positions; at time=0, it is at position %d.", &disc_number, &num_positions, &start_position);

        Disc disc;
        disc.num_positions = num_positions;
        disc.start_position = start_position;
        discs.push_back(disc);
    }

    file.close();

    // Part 1: Find the first time to press the button
    for (int drop_time = 0;; ++drop_time)
    {
        if (canCapsulePass(discs, drop_time))
        {
            std::cout << "Part 1: " << drop_time << std::endl;
            break;
        }
    }

    // Part 2: Add the new disc (11 positions, starting at position 0)
    discs.emplace_back(Disc{11, 0});

    // Find the first time to press the button for Part 2
    for (int drop_time = 0;; ++drop_time)
    {
        if (canCapsulePass(discs, drop_time))
        {
            std::cout << "Part 2: " << drop_time << std::endl;
            break;
        }
    }

    return EXIT_SUCCESS;
}
