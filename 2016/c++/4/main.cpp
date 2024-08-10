#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>

int calculateSectorID(const std::string &line)
{
    size_t lastDash = line.find_last_of('-');
    size_t openBracket = line.find('[');
    return std::stoi(line.substr(lastDash + 1, openBracket - lastDash - 1));
}

bool isValidRoom(const std::string &encrypted_name, const std::string &checksum)
{
    std::map<char, int> charCount;
    for (char c : encrypted_name)
    {
        if (c != '-')
        {
            charCount[c]++;
        }
    }

    std::vector<std::pair<char, int>> sortedChars(charCount.begin(), charCount.end());
    std::sort(sortedChars.begin(), sortedChars.end(),
              [](const auto &a, const auto &b)
              {
                  return a.second > b.second || (a.second == b.second && a.first < b.first);
              });

    std::string calculatedChecksum;
    for (int i = 0; i < 5 && i < sortedChars.size(); ++i)
    {
        calculatedChecksum += sortedChars[i].first;
    }

    return calculatedChecksum == checksum;
}

std::string decryptRoomName(const std::string &encrypted_name, int sector_id)
{
    std::string decrypted_name;
    for (char c : encrypted_name)
    {
        if (c == '-')
        {
            decrypted_name += ' ';
        }
        else
        {
            decrypted_name += (c - 'a' + sector_id) % 26 + 'a';
        }
    }
    return decrypted_name;
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
    int part1_sum = 0;
    int part2_sector_id = -1;

    while (std::getline(file, line))
    {
        std::string encrypted_name = line.substr(0, line.find_last_of('-'));
        std::string checksum = line.substr(line.find_last_of('[') + 1);
        checksum.pop_back(); // Remove the closing bracket

        int sector_id = calculateSectorID(line);

        if (isValidRoom(encrypted_name, checksum))
        {
            part1_sum += sector_id;

            // Part 2: Decrypt room name and check for "northpole object"
            std::string decrypted_name = decryptRoomName(encrypted_name, sector_id);
            if (decrypted_name.find("northpole object") != std::string::npos)
            {
                part2_sector_id = sector_id;
            }
        }
    }

    std::cout << "Part 1: " << part1_sum << std::endl;
    if (part2_sector_id != -1)
    {
        std::cout << "Part 2: " << part2_sector_id << std::endl;
    }
    else
    {
        std::cout << "Part 2: No Object Found" << std::endl;
    }

    file.close();
    return EXIT_SUCCESS;
}