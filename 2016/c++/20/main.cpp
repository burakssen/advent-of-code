#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>
#include <cstdint>

bool test_ip(const std::vector<std::pair<uint32_t, uint32_t>> &data, uint32_t n)
{
    for (const auto &[start, end] : data)
    {
        if (start <= n && n <= end)
        {
            return false;
        }
    }
    return true;
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
    std::vector<std::pair<uint32_t, uint32_t>> data;

    // Read and parse the input ranges
    while (std::getline(file, line))
    {
        std::istringstream iss(line);
        std::string start_str, end_str;

        std::getline(iss, start_str, '-');
        std::getline(iss, end_str);

        uint32_t start = std::stoul(start_str);
        uint32_t end = std::stoul(end_str);
        data.emplace_back(start, end);
    }
    file.close();

    // Sort data by range start
    std::sort(data.begin(), data.end());

    // Generate candidate IPs
    std::vector<uint32_t> candidates;
    for (const auto &[start, end] : data)
    {
        candidates.push_back(end + 1);
    }

    // Filter valid IPs
    std::vector<uint32_t> valids;
    for (auto c : candidates)
    {
        if (test_ip(data, c))
        {
            valids.push_back(c);
        }
    }

    // Count total valid IPs
    uint32_t total = 0;
    for (auto ip : valids)
    {
        while (test_ip(data, ip))
        {
            total++;
            ip++;
        }
    }

    if (!valids.empty())
    {
        std::cout << "Part 1: " << valids[0] << "\n";
    }
    std::cout << "Part 2: " << total << "\n";

    return 0;
}
