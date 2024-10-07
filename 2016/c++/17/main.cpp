#include <iostream>
#include <string>
#include <vector>
#include <queue>
#include <functional>
#include <sstream>
#include <iomanip>
#include <openssl/md5.h>
#include <fstream>

std::vector<std::function<std::pair<int, int>(int, int)>> moves = {
    [](int x, int y)
    { return std::make_pair(x, y - 1); }, // U
    [](int x, int y)
    { return std::make_pair(x, y + 1); }, // D
    [](int x, int y)
    { return std::make_pair(x - 1, y); }, // L
    [](int x, int y)
    { return std::make_pair(x + 1, y); } // R
};

std::string md5(const std::string &data)
{
    unsigned char digest[MD5_DIGEST_LENGTH];
    MD5(reinterpret_cast<const unsigned char *>(data.c_str()), data.size(), digest);

    std::ostringstream result;
    for (int i = 0; i < MD5_DIGEST_LENGTH; ++i)
    {
        result << std::hex << std::setw(2) << std::setfill('0') << static_cast<int>(digest[i]);
    }
    return result.str();
}

std::vector<bool> doors(const std::string &input, const std::string &path)
{
    std::string combined = input + path;
    std::string hash = md5(combined);
    std::vector<bool> result(4);

    for (size_t i = 0; i < 4; ++i)
    {
        result[i] = (hash[i] > 'a' && hash[i] <= 'f'); // 'a' is 10 in hex
    }
    return result;
}

std::pair<std::string, int> bfs(const std::string &input, std::pair<int, int> start, std::pair<int, int> goal)
{
    std::queue<std::tuple<std::pair<int, int>, std::vector<std::pair<int, int>>, std::string>> queue;
    queue.push(std::make_tuple(start, std::vector<std::pair<int, int>>{start}, ""));

    std::vector<std::string> paths;
    int longestPathLength = 0;

    while (!queue.empty())
    {
        auto [current, path, dirs] = queue.front();
        queue.pop();
        auto [x, y] = current;

        auto doorStatus = doors(input, dirs);
        for (size_t i = 0; i < doorStatus.size(); ++i)
        {
            if (doorStatus[i])
            { // If door is open
                auto next = moves[i](x, y);
                int nx = next.first;
                int ny = next.second;

                if (!(0 <= nx && nx < 4 && 0 <= ny && ny < 4))
                {
                    continue;
                }
                else if (next == goal)
                {
                    paths.push_back(dirs + "UDLR"[i]);
                    longestPathLength = std::max(longestPathLength, static_cast<int>(dirs.size() + 1));
                }
                else
                {
                    queue.push(std::make_tuple(next, path, dirs + "UDLR"[i]));
                }
            }
        }
    }

    if (!paths.empty())
    {
        return {paths[0], longestPathLength};
    }
    return {"", 0}; // Return empty if no path found
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
    std::getline(file, line);
    file.close();

    auto [path, len] = bfs(line, {0, 0}, {3, 3});

    std::cout << "Part 1: " << path << std::endl;
    std::cout << "Part 2: " << len << std::endl;
    return 0;
}
