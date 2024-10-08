#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <unordered_map>
#include <algorithm>
#include <cctype>
#include <sstream>

bool isInt(const std::string &s)
{
    if (s.empty())
        return false;
    size_t start = (s[0] == '-' || s[0] == '+') ? 1 : 0;
    return std::all_of(s.begin() + start, s.end(), ::isdigit);
}

std::vector<std::string> split(const std::string &s, char delimiter)
{
    std::vector<std::string> tokens;
    std::istringstream tokenStream(s);
    std::string token;
    while (std::getline(tokenStream, token, delimiter))
        tokens.push_back(token);
    return tokens;
}

void execute(std::vector<std::vector<std::string>> &data, std::unordered_map<std::string, int> &reg)
{
    for (int i = 0; i < data.size();)
    {
        auto &d = data[i];
        if (d[0] == "cpy")
        {
            if (!isInt(d[1]) && !isInt(d[2]) && i + 3 < data.size())
            {
                auto &d1 = data[i + 1], &d2 = data[i + 2], &d3 = data[i + 3];
                if (d1[0] == "dec" && d2[0] == "inc" && d3[0] == "jnz" && d3[2] == "-2")
                {
                    reg[d[1]] *= 2;
                    reg[d[2]] = 0;
                    i += 4;
                    continue;
                }
            }
            int value = isInt(d[1]) ? std::stoi(d[1]) : reg[d[1]];
            if (!isInt(d[2]))
                reg[d[2]] = value;
        }
        else if (d[0] == "inc")
        {
            if (i + 4 < data.size())
            {
                auto &d1 = data[i + 1], &d2 = data[i + 2], &d3 = data[i + 3], &d4 = data[i + 4];
                if (d1[0] == "dec" && d2[0] == "jnz" && d2[2] == "-2" && d3[0] == "dec" && d4[0] == "jnz" && d4[2] == "-5")
                {
                    reg[d[1]] += reg[d1[1]] * reg[d3[1]];
                    reg[d1[1]] = reg[d3[1]] = 0;
                    i += 5;
                    continue;
                }
            }
            reg[d[1]]++;
        }
        else if (d[0] == "dec")
            reg[d[1]]--;
        else if (d[0] == "tgl")
        {
            int oset = i + reg[d[1]];
            if (oset < data.size())
            {
                auto &gt = data[oset];
                gt[0] = (gt.size() == 2) ? ((gt[0] == "inc") ? "dec" : "inc") : ((gt[0] == "jnz") ? "cpy" : "jnz");
            }
        }
        else if (d[0] == "jnz")
        {
            int t = isInt(d[1]) ? std::stoi(d[1]) : reg[d[1]];
            int r = isInt(d[2]) ? std::stoi(d[2]) : reg[d[2]];
            if (t != 0)
            {
                i += r;
                continue;
            }
        }
        i++;
    }
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cerr << "Usage: " << argv[0] << " input.txt" << std::endl;
        return 1;
    }

    std::ifstream file(argv[1]);
    if (!file.is_open())
    {
        std::cerr << "Failed to open file: " << argv[1] << std::endl;
        return 1;
    }

    std::vector<std::vector<std::string>> data;
    std::string line;
    while (std::getline(file, line))
        data.push_back(split(line, ' '));

    auto run = [&](int init)
    {
        auto data_copy = data; // Copy the original data vector for isolation
        std::unordered_map<std::string, int> reg = {{"a", init}, {"b", 0}, {"c", 0}, {"d", 0}};
        execute(data_copy, reg);
        return reg["a"];
    };

    std::cout << "Part 1: " << run(7) << std::endl;
    std::cout << "Part 2: " << run(12) << std::endl;

    return 0;
}
