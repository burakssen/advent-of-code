#include <iostream>
#include <fstream>
#include <vector>
#include <regex>
#include <string>
#include <algorithm>

struct Node
{
    int used;
    int avail;
    int dist = std::numeric_limits<int>::max();             // Initialize distance to infinity
    std::optional<std::pair<int, int>> prev = std::nullopt; // Previous node in the path
};

int count_viable_pairs(const std::array<std::array<Node, 38>, 28> &nodes)
{
    int cnt = 0;

    std::array<Node, 1064> flat_nodes;

    for (size_t i = 0; i < nodes.size(); ++i)
    {
        for (size_t j = 0; j < nodes[i].size(); ++j)
        {
            flat_nodes[i * nodes[i].size() + j] = nodes[i][j];
        }
    }

    // Count viable pairs
    for (size_t i = 0; i < flat_nodes.size(); ++i)
    {
        for (size_t j = i + 1; j < flat_nodes.size(); ++j)
        {
            if (flat_nodes[i].used != 0 && flat_nodes[i].used <= flat_nodes[j].avail)
            {
                cnt++;
            }
            if (flat_nodes[j].used != 0 && flat_nodes[j].used <= flat_nodes[i].avail)
            {
                cnt++;
            }
        }
    }

    return cnt;
}
std::optional<std::vector<std::pair<int, int>>> find_path(
    const std::pair<int, int> &start,
    const std::pair<int, int> &end,
    std::array<std::array<Node, 38>, 28> &nodes,
    const std::optional<std::pair<int, int>> &obst = std::nullopt)
{
    int lx = nodes[0].size();
    int ly = nodes.size();

    // Reset BFS
    for (int y = 0; y < ly; ++y)
    {
        for (int x = 0; x < lx; ++x)
        {
            nodes[y][x].dist = std::numeric_limits<int>::max();
            nodes[y][x].prev = std::nullopt;
        }
    }

    // Do the actual BFS
    std::queue<std::pair<int, int>> q;
    q.push(start);
    nodes[start.second][start.first].dist = 0;

    while (!q.empty())
    {
        auto n = q.front();
        q.pop();

        std::vector<std::pair<int, int>> neighbors = {
            {n.first + 1, n.second}, {n.first - 1, n.second}, {n.first, n.second + 1}, {n.first, n.second - 1}};

        for (const auto &[x, y] : neighbors)
        {
            if (0 <= x && x < lx && 0 <= y && y < ly &&
                nodes[y][x].used < 100 &&
                (obst == std::nullopt || std::make_pair(x, y) != obst.value()))
            {
                if (nodes[y][x].dist > nodes[n.second][n.first].dist + 1)
                {
                    nodes[y][x].dist = nodes[n.second][n.first].dist + 1;
                    nodes[y][x].prev = n;
                    q.push({x, y});
                }
            }
        }

        if (std::make_pair(n.first, n.second) == end)
        {
            std::vector<std::pair<int, int>> path;
            auto current = end;
            while (nodes[current.second][current.first].prev.has_value())
            {
                path.push_back(current);
                current = nodes[current.second][current.first].prev.value();
            }
            std::reverse(path.begin(), path.end());
            return path; // Start node is already excluded
        }
    }

    return std::nullopt; // No path found
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cerr << "Usage: " << argv[0] << " <input_file>\n";
        return 1;
    }

    std::string filename = argv[1];
    std::ifstream file(filename);

    if (!file)
    {
        std::cerr << "Error: Could not open file " << filename << "\n";
        return 1;
    }

    std::array<std::array<Node, 38>, 28> nodes;
    std::string line;
    std::regex re("\\d+");

    while (std::getline(file, line))
    {
        int x, y, size, used, avail, use;
        sscanf(line.c_str(), "/dev/grid/node-x%d-y%d %dT %dT %dT %d%%", &x, &y, &size, &used, &avail, &use);
        nodes[y][x] = {used, avail};
    }

    file.close();

    std::cout << "Part 1: " << count_viable_pairs(nodes) << "\n";

    std::pair<int, int> start = {0, 0};
    std::pair<int, int> goal = {nodes[0].size() - 1, 0};
    std::optional<std::pair<int, int>> empty = std::nullopt;

    // Find the first empty node
    for (int y = 0; y < nodes.size(); ++y)
    {
        for (int x = 0; x < nodes[0].size(); ++x)
        {
            if (nodes[y][x].used == 0)
            {
                empty = {x, y};
                break;
            }
        }
        if (empty.has_value())
        {
            break;
        }
    }

    auto path = find_path(goal, start, nodes, empty);
    if (!path.has_value())
    {
        std::cout << "No path found\n";
        return 1;
    }
    int count = 0;

    // 5. Repeat until G = S
    while (goal != start)
    {
        if (path.value().empty())
        {
            std::cout << "No path found\n";
            break;
        }

        auto p = find_path(empty.value(), path.value().front(), nodes, goal);

        if (!p.has_value())
        {
            std::cout << "No path found\n";
            break;
        }

        if (p.value().empty())
        {
            std::cout << "No path found\n";
            break;
        }

        count += p.value().size() + 1;
        empty = goal;
        goal = p.value().back();
        path.value().erase(path.value().begin());
    }

    std::cout << "Part 2: " << count << std::endl;

    return 0;
}
