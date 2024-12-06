#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <queue>
#include <unordered_map>
#include <limits>
#include <algorithm>

const int INF = std::numeric_limits<int>::max();
const std::vector<std::pair<int, int>> directions = {{1, 0}, {-1, 0}, {0, 1}, {0, -1}};

// BFS to find shortest paths between all points of interest
int bfs(const std::vector<std::string> &map, const std::pair<int, int> &start, const std::pair<int, int> &end)
{
    int rows = map.size();
    int cols = map[0].size();
    std::queue<std::pair<int, int>> q;
    std::vector<std::vector<int>> dist(rows, std::vector<int>(cols, INF));

    q.push(start);
    dist[start.second][start.first] = 0;

    while (!q.empty())
    {
        auto [x, y] = q.front();
        q.pop();

        for (const auto &[dx, dy] : directions)
        {
            int nx = x + dx, ny = y + dy;
            if (nx >= 0 && ny >= 0 && nx < cols && ny < rows && map[ny][nx] != '#' && dist[ny][nx] == INF)
            {
                dist[ny][nx] = dist[y][x] + 1;
                q.push({nx, ny});
            }
        }
    }
    return dist[end.second][end.first];
}

// Recursive TSP solver with memoization for part one
int tsp(int pos, int visited, const std::vector<std::vector<int>> &distances, std::vector<std::vector<int>> &memo, bool returnToStart)
{
    if (visited == (1 << distances.size()) - 1)
    {
        return returnToStart ? distances[pos][0] : 0; // Return to start for part two, no return for part one
    }
    if (memo[pos][visited] != -1)
        return memo[pos][visited];

    int result = INF;
    for (int next = 0; next < distances.size(); ++next)
    {
        if (!(visited & (1 << next)))
        {
            result = std::min(result, distances[pos][next] + tsp(next, visited | (1 << next), distances, memo, returnToStart));
        }
    }
    return memo[pos][visited] = result;
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cerr << "Usage: " << argv[0] << " input.txt" << std::endl;
        return 1;
    }

    std::vector<std::string> map;
    std::unordered_map<int, std::pair<int, int>> positions;

    const std::string filename = argv[1];

    std::ifstream file(filename);

    if (!file.is_open())
    {
        std::cerr << "Failed to open file: " << filename << std::endl;
        return 1;
    }

    std::string line;
    int y = 0;

    while (std::getline(file, line))
    {
        map.push_back(line);
        for (int x = 0; x < line.size(); ++x)
        {
            if (isdigit(line[x]))
            {
                positions[line[x] - '0'] = {x, y};
            }
        }
        ++y;
    }

    file.close();

    // Prepare distances matrix
    int n = positions.size();
    std::vector<std::vector<int>> distances(n, std::vector<int>(n, INF));

    for (const auto &[i, pos1] : positions)
    {
        for (const auto &[j, pos2] : positions)
        {
            if (i != j)
            {
                distances[i][j] = bfs(map, pos1, pos2);
            }
        }
    }

    // Memoization table for TSP
    std::vector<std::vector<int>> memo(n, std::vector<int>(1 << n, -1));

    // Part One: Find minimum path length starting from point 0 without returning
    int resultPart1 = tsp(0, 1, distances, memo, false);
    std::cout << "Part 1: " << resultPart1 << std::endl;

    // Reset memo table for Part Two
    memo.assign(n, std::vector<int>(1 << n, -1));

    // Part Two: Find minimum path length starting from point 0 and returning to 0
    int resultPart2 = tsp(0, 1, distances, memo, true);
    std::cout << "Part 2: " << resultPart2 << std::endl;

    return 0;
}
