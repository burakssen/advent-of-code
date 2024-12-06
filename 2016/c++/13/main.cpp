#include <iostream>
#include <fstream>
#include <string>
#include <queue>
#include <vector>
#include <bitset>
#include <set>

// Custom Vector2 structure
struct Vector2
{
    int x, y;

    Vector2(int x = 0, int y = 0) : x(x), y(y) {}

    // Comparison operators
    bool operator==(const Vector2 &other) const
    {
        return x == other.x && y == other.y;
    }

    bool operator<(const Vector2 &other) const
    {
        return x < other.x || (x == other.x && y < other.y);
    }
};

const int dx[] = {1, -1, 0, 0}; // Directions for moving right, left, down, up
const int dy[] = {0, 0, 1, -1};

// Function to check if a location (x, y) is an open space based on the favorite number
bool is_open_space(int x, int y, int favorite_number)
{
    if (x < 0 || y < 0)
        return false;

    int value = x * x + 3 * x + 2 * x * y + y + y * y + favorite_number;
    std::bitset<32> bits(value);
    return bits.count() % 2 == 0;
}

// Function to perform BFS and find the shortest path from start to target
int bfs(const Vector2 &start, const Vector2 &target, int favorite_number)
{
    std::queue<Vector2> q;
    std::vector<std::vector<bool>> visited(1000, std::vector<bool>(1000, false)); // Sufficiently large grid

    q.push(start);
    visited[start.x][start.y] = true;
    int steps = 0;

    while (!q.empty())
    {
        int level_size = q.size();
        for (int i = 0; i < level_size; ++i)
        {
            Vector2 current = q.front();
            q.pop();

            if (current == target)
                return steps;

            for (int d = 0; d < 4; ++d)
            {
                int new_x = current.x + dx[d];
                int new_y = current.y + dy[d];

                if (new_x >= 0 && new_y >= 0 && !visited[new_x][new_y] && is_open_space(new_x, new_y, favorite_number))
                {
                    q.push(Vector2(new_x, new_y));
                    visited[new_x][new_y] = true;
                }
            }
        }
        ++steps;
    }

    return -1; // Target is not reachable
}

// Function to count the number of unique locations reachable within max_steps
int count_reachable_locations(const Vector2 &start, int max_steps, int favorite_number)
{
    std::queue<Vector2> q;
    std::vector<std::vector<bool>> visited(1000, std::vector<bool>(1000, false)); // Sufficiently large grid
    std::set<Vector2> unique_locations;

    q.push(start);
    visited[start.x][start.y] = true;
    int steps = 0;

    while (!q.empty() && steps <= max_steps)
    {
        int level_size = q.size();
        for (int i = 0; i < level_size; ++i)
        {
            Vector2 current = q.front();
            q.pop();

            unique_locations.insert(current);

            for (int d = 0; d < 4; ++d)
            {
                int new_x = current.x + dx[d];
                int new_y = current.y + dy[d];

                if (new_x >= 0 && new_y >= 0 && !visited[new_x][new_y] && is_open_space(new_x, new_y, favorite_number))
                {
                    q.push(Vector2(new_x, new_y));
                    visited[new_x][new_y] = true;
                }
            }
        }
        ++steps;
    }

    return unique_locations.size();
}

// Main function to read input and execute the BFS and count functions
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
    int favorite_number = std::stoi(line);

    const Vector2 start(1, 1);
    const Vector2 target(31, 39);

    int result_part1 = bfs(start, target, favorite_number);
    std::cout << "Part 1: " << result_part1 << "\n";

    const int max_steps = 50;
    int result_part2 = count_reachable_locations(start, max_steps, favorite_number);
    std::cout << "Part 2: " << result_part2 << "\n";

    return EXIT_SUCCESS;
}
