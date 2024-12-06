#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <regex>
#include <map>
#include <set>
#include <algorithm>
#include <stdexcept>

using Floor = std::vector<std::vector<std::string>>;
using Building = std::vector<Floor>;

struct GameState
{
    Building floors;
    int elevator_position;
    int object_counter;

    GameState() : floors(4), elevator_position(0), object_counter(0) {}
};

class ElevatorPuzzleSolver
{
private:
    GameState state;

    bool is_floor_safe(const Floor &floor) const
    {
        if (floor.empty())
            return true;

        std::map<std::string, std::vector<std::string>> items_by_material;
        for (const auto &item : floor)
        {
            items_by_material[item[0]].push_back(item[1]);
        }

        std::set<bool> microchips_present;
        for (const auto &[material, group] : items_by_material)
        {
            if (group.size() == 1)
            {
                microchips_present.insert(std::find(group.begin(), group.end(), "microchip") != group.end());
            }
        }

        return microchips_present.size() < 2;
    }

    bool are_floors_same(const Building &f1, const Building &f2) const
    {
        for (size_t i = 0; i < f1.size(); ++i)
        {
            auto f1_sorted = f1[i];
            auto f2_sorted = f2[i];
            std::sort(f1_sorted.begin(), f1_sorted.end());
            std::sort(f2_sorted.begin(), f2_sorted.end());
            if (f1_sorted != f2_sorted)
                return false;
        }
        return true;
    }

    bool process_floor(int position, Building current_floors, const Building &previous_floors, int level = 0)
    {
        if (position == 3 && current_floors[position].size() == state.object_counter)
        {
            std::cout << level << std::endl;
            return true;
        }

        if (try_move_upstairs(position, current_floors, previous_floors, level))
            return true;
        if (try_move_downstairs(position, current_floors, previous_floors, level))
            return true;

        return false;
    }

    bool try_move_upstairs(int position, Building &current_floors, const Building &previous_floors, int level)
    {
        if (position >= 3)
            return false;

        for (size_t i = 0; i < current_floors[position].size(); ++i)
        {
            for (size_t j = i + 1; j < current_floors[position].size(); ++j)
            {
                if (try_move_items(position, position + 1, current_floors, previous_floors, {i, j}, level))
                    return true;
            }
            if (try_move_items(position, position + 1, current_floors, previous_floors, {i}, level))
                return true;
        }

        return false;
    }

    bool try_move_downstairs(int position, Building &current_floors, const Building &previous_floors, int level)
    {
        if (position <= 0)
            return false;

        for (size_t i = 0; i < current_floors[position].size(); ++i)
        {
            if (try_move_items(position, position - 1, current_floors, previous_floors, {i}, level))
                return true;
            for (size_t j = i + 1; j < current_floors[position].size(); ++j)
            {
                if (try_move_items(position, position - 1, current_floors, previous_floors, {i, j}, level))
                    return true;
            }
        }

        return false;
    }

    bool try_move_items(int from, int to, Building &floors, const Building &previous_floors,
                        const std::vector<size_t> &item_indices, int level)
    {
        auto copy_floors = floors;
        auto &from_floor = copy_floors[from];
        auto &to_floor = copy_floors[to];

        std::vector<std::vector<std::string>> items_to_move;
        for (auto index : item_indices)
        {
            items_to_move.push_back(from_floor[index]);
        }

        to_floor.insert(to_floor.end(), items_to_move.begin(), items_to_move.end());

        if (is_floor_safe(to_floor))
        {
            for (auto it = item_indices.rbegin(); it != item_indices.rend(); ++it)
            {
                from_floor.erase(from_floor.begin() + *it);
            }

            if (is_floor_safe(from_floor) && !are_floors_same(copy_floors, previous_floors))
            {
                if (process_floor(to, copy_floors, floors, level + 1))
                    return true;
            }
        }

        return false;
    }

public:
    void load_from_file(const std::string &filename)
    {
        std::ifstream file(filename);
        if (!file.is_open())
        {
            throw std::runtime_error("Failed to open file: " + filename);
        }

        std::string line;
        std::regex regex("a (.*?) (generator|microchip)");
        int floor_index = 0;

        while (std::getline(file, line))
        {
            if (line.find("nothing relevant") == std::string::npos)
            {
                std::smatch match;
                std::string::const_iterator searchStart(line.cbegin());
                while (std::regex_search(searchStart, line.cend(), match, regex))
                {
                    state.object_counter++;
                    std::string material = match[1];
                    std::string thing_type = match[2];
                    if (thing_type == "microchip")
                    {
                        size_t pos = material.find('-');
                        if (pos != std::string::npos)
                        {
                            material = material.substr(0, pos);
                        }
                    }
                    state.floors[floor_index].push_back({material, thing_type});
                    searchStart = match.suffix().first;
                }
            }
            floor_index++;
        }
    }

    void solve_part1()
    {
        std::cout << "Part 1: ";
        process_floor(state.elevator_position, state.floors, state.floors);
    }

    void solve_part2()
    {
        state.floors[0].insert(state.floors[0].end(), {{"elerium", "generator"},
                                                       {"elerium", "microchip"},
                                                       {"dilithium", "generator"},
                                                       {"dilithium", "microchip"}});
        state.object_counter += 4;

        std::cout << "Part 2: ";
        process_floor(state.elevator_position, state.floors, state.floors);
    }
};

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cerr << "Usage: " << argv[0] << " <input_file>\n";
        return EXIT_FAILURE;
    }

    try
    {
        ElevatorPuzzleSolver solver;
        solver.load_from_file(argv[1]);
        solver.solve_part1();
        solver.solve_part2();
    }
    catch (const std::exception &e)
    {
        std::cerr << "Error: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}