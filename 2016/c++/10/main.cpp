#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <map>
#include <vector>
#include <algorithm>

struct Bot
{
    std::vector<int> chips;
    int low_to_bot = -1;
    int high_to_bot = -1;
    int low_to_output = -1;
    int high_to_output = -1;
};

std::vector<std::string> readInstructions(const std::string &filename)
{
    std::ifstream file(filename);
    if (!file)
    {
        throw std::runtime_error("Error: Could not open file " + filename);
    }

    std::vector<std::string> instructions;
    std::string line;
    while (std::getline(file, line))
    {
        instructions.push_back(line);
    }
    return instructions;
}

void processValueInstructions(const std::vector<std::string> &instructions, std::map<int, Bot> &bots)
{
    for (const auto &instr : instructions)
    {
        if (instr.substr(0, 5) == "value")
        {
            int value, bot;
            sscanf(instr.c_str(), "value %d goes to bot %d", &value, &bot);
            bots[bot].chips.push_back(value);
        }
    }
}

void processBotInstructions(const std::vector<std::string> &instructions, std::map<int, Bot> &bots)
{
    for (const auto &instr : instructions)
    {
        if (instr.substr(0, 3) == "bot")
        {
            int bot, low, high;
            char low_type[7], high_type[7];
            sscanf(instr.c_str(), "bot %d gives low to %s %d and high to %s %d",
                   &bot, low_type, &low, high_type, &high);

            if (std::string(low_type) == "bot")
            {
                bots[bot].low_to_bot = low;
            }
            else
            {
                bots[bot].low_to_output = low;
            }

            if (std::string(high_type) == "bot")
            {
                bots[bot].high_to_bot = high;
            }
            else
            {
                bots[bot].high_to_output = high;
            }
        }
    }
}

int simulateBots(std::map<int, Bot> &bots, std::map<int, int> &outputs)
{
    int target_bot = -1;
    bool changed = true;
    while (changed)
    {
        changed = false;
        for (auto &[bot_id, bot] : bots)
        {
            if (bot.chips.size() == 2)
            {
                changed = true;
                std::sort(bot.chips.begin(), bot.chips.end());

                if (bot.chips[0] == 17 && bot.chips[1] == 61)
                {
                    target_bot = bot_id;
                }

                if (bot.low_to_bot != -1)
                {
                    bots[bot.low_to_bot].chips.push_back(bot.chips[0]);
                }
                else if (bot.low_to_output != -1)
                {
                    outputs[bot.low_to_output] = bot.chips[0];
                }

                if (bot.high_to_bot != -1)
                {
                    bots[bot.high_to_bot].chips.push_back(bot.chips[1]);
                }
                else if (bot.high_to_output != -1)
                {
                    outputs[bot.high_to_output] = bot.chips[1];
                }

                bot.chips.clear();
            }
        }
    }
    return target_bot;
}

long long calculateOutputProduct(const std::map<int, int> &outputs)
{
    long long product = 1;
    for (int i = 0; i <= 2; ++i)
    {
        if (outputs.find(i) == outputs.end())
        {
            throw std::runtime_error("Error: Output " + std::to_string(i) + " not found!");
        }
        product *= outputs.at(i);
    }
    return product;
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cerr << "Usage: " << argv[0] << " <input_file>\n";
        return EXIT_FAILURE;
    }

    try
    {
        std::vector<std::string> instructions = readInstructions(argv[1]);

        std::map<int, Bot> bots;
        std::map<int, int> outputs;

        processValueInstructions(instructions, bots);
        processBotInstructions(instructions, bots);

        int target_bot = simulateBots(bots, outputs);
        long long output_product = calculateOutputProduct(outputs);

        std::cout << "Part 1: " << target_bot << std::endl;
        std::cout << "Part 2: " << output_product << std::endl;
    }
    catch (const std::exception &e)
    {
        std::cerr << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}