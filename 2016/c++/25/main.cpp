#include <iostream>
#include <vector>
#include <string>
#include <array>
#include <fstream>
#include <sstream>
#include <algorithm>

constexpr int MAX_OUTPUTS = 100;
constexpr int NUM_REGISTERS = 4;

// Optimized split function using string_view
std::vector<std::string_view> split(std::string_view s, char delimiter)
{
    std::vector<std::string_view> tokens;
    size_t start = 0;
    size_t end = s.find(delimiter);
    while (end != std::string_view::npos)
    {
        tokens.emplace_back(s.substr(start, end - start));
        start = end + 1;
        end = s.find(delimiter, start);
    }
    tokens.emplace_back(s.substr(start));
    return tokens;
}

// Enum for instruction types
enum class InstructionType
{
    CPY,
    INC,
    DEC,
    JNZ,
    OUT
};

// Struct to represent an instruction
struct Instruction
{
    InstructionType type;
    int arg1;
    int arg2;
    bool arg1_is_reg;
    bool arg2_is_reg;
};

// Function to parse instructions
std::vector<Instruction> parse_instructions(const std::vector<std::string> &code)
{
    std::vector<Instruction> instructions;
    instructions.reserve(code.size());

    for (const auto &line : code)
    {
        auto tokens = split(line, ' ');
        Instruction instr;

        if (tokens[0] == "cpy")
        {
            instr.type = InstructionType::CPY;
        }
        else if (tokens[0] == "inc")
        {
            instr.type = InstructionType::INC;
        }
        else if (tokens[0] == "dec")
        {
            instr.type = InstructionType::DEC;
        }
        else if (tokens[0] == "jnz")
        {
            instr.type = InstructionType::JNZ;
        }
        else if (tokens[0] == "out")
        {
            instr.type = InstructionType::OUT;
        }

        instr.arg1_is_reg = (tokens[1][0] >= 'a' && tokens[1][0] <= 'd');
        instr.arg1 = instr.arg1_is_reg ? tokens[1][0] - 'a' : std::stoi(std::string(tokens[1]));

        if (tokens.size() > 2)
        {
            instr.arg2_is_reg = (tokens[2][0] >= 'a' && tokens[2][0] <= 'd');
            instr.arg2 = instr.arg2_is_reg ? tokens[2][0] - 'a' : std::stoi(std::string(tokens[2]));
        }

        instructions.push_back(instr);
    }

    return instructions;
}

// Function to simulate the Assembunny code and produce the clock signal
bool simulate(const std::vector<Instruction> &instructions, int a)
{
    std::array<int, NUM_REGISTERS> registers = {a, 0, 0, 0};
    int pc = 0;
    int last_output = -1;
    int count = 0;

    auto getValue = [&](int arg, bool is_reg)
    {
        return is_reg ? registers[arg] : arg;
    };

    while (pc < static_cast<int>(instructions.size()))
    {
        const auto &instr = instructions[pc];
        switch (instr.type)
        {
        case InstructionType::CPY:
            if (instr.arg2_is_reg)
            {
                registers[instr.arg2] = getValue(instr.arg1, instr.arg1_is_reg);
            }
            ++pc;
            break;
        case InstructionType::INC:
            if (instr.arg1_is_reg)
            {
                ++registers[instr.arg1];
            }
            ++pc;
            break;
        case InstructionType::DEC:
            if (instr.arg1_is_reg)
            {
                --registers[instr.arg1];
            }
            ++pc;
            break;
        case InstructionType::JNZ:
            pc += (getValue(instr.arg1, instr.arg1_is_reg) != 0) ? getValue(instr.arg2, instr.arg2_is_reg) : 1;
            break;
        case InstructionType::OUT:
            int value = getValue(instr.arg1, instr.arg1_is_reg);
            if (last_output != -1 && value == last_output)
            {
                return false;
            }
            last_output = value;
            if (++count > MAX_OUTPUTS)
            {
                return true;
            }
            ++pc;
            break;
        }
    }
    return false;
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

    std::vector<std::string> code;
    std::string line;
    while (std::getline(file, line))
    {
        code.push_back(line);
    }

    auto instructions = parse_instructions(code);

    int a = 0;
    while (!simulate(instructions, a))
    {
        ++a;
    }

    std::cout << "Part 1: " << a << std::endl;
    return 0;
}