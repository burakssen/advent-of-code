#include <iostream>
#include <fstream>
#include <string>
#include <unordered_map>
#include <vector>
#include <array>
#include <cstdint>

enum class InstructionType : uint8_t
{
    CPY,
    INC,
    DEC,
    JNZ
};

struct Instruction
{
    InstructionType type;
    int16_t x;
    int16_t y;
    bool x_is_reg;
    bool y_is_reg;
};

constexpr size_t NUM_REGISTERS = 4;
using Registers = std::array<int32_t, NUM_REGISTERS>;

std::vector<Instruction> parseInstructions(const std::string &filename)
{
    std::vector<Instruction> instructions;
    std::ifstream file(filename);
    if (!file)
    {
        std::cerr << "Error: Could not open file " << filename << "\n";
        exit(EXIT_FAILURE);
    }

    instructions.reserve(1000);
    std::string op, x, y;

    auto getRegIndex = [](const std::string &s) -> int16_t
    {
        return s[0] - 'a';
    };

    while (file >> op)
    {
        Instruction inst;
        if (op == "cpy")
        {
            file >> x >> y;
            inst.type = InstructionType::CPY;
            inst.x_is_reg = (x[0] >= 'a' && x[0] <= 'd');
            inst.x = inst.x_is_reg ? getRegIndex(x) : std::stoi(x);
            inst.y = getRegIndex(y);
        }
        else if (op == "inc")
        {
            file >> x;
            inst.type = InstructionType::INC;
            inst.x = getRegIndex(x);
        }
        else if (op == "dec")
        {
            file >> x;
            inst.type = InstructionType::DEC;
            inst.x = getRegIndex(x);
        }
        else if (op == "jnz")
        {
            file >> x >> y;
            inst.type = InstructionType::JNZ;
            inst.x_is_reg = (x[0] >= 'a' && x[0] <= 'd');
            inst.x = inst.x_is_reg ? getRegIndex(x) : std::stoi(x);
            inst.y_is_reg = (y[0] >= 'a' && y[0] <= 'd');
            inst.y = inst.y_is_reg ? getRegIndex(y) : std::stoi(y);
        }
        instructions.push_back(inst);
    }
    return instructions;
}

void executeInstructions(const std::vector<Instruction> &instructions, Registers &registers)
{
    size_t pc = 0;
    const size_t size = instructions.size();

    while (pc < size)
    {
        const auto &inst = instructions[pc];
        switch (inst.type)
        {
        case InstructionType::CPY:
            registers[inst.y] = inst.x_is_reg ? registers[inst.x] : inst.x;
            ++pc;
            break;
        case InstructionType::INC:
            ++registers[inst.x];
            ++pc;
            break;
        case InstructionType::DEC:
            --registers[inst.x];
            ++pc;
            break;
        case InstructionType::JNZ:
            if (inst.x_is_reg ? registers[inst.x] != 0 : inst.x != 0)
            {
                pc += inst.y_is_reg ? registers[inst.y] : inst.y;
            }
            else
            {
                ++pc;
            }
            break;
        }
    }
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cerr << "Usage: " << argv[0] << " <input_file>\n";
        return EXIT_FAILURE;
    }

    std::vector<Instruction> instructions = parseInstructions(argv[1]);
    Registers registers = {0, 0, 0, 0};

    executeInstructions(instructions, registers);
    std::cout << "Part 1: " << registers[0] << '\n';

    registers = {0, 0, 1, 0};
    executeInstructions(instructions, registers);
    std::cout << "Part 2: " << registers[0] << '\n';

    return EXIT_SUCCESS;
}