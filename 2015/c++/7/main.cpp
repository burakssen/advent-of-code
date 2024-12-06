#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <map>
#include <sstream>

enum class InstructionType
{
    AND,
    OR,
    LSHIFT,
    RSHIFT,
    NOT,
    ASSIGN
};

typedef struct Instruction
{
    InstructionType type;
    std::string input1;
    std::string input2;
    std::string output;
} Instruction;

std::vector<Instruction> instructions;
std::map<std::string, uint16_t> wires;

void InsertInstruction(std::string line);
int evaluate(std::string wire);

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        std::cerr << "Usage: " << argv[0] << " <input.txt>" << std::endl;
        return EXIT_FAILURE;
    }

    std::string filename = argv[1];
    std::fstream file(filename, std::ios::in);

    if (!file.is_open())
    {
        std::cerr << "Failed to open file: " << filename << std::endl;
        return EXIT_FAILURE;
    }

    std::string line;
    while (std::getline(file, line))
    {
        InsertInstruction(line);
    }

    int a = evaluate("a");
    std::cout << "Part 1: " << a << std::endl;

    wires.clear();
    wires["b"] = a;
    a = evaluate("a");
    std::cout << "Part 2: " << a << std::endl;

    file.close();

    return EXIT_SUCCESS;
}

void InsertInstruction(std::string line)
{
    std::string temp;
    std::stringstream ss(line);

    Instruction instruction;
    if (line.find("AND") != std::string::npos)
    {
        ss >> instruction.input1 >> temp >> instruction.input2 >> temp >> instruction.output;
        instruction.type = InstructionType::AND;
    }
    else if (line.find("OR") != std::string::npos)
    {
        ss >> instruction.input1 >> temp >> instruction.input2 >> temp >> instruction.output;
        instruction.type = InstructionType::OR;
    }
    else if (line.find("LSHIFT") != std::string::npos)
    {
        ss >> instruction.input1 >> temp >> instruction.input2 >> temp >> instruction.output;
        instruction.type = InstructionType::LSHIFT;
    }
    else if (line.find("RSHIFT") != std::string::npos)
    {
        ss >> instruction.input1 >> temp >> instruction.input2 >> temp >> instruction.output;
        instruction.type = InstructionType::RSHIFT;
    }
    else if (line.find("NOT") != std::string::npos)
    {
        ss >> temp >> instruction.input1 >> temp >> instruction.output;
        instruction.type = InstructionType::NOT;
    }
    else
    {
        ss >> instruction.input1 >> temp >> instruction.output;
        instruction.type = InstructionType::ASSIGN;
    }

    instructions.push_back(instruction);
}

int evaluate(std::string wire)
{
    if (wires.find(wire) != wires.end())
    {
        return wires[wire];
    }

    Instruction instruction;

    // find the instruction.output == wire
    for (auto &i : instructions)
    {
        if (i.output == wire)
        {
            instruction = i;
            break;
        }
    }

    int input1 = 0;
    int input2 = 0;

    if (instruction.input1 != "")
    {
        if (std::isdigit(instruction.input1[0]) || instruction.input1[0] == '-')
            input1 = std::stoi(instruction.input1);
        else
            input1 = evaluate(instruction.input1);
    }

    if (instruction.input2 != "")
    {
        if (std::isdigit(instruction.input2[0]) || instruction.input2[0] == '-')
            input2 = std::stoi(instruction.input2);
        else
            input2 = evaluate(instruction.input2);
    }

    switch (instruction.type)
    {
    case InstructionType::AND:
        return wires[wire] = input1 & input2;
        break;
    case InstructionType::OR:
        return wires[wire] = input1 | input2;
        break;
    case InstructionType::LSHIFT:
        return wires[wire] = input1 << input2;
        break;
    case InstructionType::RSHIFT:
        return wires[wire] = input1 >> input2;
        break;
    case InstructionType::NOT:
        return wires[wire] = ~input1;
        break;
    case InstructionType::ASSIGN:
        return wires[wire] = input1;
        break;
    }
}