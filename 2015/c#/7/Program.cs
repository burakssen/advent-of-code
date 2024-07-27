using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    enum InstructionType
    {
        AND,
        OR,
        LSHIFT,
        RSHIFT,
        NOT,
        ASSIGN
    }

    class Instruction
    {
        public InstructionType Type { get; set; }
        public string Input1 { get; set; }
        public string Input2 { get; set; }
        public string Output { get; set; }
    }

    static List<Instruction> instructions = new List<Instruction>();
    static Dictionary<string, ushort> wires = new Dictionary<string, ushort>();

    static void Main(string[] args)
    {
        if (args.Length < 1)
        {
            Console.Error.WriteLine($"Usage: {AppDomain.CurrentDomain.FriendlyName} <input.txt>");
            Environment.Exit(1);
        }

        string filename = args[0];
        try
        {
            string[] lines = File.ReadAllLines(filename);
            foreach (string line in lines)
            {
                InsertInstruction(line);
            }

            int a = Evaluate("a");
            Console.WriteLine($"Part 1: {a}");

            wires.Clear();
            wires["b"] = (ushort)a;
            a = Evaluate("a");
            Console.WriteLine($"Part 2: {a}");
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"An error occurred: {ex.Message}");
            Environment.Exit(1);
        }
    }

    static void InsertInstruction(string line)
    {
        string[] parts = line.Split(' ');
        Instruction instruction = new Instruction();

        if (line.Contains("AND"))
        {
            instruction.Type = InstructionType.AND;
            instruction.Input1 = parts[0];
            instruction.Input2 = parts[2];
            instruction.Output = parts[4];
        }
        else if (line.Contains("OR"))
        {
            instruction.Type = InstructionType.OR;
            instruction.Input1 = parts[0];
            instruction.Input2 = parts[2];
            instruction.Output = parts[4];
        }
        else if (line.Contains("LSHIFT"))
        {
            instruction.Type = InstructionType.LSHIFT;
            instruction.Input1 = parts[0];
            instruction.Input2 = parts[2];
            instruction.Output = parts[4];
        }
        else if (line.Contains("RSHIFT"))
        {
            instruction.Type = InstructionType.RSHIFT;
            instruction.Input1 = parts[0];
            instruction.Input2 = parts[2];
            instruction.Output = parts[4];
        }
        else if (line.Contains("NOT"))
        {
            instruction.Type = InstructionType.NOT;
            instruction.Input1 = parts[1];
            instruction.Output = parts[3];
        }
        else
        {
            instruction.Type = InstructionType.ASSIGN;
            instruction.Input1 = parts[0];
            instruction.Output = parts[2];
        }

        instructions.Add(instruction);
    }

    static int Evaluate(string wire)
    {
        if (wires.TryGetValue(wire, out ushort value))
        {
            return value;
        }

        Instruction instruction = instructions.First(i => i.Output == wire);

        int input1 = 0;
        int input2 = 0;

        if (!string.IsNullOrEmpty(instruction.Input1))
        {
            input1 = char.IsDigit(instruction.Input1[0]) || instruction.Input1[0] == '-'
                ? int.Parse(instruction.Input1)
                : Evaluate(instruction.Input1);
        }

        if (!string.IsNullOrEmpty(instruction.Input2))
        {
            input2 = char.IsDigit(instruction.Input2[0]) || instruction.Input2[0] == '-'
                ? int.Parse(instruction.Input2)
                : Evaluate(instruction.Input2);
        }

        ushort result = instruction.Type switch
        {
            InstructionType.AND => (ushort)(input1 & input2),
            InstructionType.OR => (ushort)(input1 | input2),
            InstructionType.LSHIFT => (ushort)(input1 << input2),
            InstructionType.RSHIFT => (ushort)(input1 >> input2),
            InstructionType.NOT => (ushort)(~input1),
            InstructionType.ASSIGN => (ushort)input1,
            _ => throw new InvalidOperationException($"Unknown instruction type: {instruction.Type}")
        };

        wires[wire] = result;
        return result;
    }
}