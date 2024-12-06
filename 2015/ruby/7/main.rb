
$instructions = []
$wires = {}

module InstructionType
    AND = "AND"
    OR = "OR"
    NOT = "NOT"
    LSHIFT = "LSHIFT"
    RSHIFT = "RSHIFT"
    ASSIGN = "ASSIGN"
end

class Instruction
    operation = ""
    input1 = ""
    input2 = ""
    output = ""

    public def initialize(operation, output, input1, input2 = "")
        @operation = operation
        @input1 = input1
        @input2 = input2
        @output = output
    end

    public def operation
        return @operation
    end

    public def input1
        return @input1
    end

    public def input2
        return @input2
    end

    public def output
        return @output
    end
end

def eval(target)
    if $wires[target] != nil
        return $wires[target]
    end

    instruction = $instructions.find { |instruction| instruction.output == target }
    if instruction == nil
        return nil
    end

    op1 = instruction.input1.to_i.to_s == instruction.input1 ? instruction.input1.to_i : eval(instruction.input1)
    op2 = 0
    if instruction.input2 != ""
        op2 = instruction.input2.to_i.to_s == instruction.input2 ? instruction.input2.to_i : eval(instruction.input2)
    end

    if instruction.operation == InstructionType::ASSIGN
        $wires[target] = op1
    end
    if instruction.operation == InstructionType::AND
        $wires[target] = op1 & op2
    end
    if instruction.operation == InstructionType::OR
        $wires[target] = op1 | op2
    end
    if instruction.operation == InstructionType::NOT
        $wires[target] = ~op1
    end
    if instruction.operation == InstructionType::LSHIFT
        $wires[target] = op1 << op2
    end
    if instruction.operation == InstructionType::RSHIFT
        $wires[target] = op1 >> op2
    end
    return $wires[target]
end

def main()    
    # get command line arguments
    args = ARGV
    if args.length < 1
        puts "Usage: ruby main.rb <input.txt>"
        return
    end

    # read file
    file = File.open(args[0], "r")
    if !file
        puts "Failed to open file: #{args[0]}"
        return
    end

    # read file line by line
    file.each_line do |line|
        words = line.split(" ")
        if words.length == 3
            $instructions.push(Instruction.new(InstructionType::ASSIGN, words[2], words[0]))
        end
        if words.length == 4
            $instructions.push(Instruction.new(InstructionType::NOT, words[3], words[1]))
        end
        if words.length == 5
            if words[1] == "AND"
                $instructions.push(Instruction.new(InstructionType::AND, words[4], words[0], words[2]))
            end
            if words[1] == "OR"
                $instructions.push(Instruction.new(InstructionType::OR, words[4], words[0], words[2]))
            end
            if words[1] == "LSHIFT"
                $instructions.push(Instruction.new(InstructionType::LSHIFT, words[4], words[0], words[2]))
            end
            if words[1] == "RSHIFT"
                $instructions.push(Instruction.new(InstructionType::RSHIFT, words[4], words[0], words[2]))
            end
        end
    end

    a = eval("a")
    puts "Part 1: #{a}"
    $wires = {"b" => a}
    a = eval("a")
    puts "Part 2: #{a}"


end

main()