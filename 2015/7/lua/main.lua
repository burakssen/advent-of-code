InstructionType = {
    AND = "AND",
    OR = "OR",
    LSHIFT = "LSHIFT",
    RSHIFT = "RSHIFT",
    NOT = "NOT",
    ASSIGN = "ASSIGN"
}

Instructions = {}

Wires = {}

function get_signal(target)
    if tonumber(target) then
        return tonumber(target)
    end

    if Wires[target] then
        return Wires[target]
    end

    local instruction = nil
    for i, inst in ipairs(Instructions) do
        if inst.output == target then
            instruction = inst
            break
        end
    end

    if not instruction then
        return nil
    end

    local input1 = tonumber(instruction.input1) or get_signal(instruction.input1)
    local input2 = nil
    if instruction.input2 then
        input2 = tonumber(instruction.input2) or get_signal(instruction.input2)
    end

    local output = 0
    if instruction.type == InstructionType.AND then
        output = input1 & input2
    elseif instruction.type == InstructionType.OR then
        output = input1 | input2
    elseif instruction.type == InstructionType.LSHIFT then
        output = input1 << input2
    elseif instruction.type == InstructionType.RSHIFT then
        output = input1 >> input2
    elseif instruction.type == InstructionType.NOT then
        output = ~input1
    elseif instruction.type == InstructionType.ASSIGN then
        output = input1
    end

    Wires[target] = output
    return output
end

local function main()
    if #arg < 1 then
        print("Usage: lua main.lua <input.txt>")
        os.exit(1)
    end

    local filename = arg[1]
    local file = io.open(filename, "r")
    if not file then
        print("Error: Could not open file " .. filename)
        os.exit(1)
    end

    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end

    file:close()

    for i, line in ipairs(lines) do
        local instruction = nil
        --split line by space
        local parts = {}
        for part in string.gmatch(line, "%S+") do
            table.insert(parts, part)
        end

        if #parts == 3 then
            instruction = {
                type = InstructionType.ASSIGN,
                input1 = parts[1],
                output = parts[3]
            }
        elseif #parts == 4 then
            instruction = {
                type = InstructionType.NOT,
                input1 = parts[2],
                output = parts[4]
            }
        elseif #parts == 5 then
            if parts[2] == "AND" then
                instruction = {
                    type = InstructionType.AND,
                    input1 = parts[1],
                    input2 = parts[3],
                    output = parts[5]
                }
            elseif parts[2] == "OR" then
                instruction = {
                    type = InstructionType.OR,
                    input1 = parts[1],
                    input2 = parts[3],
                    output = parts[5]
                }
            elseif parts[2] == "LSHIFT" then
                instruction = {
                    type = InstructionType.LSHIFT,
                    input1 = parts[1],
                    input2 = parts[3],
                    output = parts[5]
                }
            elseif parts[2] == "RSHIFT" then
                instruction = {
                    type = InstructionType.RSHIFT,
                    input1 = parts[1],
                    input2 = parts[3],
                    output = parts[5]
                }
            end
        end

        table.insert(Instructions, instruction)
    end

    local a = get_signal("a")
    print("Part 1: " .. a)

    Wires = {}
    Wires["b"] = a
    a = get_signal("a")
    print("Part 2: " .. a)
end

main()
