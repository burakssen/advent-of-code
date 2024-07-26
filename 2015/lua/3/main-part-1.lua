local Position = {
    x = 0,
    y = 0
}

function move(Position, direction)
    if direction == "^" then
        Position.y = Position.y + 1
    elseif direction == "v" then
        Position.y = Position.y - 1
    elseif direction == "<" then
        Position.x = Position.x - 1
    elseif direction == ">" then
        Position.x = Position.x + 1
    end
end

function main()
    if #arg < 1 then
        print("Usage: lua main.lua <input_file>")
        os.exit(1)
    end

    local filename = arg[1]
    local file = io.open(filename, "r")
    if (file == nil) then
        print("Error: Could not open file " .. filename)
        os.exit(1)
    end

    local content = file:read("*all")
    file:close()

    -- hash table to store the number of houses visited
    local houses = {}
    local key = Position.x .. "," .. Position.y
    houses[key] = 1

    -- loop each line
    for line in content:gmatch("[^\r\n]+") do
        -- loop each character
        for c in line:gmatch(".") do
            move(Position, c)
            key = Position.x .. "," .. Position.y
            if houses[key] == nil then
                houses[key] = 1
            else
                houses[key] = houses[key] + 1
            end
        end
    end

    local count = 0
    for k, v in pairs(houses) do
        count = count + 1
    end

    print("Part 1: " .. count)
end

main()
