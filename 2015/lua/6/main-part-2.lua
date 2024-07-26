local function get_action(line)
    local words = {}

    for word in line:gmatch("%w+") do
        table.insert(words, word)
    end

    if words[1] == "turn" then
        local action = words[2]
        local start_x = words[3]
        local start_y = words[4]
        local end_x = words[6]
        local end_y = words[7]
        return action, start_x, start_y, end_x, end_y
    elseif words[1] == "toggle" then
        local action = words[1]
        local start_x = words[2]
        local start_y = words[3]
        local end_x = words[5]
        local end_y = words[6]
        return action, start_x, start_y, end_x, end_y
    else
        print("Error")
    end
end

local function main()
    if #arg < 1 then
        print("Usage: lua main-part1.lua <input.txt>")
        os.exit(1)
    end

    local filename = arg[1]

    local file = io.open(filename, "r")

    if file == nil then
        print("Error: could not open file")
        os.exit(1)
    end

    local data = file:read("*all")

    local grid = {}
    for x = 0, 999 do
        grid[x] = {}
        for y = 0, 999 do
            grid[x][y] = 0
        end
    end


    for line in data:gmatch("[^\n]+") do
        local action, startx, starty, endx, endy = get_action(line)

        for x = startx, endx do
            for y = starty, endy do
                if action == "on" then
                    grid[x][y] = grid[x][y] + 1
                elseif action == "off" then
                    grid[x][y] = math.max(grid[x][y] - 1, 0)
                elseif action == "toggle" then
                    grid[x][y] = grid[x][y] + 2
                end
            end
        end
    end

    local count = 0
    for x = 0, 999 do
        for y = 0, 999 do
            count = count + grid[x][y]
        end
    end

    print("Part 2: " .. count)
end

main()
