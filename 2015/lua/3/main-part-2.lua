local Position = {}

function Position:new()
    o = {}
    setmetatable(o, self)
    self.__index = self
    o:_create()
    return o
end

function Position:_create()
    self.x = 0
    self.y = 0
end

function Position:move(direction)
    if direction == "^" then
        self.y = self.y + 1
    elseif direction == "v" then
        self.y = self.y - 1
    elseif direction == "<" then
        self.x = self.x - 1
    elseif direction == ">" then
        self.x = self.x + 1
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

    local santa = Position:new()
    local robo_santa = Position:new()

    local key = santa.x .. "," .. santa.y
    houses[key] = 1
    houses[robo_santa.x .. "," .. robo_santa.y] = houses[robo_santa.x .. "," .. robo_santa.y] + 1
    local santa_turn = true
    -- loop each line
    for line in content:gmatch("[^\r\n]+") do
        -- loop each character
        for c in line:gmatch(".") do
            if santa_turn then
                santa:move(c)
                key = santa.x .. "," .. santa.y
            else
                robo_santa:move(c)
                key = robo_santa.x .. "," .. robo_santa.y
            end

            if houses[key] == nil then
                houses[key] = 1
            else
                houses[key] = houses[key] + 1
            end

            santa_turn = not santa_turn
        end
    end

    local count = 0
    for k, v in pairs(houses) do
        count = count + 1
    end

    print("Part 2: " .. count)
end

main()
