function main()
    if (#arg < 1) then
        print("Usage: lua main.lua <input_file>")
        return
    end

    local input_file = arg[1]
    -- read input file
    local file = io.open(input_file, "r")
    if (file == nil) then
        print("Error: cannot open file " .. input_file)
        return
    end

    local input = file:read("*all")
    file:close()

    -- parse input
    local lines = {}
    for line in input:gmatch("[^\r\n]+") do
        table.insert(lines, line)
    end

    local paper_size = 0
    local ribbon_length = 0
    -- process input
    for i, line in pairs(lines) do
        -- split line by x
        local parts = {}
        for part in line:gmatch("[^x]+") do
            table.insert(parts, tonumber(part))
        end

        local l = parts[1]
        local w = parts[2]
        local h = parts[3]

        local sides = { l, w, h }
        table.sort(sides)

        -- calculate ribbon length
        ribbon_length = ribbon_length + 2 * sides[1] + 2 * sides[2] + l * w * h

        -- calculate surface area
        local area = 2 * l * w + 2 * w * h + 2 * h * l

        -- find the smallest side
        local smallest = math.min(l * w, w * h, h * l)

        -- calculate total area
        paper_size = paper_size + area + smallest
    end

    print("Part 1: " .. paper_size)
    print("Part 2: " .. ribbon_length)
end

main()
