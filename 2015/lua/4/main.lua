local md5 = require 'md5'
function main()
    if #arg < 1 then
        print("Usage: lua main.lua <input.txt>")
        os.exit(1)
    end

    local filename = arg[1]

    local file = io.open(filename, "r")

    if not file then
        print("Could not open file: " .. filename)
        os.exit(1)
    end

    local lines = {}

    for line in file:lines() do
        table.insert(lines, line)
    end

    file:close()

    local prefix = lines[1]

    local i = 0

    local leading_5_found = false

    while true do
        local hash = md5.sumhexa(prefix .. i)
        -- check if the hash starts with 5 zeros
        if hash:sub(1, 5) == "00000" and not leading_5_found then
            print("Part 1: " .. i)
            leading_5_found = true
        end

        if hash:sub(1, 6) == "000000" then
            print("Part 2: " .. i)
            break
        end

        i = i + 1
    end
end

main()
