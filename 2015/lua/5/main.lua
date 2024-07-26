function check_good_string_part1(str)
    local vowels = 0
    local double = false
    local bad = false

    local last = ""
    for i = 1, #str do
        local c = str:sub(i, i)
        if c == "a" or c == "e" or c == "i" or c == "o" or c == "u" then
            vowels = vowels + 1
        end

        if c == last then
            double = true
        end

        if last == "a" and c == "b" then
            bad = true
        end
        if last == "c" and c == "d" then
            bad = true
        end
        if last == "p" and c == "q" then
            bad = true
        end
        if last == "x" and c == "y" then
            bad = true
        end

        last = c
    end

    return vowels >= 3 and double and not bad
end

function check_good_string_part2(str)
    local double = false
    local repeat_ = false

    for i = 1, #str - 1 do
        local pair = str:sub(i, i + 1)
        if str:find(pair, i + 2) then
            repeat_ = true
        end

        if str:sub(i, i) == str:sub(i + 2, i + 2) then
            double = true
        end
    end

    return double and repeat_
end

function main()
    -- get command line arguments
    if #arg < 1 then
        print("Usage: lua main.lua <input.txt>")
        return
    end

    -- read input file
    local input = arg[1]
    local file = io.open(input, "r")
    if not file then
        print("Error: could not open file " .. input)
        return
    end

    -- read file line by line
    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end

    local part1_count = 0
    local part2_count = 0

    for i, line in ipairs(lines) do
        if check_good_string_part1(line) then
            part1_count = part1_count + 1
        end

        if check_good_string_part2(line) then
            part2_count = part2_count + 1
        end
    end

    print("Part 1: " .. part1_count)
    print("Part 2: " .. part2_count)
end

main()
