function main(...)
    -- get command line arguments
    if #arg < 1 then
        print("Usage: lua main.lua <input_file>")
        return 1
    end

    -- read file
    local file_name = arg[1]
    local file = io.open(file_name, "r")
    if not file then
        print("Error: could not open file " .. file_name)
        return 1
    end

    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()

    local count = 0
    -- loop characters for each line
    for i, line in ipairs(lines) do
        for c in line:gmatch(".") do
            if c == "(" then
                count = count + 1
            elseif c == ")" then
                count = count - 1
            end
        end
    end

    print(count)



    return 0
end

main()
