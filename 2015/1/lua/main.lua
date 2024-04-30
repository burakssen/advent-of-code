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

end

main()
