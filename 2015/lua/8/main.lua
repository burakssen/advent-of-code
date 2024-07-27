local function countChars(s, counts)
    counts.codeChars = counts.codeChars + #s

    local i = 2 -- Skip first quote
    while i < #s do
        if s:sub(i, i) == '\\' then
            if i + 1 < #s then
                local nextChar = s:sub(i + 1, i + 1)
                if nextChar == '\\' or nextChar == '"' then
                    counts.stringChars = counts.stringChars + 1
                    i = i + 1
                elseif nextChar == 'x' then
                    if i + 3 < #s then
                        counts.stringChars = counts.stringChars + 1
                        i = i + 3
                    end
                else
                    counts.stringChars = counts.stringChars + 1
                end
            end
        else
            counts.stringChars = counts.stringChars + 1
        end
        i = i + 1
    end

    return counts
end

local function encodeString(s)
    local encoded = '"'
    for i = 1, #s do
        local ch = s:sub(i, i)
        if ch == '"' or ch == '\\' then
            encoded = encoded .. '\\'
        end
        encoded = encoded .. ch
    end
    encoded = encoded .. '"'
    return { encoded = encoded, length = #encoded }
end

local function main()
    -- get command line arguments
    local args = arg
    if #args < 1 then
        print("Usage: lua main.lua <input.txt>")
        return
    end

    local filename = args[1]

    local file = io.open(filename, "r")
    if not file then
        print("File not found: " .. filename)
        return
    end

    local data = file:read("*all")
    file:close()

    if not data then
        print("File is empty: " .. filename)
        return
    end

    local lines = {}
    for line in data:gmatch("[^\r\n]+") do
        table.insert(lines, line)
    end

    local counts = { codeChars = 0, stringChars = 0 }
    local encodedLength = 0

    for _, line in ipairs(lines) do
        counts = countChars(line, counts)
        local encoded = encodeString(line)
        encodedLength = encodedLength + encoded.length
    end

    print("Part 1:", counts.codeChars - counts.stringChars)
    print("Part 2:", encodedLength - counts.codeChars)
end

main()
