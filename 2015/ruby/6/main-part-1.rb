#!/usr/bin/env ruby

def get_action(line)
    words = line.split(" ")
    if words[0] == "turn"
        action = words[1]
        start = words[2].split(",").map { |x| x.to_i }
        end_ = words[4].split(",").map { |x| x.to_i }
        return action, start, end_
    else
        action = words[0]
        start = words[1].split(",").map { |x| x.to_i }
        end_ = words[3].split(",").map { |x| x.to_i }
        return action, start, end_
    end
end

def main
    args = ARGV

    if args.length < 1
        puts "Usage: ruby main-part1.rb <input.txt>"
        exit 1
    end

    filename = args[0]
    file = File.open(filename, "r")

    if file.nil?
        puts "Could not open file"
        exit 1
    end

    lines = file.readlines
    file.close

    grid = Array.new(1000) { Array.new(1000, false) }
    
    lines.each do |line|
        action, start, end_ = get_action(line)
        for i in start[0]..end_[0]
            for j in start[1]..end_[1]
                if action == "on"
                    grid[i][j] = true
                elsif action == "off"
                    grid[i][j] = false
                else
                    grid[i][j] = !grid[i][j]
                end
            end
        end
    end

    count = 0
    for i in 0..999
        for j in 0..999
            if grid[i][j] == true
                count += 1
            end
        end
    end

    puts "Part 1: #{count}"
end

main()