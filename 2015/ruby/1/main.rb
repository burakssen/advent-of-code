#!/usr/bin/env ruby

def main()
    args = ARGV
    if args.length < 1
        puts "Usage: ruby main.rb <input.txt>"
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

    floor = 0
    basement = 0
    count = 0
    lines.each do |line|
        line.each_char do |char|
            count += 1
            if char == '('
                floor += 1
            elsif char == ')'
                floor -= 1
            end

            if floor == -1 && basement == 0
                basement = count
            end
        end
    end

    puts "Part 1: #{floor}"
    puts "Part 2: #{basement}"
end

main()