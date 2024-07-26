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

    paper_size = 0
    ribbon_length = 0

    lines.each do |line|
        dimensions = line.split('x').map(&:to_i)
        dimensions.sort!
        l, w, h = dimensions

        paper_size += 2*l*w + 2*w*h + 2*h*l + l*w
        ribbon_length += 2*l + 2*w + l*w*h
    end

    puts "Part 1: #{paper_size}" 
    puts "Part 2: #{ribbon_length}"
end 

main()