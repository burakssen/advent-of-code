#!/usr/bin/env ruby

class Position 

    attr_accessor :x, :y

    def initialize(x, y)
        self.x = x
        self.y = y
    end

    def move(direction)
        case direction
        when '^'
            self.y += 1
        when 'v'
            self.y -= 1
        when '>'
            self.x += 1
        when '<'
            self.x -= 1
        end
    end
end

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

    santa = Position.new(0, 0)
    grid = Hash.new(0)
    grid[[0, 0]] = 1

    lines.each do |line|
        line.each_char do |char|
            santa.move(char)
            grid[[santa.x, santa.y]] += 1
        end
    end

    puts "Part 1: #{grid.length}"
end

main()