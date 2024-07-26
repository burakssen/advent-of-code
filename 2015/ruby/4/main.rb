#!/usr/bin/env ruby

require 'digest'

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

    prefix = file.readlines
    file.close

    leading_5_found = false
    i = 0
    while true
        input = prefix[0].strip + i.to_s
        hash = Digest::MD5.hexdigest(input)

        if hash[0..5] == "000000"
            puts "Part 2: #{i}"
            break
        end

        if hash[0..4] == "00000" && !leading_5_found
            puts "Part 1: #{i}"
            leading_5_found = true
        end

        i += 1
    end

end

main()