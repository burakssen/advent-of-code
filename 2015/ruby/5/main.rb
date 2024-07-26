#!/usr/bin/env ruby

def check_good_string_part1(str)
    vowels = 0
    double_letter = false
    bad_string = false

    str.each_char.with_index do |c, i|
        if c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'
            vowels += 1
        end

        if i > 0 && c == str[i - 1]
            double_letter = true
        end

        if i > 0 && (str[i - 1] + c) == "ab" || (str[i - 1] + c) == "cd" || (str[i - 1] + c) == "pq" || (str[i - 1] + c) == "xy"
            bad_string = true
        end
    end

    return vowels >= 3 && double_letter && !bad_string
end

def check_good_string_part2(s)
    pair = false
    repeat = false
  
    (0..s.length - 2).each do |i|
      if i < s.length - 2 && s[i] == s[i + 2]
        repeat = true
      end
  
      (i + 2..s.length - 1).each do |j|
        if s[i] == s[j] && s[i + 1] == s[j + 1]
          pair = true
        end
      end
    end
  
    pair && repeat
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

    part1_count = 0
    part2_count = 0

    lines.each do |line|
        if check_good_string_part1(line)
            part1_count += 1
        end

        if check_good_string_part2(line)
            part2_count += 1
        end
    end

    puts "Part 1: #{part1_count}"
    puts "Part 2: #{part2_count}"
end

main()