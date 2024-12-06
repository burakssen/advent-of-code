import Foundation


func check_good_string_part1(s: String) -> Bool {
    var vowels = 0
    var double = false
    var bad = false
    var last = Character(" ")

    for c in s {
        if c == "a" || c == "e" || c == "i" || c == "o" || c == "u" {
            vowels += 1
        }
        if c == last {
            double = true
        }
        if (last == "a" && c == "b") || (last == "c" && c == "d") || (last == "p" && c == "q") || (last == "x" && c == "y") {
            bad = true
        }
        last = c
    }

    return vowels >= 3 && double && !bad
}

func check_good_string_part2(s: String) -> Bool {
    var double = false
    var repeat_ = false
    var last = Character(" ")
    var last_last = Character(" ")

    for i in 0..<s.count {
        let c = s[s.index(s.startIndex, offsetBy: i)]
        if c == last_last {
            repeat_ = true
        }
        if i > 0 {
            let pair = String(last) + String(c)
            if s.range(of: pair, options: .literal, range: s.index(s.startIndex, offsetBy: i+1)..<s.endIndex, locale: nil) != nil {
                double = true
            }
        }
        last_last = last
        last = c
    }

    return double && repeat_
}

func main(){
    let args = CommandLine.arguments

    if args.count < 2 {
        print("Usage: swift main.swift <input.txt>")
        return
    }
    let filename = args[1]
    let file = FileHandle(forReadingAtPath: filename)
    if file == nil {
        print("Cannot open file")
        return
    }

    let data = file!.readDataToEndOfFile()
    let str = String(data: data, encoding: .utf8)
    let lines = str!.split(separator: "\n")

    var part1_count = 0
    var part2_count = 0

    for line in lines {
        if check_good_string_part1(s: String(line)) {
            part1_count += 1
        }

        if check_good_string_part2(s: String(line)) {
            part2_count += 1
        }
    }

    print("Part 1: \(part1_count)")
    print("Part 2: \(part2_count)")
}

main()