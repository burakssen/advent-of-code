import Foundation

// Helper function to check password criteria
func is_valid_password(_ number: Int, strictDouble: Bool) -> Bool {
    let digits = Array(String(number))
    var has_double = false
    var run_length = 1

    for i in 1..<digits.count {
        if digits[i] == digits[i - 1] {
            run_length += 1
        } else {
            if run_length == 2 || (!strictDouble && run_length > 1) {
                has_double = true
            }
            run_length = 1
        }
        if digits[i] < digits[i - 1] { return false }
    }

    // Check the final run of digits
    if run_length == 2 || (!strictDouble && run_length > 1) {
        has_double = true
    }

    return has_double
}

func count_valid_passwords(in range: ClosedRange<Int>, strictDouble: Bool) -> Int {
    return range.filter { is_valid_password($0, strictDouble: strictDouble) }.count
}

func main() {
    guard let filename = CommandLine.arguments.dropFirst().first,
          let content = try? String(contentsOfFile: filename, encoding: .utf8) else {
        print("Usage: swift main.swift <input_file>")
        return
    }

    let range = content.trimmingCharacters(in: .whitespacesAndNewlines)
                       .split(separator: "-")
                       .compactMap { Int($0) }

    guard range.count == 2 else {
        print("Invalid input format")
        return
    }

    let (lower, upper) = (range[0], range[1])

    let part1_count = count_valid_passwords(in: lower...upper, strictDouble: false)
    let part2_count = count_valid_passwords(in: lower...upper, strictDouble: true)

    print("Part 1: \(part1_count)")
    print("Part 2: \(part2_count)")
}

main()
