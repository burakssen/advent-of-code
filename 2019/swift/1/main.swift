import Foundation

func calculateFuel(for mass: Int) -> Int {
    return mass / 3 - 2
}

func totalFuel(for mass: Int) -> Int {
    var total = 0
    var fuel = calculateFuel(for: mass)

    while fuel > 0 {
        total += fuel
        fuel = calculateFuel(for: fuel)
    }

    return total
}

func main() {
    // Get command line arguments
    let args = CommandLine.arguments
    // Check if there are enough arguments
    if args.count < 2 {
        print("Usage: swift main.swift <input_file>")
        return
    }

    let filename = args[1]
    guard let content = try? String(contentsOfFile: filename, encoding: .utf8) else {
        print("Could not read file \(filename)")
        return
    }

    let mass = content.split(separator: "\n").compactMap { Int($0) }

    let totalFuelPart1 = mass.reduce(0) { $0 + calculateFuel(for: $1) }
    let totalFuelPart2 = mass.reduce(0) { $0 + totalFuel(for: $1) }

    print("Part 1: \(totalFuelPart1)")
    print("Part 2: \(totalFuelPart2)")
}

main()
