import Foundation

func generateOrbitMap(from orbitData: [String]) -> [String: String] {
    return orbitData.reduce(into: [String: String]()) { orbitMap, row in
        let pair = row.split(separator: ")").map(String.init)
        orbitMap[pair[1]] = pair[0]
    }
}

func findPath(to value: String, in orbitMap: [String: String]) -> [String] {
    var path = [String]()
    var current = value
    while let next = orbitMap[current] {
        path.append(next)
        current = next
    }
    return path
}

func main() {
    guard CommandLine.arguments.count >= 2 else {
        print("Usage: swift main.swift <input_file>")
        return
    }

    let filename = CommandLine.arguments[1]
    guard let content = try? String(contentsOfFile: filename, encoding: .utf8) else {
        print("Could not read file \(filename)")
        return
    }

    let orbitMap = generateOrbitMap(from: content.split(separator: "\n").map(String.init))

    // Part 1
    let part1Result = orbitMap.keys.reduce(0) { $0 + findPath(to: $1, in: orbitMap).count }
    print("Part 1: \(part1Result)")

    // Part 2
    let youPath = Set(findPath(to: "YOU", in: orbitMap))
    let sanPath = Set(findPath(to: "SAN", in: orbitMap))
    let part2Result = youPath.symmetricDifference(sanPath).count
    print("Part 2: \(part2Result)")
}

main()
