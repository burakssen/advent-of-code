import Foundation

// Structure to hold x, y coordinates on the grid
struct Point: Hashable {
    let x: Int
    let y: Int

    func manhattanDistance() -> Int {
        return abs(x) + abs(y)
    }
}

// Function to trace the wire path and return a dictionary of points and the number of steps to each point
func traceWirePath(_ path: String) -> [Point: Int] {
    var (x, y, steps) = (0, 0, 0)
    var points = [Point: Int]()

    path.split(separator: ",").forEach { move in
        let direction = move.first!
        let distance = Int(move.dropFirst())!

        for _ in 0..<distance {
            switch direction {
            case "R": x += 1
            case "L": x -= 1
            case "U": y += 1
            case "D": y -= 1
            default: break
            }

            steps += 1
            let point = Point(x: x, y: y)
            points[point] = points[point] ?? steps
        }
    }
    return points
}

func findIntersections(_ wire1Points: [Point: Int], _ wire2Points: [Point: Int]) -> [Point] {
    return Array(Set(wire1Points.keys).intersection(wire2Points.keys))
}

func main() {
    guard let filename = CommandLine.arguments.dropFirst().first else {
        print("Usage: swift main.swift <input_file>")
        return
    }

    guard let content = try? String(contentsOfFile: filename, encoding: .utf8) else {
        print("Could not read file \(filename)")
        return
    }

    let lines = content.split(separator: "\n").map(String.init)
    guard lines.count == 2 else {
        print("Input must contain exactly two lines")
        return
    }

    let wire1Points = traceWirePath(lines[0])
    let wire2Points = traceWirePath(lines[1])

    let intersections = findIntersections(wire1Points, wire2Points)

    if let closest = intersections.map({ $0.manhattanDistance() }).min() {
        print("Part 1: \(closest)")
    } else {
        print("No intersections found in Part 1")
    }

    if let fewestSteps = intersections.map({ wire1Points[$0]! + wire2Points[$0]! }).min() {
        print("Part 2: \(fewestSteps)")
    } else {
        print("No intersections found in Part 2")
    }
}

main()
