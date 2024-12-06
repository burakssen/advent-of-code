import Foundation

struct Point: Hashable {
    let x, y: Int
}

class BugsSimulation {
    // Part 1: Grid Layout Tracking
    func calculateBiodiversityRating(_ initialGrid: [String]) -> Int {
        var grid = initialGrid
        var seenLayouts = Set<String>()

        while true {
            let layout = grid.joined(separator: "\n")
            guard !seenLayouts.contains(layout) else {
                return calculateBiodiversity(grid)
            }
            seenLayouts.insert(layout)
            grid = simulateGridStep(grid)
        }
    }

    private func simulateGridStep(_ grid: [String]) -> [String] {
        return grid.enumerated().map { (y, row) in
            var updatedRow = Array(row)
            updatedRow = updatedRow.enumerated().map { (x, tile) in
                let adjacentBugs = countAdjacentBugs(grid, x, y)

                if tile == "#" && adjacentBugs != 1 { return "." }
                if tile == "." && (adjacentBugs == 1 || adjacentBugs == 2) { return "#" }
                return tile
            }
            return String(updatedRow)
        }
    }

    private func countAdjacentBugs(_ grid: [String], _ x: Int, _ y: Int) -> Int {
        let directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]
        return directions.reduce(0) { count, dir in
            let (newX, newY) = (x + dir.0, y + dir.1)
            guard newX >= 0 && newX < grid[0].count && newY >= 0 && newY < grid.count else {
                return count
            }
            return count + (Array(grid[newY])[newX] == "#" ? 1 : 0)
        }
    }

    private func calculateBiodiversity(_ grid: [String]) -> Int {
        var rating = 0
        var power = 1

        for row in grid {
            for tile in row {
                if tile == "#" { rating += power }
                power *= 2
            }
        }

        return rating
    }

    // Part 2: Recursive Grid Simulation
    func countBugsAfterMinutes(_ initialGrid: [String], minutes: Int) -> Int {
        var levels = [0: gridToSet(initialGrid)]

        for _ in 0..<minutes {
            levels = simulateRecursiveLevels(levels)
        }

        return levels.values.flatMap { $0 }.count
    }

    private func simulateRecursiveLevels(_ levels: [Int: Set<Point>]) -> [Int: Set<Point>] {
        let minLevel = levels.keys.min()! - 1
        let maxLevel = levels.keys.max()! + 1

        return Dictionary(
            uniqueKeysWithValues: (minLevel...maxLevel).compactMap { level in
                let nextLevelBugs = simulateLevelBugs(levels, level)
                return nextLevelBugs.isEmpty ? nil : (level, nextLevelBugs)
            })
    }

    private func simulateLevelBugs(_ levels: [Int: Set<Point>], _ level: Int) -> Set<Point> {
        var nextLevelBugs = Set<Point>()

        for y in 0..<5 {
            for x in 0..<5 {
                guard x != 2 || y != 2 else { continue }  // Skip middle tile

                let point = Point(x: x, y: y)
                let adjacentBugs = countRecursiveAdjacentBugs(levels, point, level)
                let currentBug = levels[level]?.contains(point) ?? false

                if (currentBug && adjacentBugs == 1)
                    || (!currentBug && (adjacentBugs == 1 || adjacentBugs == 2))
                {
                    nextLevelBugs.insert(point)
                }
            }
        }

        return nextLevelBugs
    }

    private func countRecursiveAdjacentBugs(
        _ levels: [Int: Set<Point>], _ point: Point, _ level: Int
    ) -> Int {
        let (outerLevel, currentLevel, innerLevel) = (
            levels[level - 1] ?? Set(),
            levels[level] ?? Set(),
            levels[level + 1] ?? Set()
        )

        let directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]
        return directions.reduce(0) { (count, direction) in
            let (dx, dy) = direction
            let (newX, newY) = (point.x + dx, point.y + dy)

            if newX == 2 && newY == 2 {
                return count + countInnerLevelBugs(innerLevel, dx, dy)
            }

            return count + countEdgeBugs(outerLevel, currentLevel, newX, newY)
        }
    }

    private func countInnerLevelBugs(_ innerLevel: Set<Point>, _ dx: Int, _ dy: Int) -> Int {
        switch (dx, dy) {
        case (0, 1): return innerLevel.filter { $0.y == 0 }.count
        case (0, -1): return innerLevel.filter { $0.y == 4 }.count
        case (1, 0): return innerLevel.filter { $0.x == 0 }.count
        case (-1, 0): return innerLevel.filter { $0.x == 4 }.count
        default: return 0
        }
    }

    private func countEdgeBugs(
        _ outerLevel: Set<Point>, _ currentLevel: Set<Point>, _ x: Int, _ y: Int
    ) -> Int {
        switch (x, y) {
        case (-1, _): return outerLevel.contains(Point(x: 1, y: 2)) ? 1 : 0
        case (5, _): return outerLevel.contains(Point(x: 3, y: 2)) ? 1 : 0
        case (_, -1): return outerLevel.contains(Point(x: 2, y: 1)) ? 1 : 0
        case (_, 5): return outerLevel.contains(Point(x: 2, y: 3)) ? 1 : 0
        default: return currentLevel.contains(Point(x: x, y: y)) ? 1 : 0
        }
    }

    private func gridToSet(_ grid: [String]) -> Set<Point> {
        var bugs = Set<Point>()
        for (y, row) in grid.enumerated() {
            for (x, char) in row.enumerated() where char == "#" {
                bugs.insert(Point(x: x, y: y))
            }
        }
        return bugs
    }
}

func main() {
    guard CommandLine.arguments.count > 1,
        let content = try? String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
            .components(separatedBy: .newlines)
    else {
        print("Usage: \(CommandLine.arguments[0]) <input_file>")
        return
    }

    let simulation = BugsSimulation()
    print("Part 1: \(simulation.calculateBiodiversityRating(content))")
    print("Part 2: \(simulation.countBugsAfterMinutes(content, minutes: 200))")
}

main()
