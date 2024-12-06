import Foundation

struct Point: Hashable {
    let x: Int
    let y: Int

    func neighbors() -> [Point] {
        return [(1, 0), (0, 1), (-1, 0), (0, -1)].map {
            Point(x: self.x + $0.0, y: self.y + $0.1)
        }
    }
}

struct Route {
    let distance: Int
    let path: String
}

struct State: Hashable {
    let positions: [String]
    let keys: Set<String>

    func withKey(_ key: String, robotIndex: Int) -> State {
        var newPos = positions
        newPos[robotIndex] = key
        var newKeys = keys
        newKeys.insert(key)
        return State(positions: newPos, keys: newKeys)
    }
}

class Maze {
    let grid: [String]
    let height: Int
    let width: Int
    private var cache: [String: [String: Route]] = [:]

    init(_ data: [String]) {
        self.grid = data
        self.height = data.count
        self.width = data[0].count
    }

    func get(_ point: Point) -> Character {
        let row = grid[point.y]
        let index = row.index(row.startIndex, offsetBy: point.x)
        return row[index]
    }

    func isValid(_ point: Point) -> Bool {
        return point.x >= 0 && point.x < width && point.y >= 0 && point.y < height
            && get(point) != "#"
    }

    func findPosition(_ target: String) -> Point? {
        for (y, row) in grid.enumerated() {
            if let x = row.firstIndex(of: Character(target)) {
                return Point(x: row.distance(from: row.startIndex, to: x), y: y)
            }
        }
        return nil
    }

    func getKeys() -> Set<String> {
        return Set(grid.flatMap { $0.filter { $0.isLowercase }.map { String($0) } })
    }

    func findRoutes(from start: String) -> [String: Route] {
        if let cached = cache[start] { return cached }

        guard let startPos = findPosition(start) else { return [:] }

        var routes: [String: Route] = [:]
        var visited: Set<Point> = [startPos]
        var queue: [(pos: Point, dist: Int, path: String)] = [(startPos, 0, "")]

        while let (pos, dist, path) = queue.first {
            queue.removeFirst()
            let cell = String(get(pos))

            if cell.first?.isLetter == true, cell != start {
                routes[cell] = Route(distance: dist, path: path)
            }

            for nextPos in pos.neighbors() where isValid(nextPos) && !visited.contains(nextPos) {
                visited.insert(nextPos)
                queue.append((nextPos, dist + 1, path + cell))
            }
        }

        cache[start] = routes
        return routes
    }
}

class KeyCollector {
    private let maze: Maze
    private let allKeys: Set<String>

    init(maze: Maze) {
        self.maze = maze
        self.allKeys = maze.getKeys()
    }

    private func canTraverse(_ path: String, collectedKeys: Set<String>) -> Bool {
        return path.allSatisfy { char in
            char.isLetter ? collectedKeys.contains(char.lowercased()) : true
        }
    }

    func solve(startPositions: [String]) -> Int {
        var states = [State(positions: startPositions, keys: []): 0]

        for _ in 0..<allKeys.count {
            var nextStates: [State: Int] = [:]

            for (state, dist) in states {
                let remainingKeys = allKeys.subtracting(state.keys)

                for key in remainingKeys {
                    for (robotIdx, robotPos) in state.positions.enumerated() {
                        let routes = maze.findRoutes(from: robotPos)
                        if let route = routes[key],
                            canTraverse(route.path, collectedKeys: state.keys)
                        {
                            let newState = state.withKey(key, robotIndex: robotIdx)
                            let newDist = dist + route.distance
                            nextStates[newState] = min(nextStates[newState] ?? Int.max, newDist)
                        }
                    }
                }
            }

            states = nextStates
        }

        return states.values.min() ?? Int.max
    }
}

func solvePart1(_ data: [String]) -> Int {
    let maze = Maze(data)
    let keyCollector = KeyCollector(maze: maze)
    return keyCollector.solve(startPositions: ["@"])
}

func solvePart2(_ data: [String]) -> Int {
    var grid = data.map { Array($0) }

    guard let center = findCenter(in: grid) else { return Int.max }

    placeRobots(in: &grid, at: center)

    let maze = Maze(grid.map { String($0) })
    let keyCollector = KeyCollector(maze: maze)
    return keyCollector.solve(startPositions: ["1", "2", "3", "4"])
}

func findCenter(in grid: [[Character]]) -> Point? {
    for (y, row) in grid.enumerated() {
        if let x = row.firstIndex(of: "@") {
            return Point(x: x, y: y)
        }
    }
    return nil
}

func placeRobots(in grid: inout [[Character]], at center: Point) {
    let robotPositions = [
        Point(x: center.x - 1, y: center.y - 1),
        Point(x: center.x + 1, y: center.y - 1),
        Point(x: center.x - 1, y: center.y + 1),
        Point(x: center.x + 1, y: center.y + 1),
    ]
    robotPositions.forEach {
        grid[$0.y][$0.x] = Character("\(robotPositions.firstIndex(of: $0)! + 1)")
    }
    center.neighbors().forEach { grid[$0.y][$0.x] = "#" }
}

func main() {
    guard CommandLine.arguments.count > 1,
        let content = try? String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
    else {
        print("Usage: \(CommandLine.arguments[0]) <input_file>")
        return
    }

    let data =
        content
        .components(separatedBy: .newlines)
        .filter { !$0.isEmpty }

    print("Part 1: \(solvePart1(data))")
    print("Part 2: \(solvePart2(data))")
}

main()
