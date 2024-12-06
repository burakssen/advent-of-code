import Foundation

// MARK: - Models
struct Position: Hashable {
    let x: Int
    let y: Int

    // Pre-compute cardinal directions for better readability and potential micro-optimization
    static let cardinalOffsets = [(1, 0), (-1, 0), (0, 1), (0, -1)]

    var neighbors: [Position] {
        Position.cardinalOffsets.map { Position(x: x + $0.0, y: y + $0.1) }
    }
}

struct MazeState: Hashable {
    let position: Position
    let level: Int
}

// MARK: - Priority Queue
struct PriorityQueue<T> {
    private var heap: [(T, Int)] = []
    private let orderCriteria: (Int, Int) -> Bool

    init(ascending: Bool = true) {
        self.orderCriteria = ascending ? (<) : (>)
    }

    var isEmpty: Bool { heap.isEmpty }

    mutating func enqueue(_ element: T, priority: Int) {
        heap.append((element, priority))
        siftUp(from: heap.count - 1)
    }

    mutating func dequeue() -> (T, Int)? {
        guard !heap.isEmpty else { return nil }

        if heap.count == 1 { return heap.removeLast() }

        let result = heap[0]
        heap[0] = heap.removeLast()
        if !heap.isEmpty { siftDown(from: 0) }

        return result
    }

    private mutating func siftUp(from index: Int) {
        var child = index
        var parent = (child - 1) / 2

        while child > 0 && orderCriteria(heap[child].1, heap[parent].1) {
            heap.swapAt(child, parent)
            child = parent
            parent = (child - 1) / 2
        }
    }

    private mutating func siftDown(from index: Int) {
        var parent = index

        while true {
            let leftChild = 2 * parent + 1
            let rightChild = leftChild + 1
            var candidate = parent

            if leftChild < heap.count && orderCriteria(heap[leftChild].1, heap[candidate].1) {
                candidate = leftChild
            }
            if rightChild < heap.count && orderCriteria(heap[rightChild].1, heap[candidate].1) {
                candidate = rightChild
            }

            if candidate == parent { return }

            heap.swapAt(parent, candidate)
            parent = candidate
        }
    }
}

// MARK: - DonutMaze
final class DonutMaze {
    private let grid: [[Character]]
    private let portalsMap: [Position: Position]  // Direct portal mapping for O(1) lookup
    private let outerPortals: Set<Position>  // Cache outer portal positions
    private let start: Position
    private let end: Position
    private let width: Int
    private let height: Int
    private let maxRecursionDepth = 25

    // Cache boundary checks
    private let xRange: Range<Int>
    private let yRange: Range<Int>

    init?(input: String) {
        // Initialize grid
        self.grid = input.components(separatedBy: .newlines).map(Array.init)
        self.height = grid.count
        self.width = grid[0].count

        self.xRange = 0..<width
        self.yRange = 0..<height

        // Process portals
        let portalInfo = DonutMaze.findPortals(in: grid)
        var portalsMap = [Position: Position]()
        var outerPortals = Set<Position>()

        // Create direct portal mappings and identify outer portals
        for (_, positions) in portalInfo where positions.count == 2 {
            portalsMap[positions[0]] = positions[1]
            portalsMap[positions[1]] = positions[0]

            if positions[0].x == 2 || positions[0].y == 2 || positions[0].x == width - 3
                || positions[0].y == height - 3
            {
                outerPortals.insert(positions[0])
            }
            if positions[1].x == 2 || positions[1].y == 2 || positions[1].x == width - 3
                || positions[1].y == height - 3
            {
                outerPortals.insert(positions[1])
            }
        }

        self.portalsMap = portalsMap
        self.outerPortals = outerPortals

        // Set start and end positions
        guard let start = portalInfo["AA"]?.first,
            let end = portalInfo["ZZ"]?.first
        else {
            return nil
        }

        self.start = start
        self.end = end
    }

    // MARK: - Public Methods
    func findShortestPath(recursive: Bool = false) -> Int? {
        recursive ? findRecursivePath() : findSimplePath()
    }

    // MARK: - Private Methods
    private func findSimplePath() -> Int? {
        var queue = PriorityQueue<Position>()
        var distances = [Position: Int]()

        queue.enqueue(start, priority: 0)
        distances[start] = 0

        while let (current, steps) = queue.dequeue() {
            if current == end { return steps }

            for next in validMoves(from: current) {
                let newDistance = steps + 1
                if let existingDistance = distances[next], existingDistance <= newDistance {
                    continue
                }
                distances[next] = newDistance
                queue.enqueue(next, priority: newDistance)
            }
        }
        return nil
    }

    private func findRecursivePath() -> Int? {
        var queue = PriorityQueue<MazeState>()
        var distances = [MazeState: Int]()
        let initialState = MazeState(position: start, level: 0)

        queue.enqueue(initialState, priority: 0)
        distances[initialState] = 0

        while let (current, steps) = queue.dequeue() {
            if current.position == end && current.level == 0 { return steps }

            for next in validRecursiveMoves(from: current) {
                let newDistance = steps + 1
                if let existingDistance = distances[next], existingDistance <= newDistance {
                    continue
                }
                distances[next] = newDistance
                queue.enqueue(next, priority: newDistance)
            }
        }
        return nil
    }

    private func validMoves(from position: Position) -> [Position] {
        var moves = position.neighbors.filter(isValidTile)
        if let portalDest = portalsMap[position] {
            moves.append(portalDest)
        }
        return moves
    }

    private func validRecursiveMoves(from state: MazeState) -> [MazeState] {
        var moves = state.position.neighbors
            .filter(isValidTile)
            .map { MazeState(position: $0, level: state.level) }

        if let portalDest = portalsMap[state.position] {
            let isCurrentOuter = outerPortals.contains(state.position)
            let newLevel = state.level + (isCurrentOuter ? -1 : 1)

            if newLevel >= 0 && newLevel <= maxRecursionDepth {
                moves.append(MazeState(position: portalDest, level: newLevel))
            }
        }

        return moves
    }

    private func isValidTile(_ pos: Position) -> Bool {
        xRange.contains(pos.x) && yRange.contains(pos.y) && grid[pos.y][pos.x] == "."
    }

    // MARK: - Static Methods
    private static func findPortals(in grid: [[Character]]) -> [String: [Position]] {
        var portals: [String: [Position]] = [:]
        let height = grid.count
        let width = grid[0].count

        // Scan vertical portals
        for y in 0..<height - 1 {
            for x in 0..<width where grid[y][x].isUppercase && grid[y + 1][x].isUppercase {
                let label = String([grid[y][x], grid[y + 1][x]])
                if y > 0 && grid[y - 1][x] == "." {
                    portals[label, default: []].append(Position(x: x, y: y - 1))
                } else if y + 2 < height && grid[y + 2][x] == "." {
                    portals[label, default: []].append(Position(x: x, y: y + 2))
                }
            }
        }

        // Scan horizontal portals
        for y in 0..<height {
            for x in 0..<width - 1 where grid[y][x].isUppercase && grid[y][x + 1].isUppercase {
                let label = String([grid[y][x], grid[y][x + 1]])
                if x > 0 && grid[y][x - 1] == "." {
                    portals[label, default: []].append(Position(x: x - 1, y: y))
                } else if x + 2 < width && grid[y][x + 2] == "." {
                    portals[label, default: []].append(Position(x: x + 2, y: y))
                }
            }
        }

        return portals
    }
}

// MARK: - Main
func main() {
    guard CommandLine.arguments.count > 1,
        let content = try? String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8),
        let maze = DonutMaze(input: content)
    else {
        print("Usage: \(CommandLine.arguments[0]) <input_file>")
        return
    }

    // Part 1
    if let steps = maze.findShortestPath() {
        print("Part 1: \(steps)")
    } else {
        print("Part 1 - No path found!")
    }

    // Part 2
    if let recursiveSteps = maze.findShortestPath(recursive: true) {
        print("Part 2: \(recursiveSteps)")
    } else {
        print("Part 2 - No recursive path found!")
    }
}

main()
