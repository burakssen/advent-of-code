import Foundation

struct Point: Hashable {
    let x: Int, y: Int
}

enum Direction: Int {
    case up = 0
    case right, down, left

    mutating func turn(_ clockwise: Bool) {
        self =
            clockwise
            ? Direction(rawValue: (rawValue + 1) % 4)! : Direction(rawValue: (rawValue + 3) % 4)!
    }

    func move(from point: Point) -> Point {
        switch self {
        case .up: return Point(x: point.x, y: point.y + 1)
        case .right: return Point(x: point.x + 1, y: point.y)
        case .down: return Point(x: point.x, y: point.y - 1)
        case .left: return Point(x: point.x - 1, y: point.y)
        }
    }
}

class IntcodeComputer {
    private var memory: [Int: Int]
    private var ip = 0, relativeBase = 0

    init(program: String) {
        memory = Dictionary(
            uniqueKeysWithValues:
                program.split(separator: ",").enumerated().map { ($0.offset, Int($0.element)!) }
        )
    }

    private func getValue(_ mode: Int, _ parameter: Int) -> Int {
        switch mode {
        case 0: return memory[parameter, default: 0]
        case 1: return parameter
        case 2: return memory[relativeBase + parameter, default: 0]
        default: fatalError("Unknown parameter mode: \(mode)")
        }
    }

    private func getAddress(_ mode: Int, _ parameter: Int) -> Int {
        return mode == 2 ? relativeBase + parameter : parameter
    }

    func run(input: () -> Int, output: (Int) -> Void) -> Bool {
        while true {
            let instruction = memory[ip, default: 0]
            let opcode = instruction % 100
            let modes = (1...3).map { (instruction / Int(pow(10, Double($0 + 1)))) % 10 }

            switch opcode {
            case 1, 2:  // Add, Multiply
                let (a, b, dest) = (
                    getValue(modes[0], memory[ip + 1]!), getValue(modes[1], memory[ip + 2]!),
                    getAddress(modes[2], memory[ip + 3]!)
                )
                memory[dest] = opcode == 1 ? a + b : a * b
                ip += 4
            case 3:  // Input
                memory[getAddress(modes[0], memory[ip + 1]!)] = input()
                ip += 2
            case 4:  // Output
                output(getValue(modes[0], memory[ip + 1]!))
                ip += 2
            case 5, 6:  // Jump if true/false
                let (cond, dest) = (
                    getValue(modes[0], memory[ip + 1]!), getValue(modes[1], memory[ip + 2]!)
                )
                ip = (opcode == 5 && cond != 0) || (opcode == 6 && cond == 0) ? dest : ip + 3
            case 7, 8:  // Less than, Equals
                let (a, b, dest) = (
                    getValue(modes[0], memory[ip + 1]!), getValue(modes[1], memory[ip + 2]!),
                    getAddress(modes[2], memory[ip + 3]!)
                )
                memory[dest] = (opcode == 7 ? a < b : a == b) ? 1 : 0
                ip += 4
            case 9:  // Adjust relative base
                relativeBase += getValue(modes[0], memory[ip + 1]!)
                ip += 2
            case 99: return true
            default: fatalError("Unknown opcode: \(opcode)")
            }
        }
    }
}

func runRobot(program: String, startingColor: Int) -> [Point: Int] {
    let computer = IntcodeComputer(program: program)
    var paintedPanels = [Point: Int]()
    var position = Point(x: 0, y: 0)
    var direction = Direction.up

    paintedPanels[position] = startingColor
    var outputs = [Int]()

    _ = computer.run(
        input: { paintedPanels[position, default: 0] },
        output: { value in
            outputs.append(value)
            if outputs.count == 2 {
                paintedPanels[position] = outputs[0]
                direction.turn(outputs[1] == 1)
                position = direction.move(from: position)
                outputs.removeAll()
            }
        }
    )

    return paintedPanels
}

func visualizePanels(_ panels: [Point: Int]) {
    let (minX, maxX) = (panels.keys.map(\.x).min()!, panels.keys.map(\.x).max()!)
    let (minY, maxY) = (panels.keys.map(\.y).min()!, panels.keys.map(\.y).max()!)

    for y in (minY...maxY).reversed() {
        print(
            " ",
            (minX...maxX).map { panels[Point(x: $0, y: y), default: 0] == 1 ? "█" : " " }.joined())
    }
}

func main() {
    guard let content = try? String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
    else {
        print("Could not read input file")
        return
    }

    let part1 = runRobot(program: content, startingColor: 0).count
    print("Part 1:", part1)
    let part2 = runRobot(program: content, startingColor: 1)
    print("Part 2:")
    visualizePanels(part2)
}

main()
