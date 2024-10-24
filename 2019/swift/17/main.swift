import Foundation

struct Point: Hashable {
    let x: Int
    let y: Int
}

enum OpCode: Int {
    case add = 1
    case multiply = 2
    case input = 3
    case output = 4
    case jumpIfTrue = 5
    case jumpIfFalse = 6
    case lessThan = 7
    case equals = 8
    case adjustRelativeBase = 9
    case halt = 99
}

enum ParameterMode: Int {
    case position = 0
    case immediate = 1
    case relative = 2
}

struct Instruction {
    let opcode: OpCode
    let modes: [ParameterMode]

    init(_ value: Int) {
        self.opcode = OpCode(rawValue: value % 100) ?? .halt
        self.modes = [
            ParameterMode(rawValue: (value / 100) % 10) ?? .position,
            ParameterMode(rawValue: (value / 1000) % 10) ?? .position,
            ParameterMode(rawValue: (value / 10000) % 10) ?? .position,
        ]
    }
}

final class IntCodeComputer {
    private var memory: [Int: Int]
    private var ip = 0
    private var relativeBase = 0
    private var inputBuffer: [Int] = []
    private var inputIndex = 0

    init(program: [Int]) {
        self.memory = Dictionary(uniqueKeysWithValues: program.enumerated().map { ($0.0, $0.1) })
    }

    func setValue(at address: Int, to value: Int) {
        memory[address] = value
    }

    func setInput(_ input: [Int]) {
        inputBuffer = input
        inputIndex = 0
    }

    private func getValue(at position: Int, mode: ParameterMode) -> Int {
        let value = memory[position, default: 0]
        switch mode {
        case .position: return memory[value, default: 0]
        case .immediate: return value
        case .relative: return memory[value + relativeBase, default: 0]
        }
    }

    private func getAddress(at position: Int, mode: ParameterMode) -> Int {
        let value = memory[position, default: 0]
        return mode == .relative ? value + relativeBase : value
    }

    func execute() -> [Int] {
        var output: [Int] = []

        while true {
            let instruction = Instruction(memory[ip, default: 0])

            switch instruction.opcode {
            case .add:
                let value1 = getValue(at: ip + 1, mode: instruction.modes[0])
                let value2 = getValue(at: ip + 2, mode: instruction.modes[1])
                let resultAddress = getAddress(at: ip + 3, mode: instruction.modes[2])
                memory[resultAddress] = value1 + value2
                ip += 4

            case .multiply:
                let value1 = getValue(at: ip + 1, mode: instruction.modes[0])
                let value2 = getValue(at: ip + 2, mode: instruction.modes[1])
                let resultAddress = getAddress(at: ip + 3, mode: instruction.modes[2])
                memory[resultAddress] = value1 * value2
                ip += 4

            case .input:
                guard inputIndex < inputBuffer.count else { fatalError("Input buffer empty") }
                let resultAddress = getAddress(at: ip + 1, mode: instruction.modes[0])
                memory[resultAddress] = inputBuffer[inputIndex]
                inputIndex += 1
                ip += 2

            case .output:
                output.append(getValue(at: ip + 1, mode: instruction.modes[0]))
                ip += 2

            case .jumpIfTrue:
                let value = getValue(at: ip + 1, mode: instruction.modes[0])
                let jumpTo = getValue(at: ip + 2, mode: instruction.modes[1])
                ip = value != 0 ? jumpTo : ip + 3

            case .jumpIfFalse:
                let value = getValue(at: ip + 1, mode: instruction.modes[0])
                let jumpTo = getValue(at: ip + 2, mode: instruction.modes[1])
                ip = value == 0 ? jumpTo : ip + 3

            case .lessThan:
                let value1 = getValue(at: ip + 1, mode: instruction.modes[0])
                let value2 = getValue(at: ip + 2, mode: instruction.modes[1])
                let resultAddress = getAddress(at: ip + 3, mode: instruction.modes[2])
                memory[resultAddress] = value1 < value2 ? 1 : 0
                ip += 4

            case .equals:
                let value1 = getValue(at: ip + 1, mode: instruction.modes[0])
                let value2 = getValue(at: ip + 2, mode: instruction.modes[1])
                let resultAddress = getAddress(at: ip + 3, mode: instruction.modes[2])
                memory[resultAddress] = value1 == value2 ? 1 : 0
                ip += 4

            case .adjustRelativeBase:
                relativeBase += getValue(at: ip + 1, mode: instruction.modes[0])
                ip += 2

            case .halt:
                return output
            }
        }
    }
}

struct GridProcessor {
    static func findIntersections(from asciiOutput: [Int]) -> [(Point, Int)] {
        let grid = processOutput(asciiOutput)
        return findIntersectionPoints(in: grid)
    }

    private static func processOutput(_ asciiOutput: [Int]) -> [[Character]] {
        var grid: [[Character]] = []
        var currentRow: [Character] = []

        for code in asciiOutput {
            if code == 10 {
                if !currentRow.isEmpty {
                    grid.append(currentRow)
                    currentRow = []
                }
            } else {
                currentRow.append(Character(UnicodeScalar(code)!))
            }
        }

        return grid.filter { !$0.isEmpty }
    }

    private static func findIntersectionPoints(in grid: [[Character]]) -> [(Point, Int)] {
        var intersections: [(Point, Int)] = []

        for y in 1..<(grid.count - 1) {
            for x in 1..<(grid[y].count - 1) {
                if isIntersection(at: Point(x: x, y: y), in: grid) {
                    intersections.append((Point(x: x, y: y), x * y))
                }
            }
        }

        return intersections
    }

    private static func isIntersection(at point: Point, in grid: [[Character]]) -> Bool {
        let positions = [
            (point.x, point.y),
            (point.x, point.y - 1),
            (point.x, point.y + 1),
            (point.x - 1, point.y),
            (point.x + 1, point.y),
        ]

        return positions.allSatisfy { grid[$0.1][$0.0] == "#" }
    }
}

struct MovementCommands {
    static func getCommands() -> [Int] {
        let commands = [
            "A,B,A,B,C,C,B,A,B,C\n",
            "L,4,R,8,L,6,L,10\n",
            "L,6,R,8,R,10,L,6,L,6\n",
            "L,4,L,4,L,10\n",
            "n\n",
        ].joined()

        return commands.utf8.map { Int($0) }
    }
}

func main() {
    guard CommandLine.arguments.count > 1,
        let content = try? String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
    else {
        print("Usage: \(CommandLine.arguments[0]) <input_file>")
        return
    }

    let program = content.components(separatedBy: ",")
        .compactMap { Int($0.trimmingCharacters(in: .whitespacesAndNewlines)) }

    // Part 1
    let computer1 = IntCodeComputer(program: program)
    let output1 = computer1.execute()

    let intersections = GridProcessor.findIntersections(from: output1)
    print("Part 1:", intersections.reduce(0) { $0 + $1.1 })

    // Part 2
    let computer2 = IntCodeComputer(program: program)
    computer2.setValue(at: 0, to: 2)
    computer2.setInput(MovementCommands.getCommands())
    let output2 = computer2.execute()
    print("Part 2:", output2.last!)
}

main()
