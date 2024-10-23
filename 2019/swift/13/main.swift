import Foundation

// Use Int8 for smaller memory footprint where possible
enum Opcode: Int8 {
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

enum ParameterMode: Int8 {
    case position = 0
    case immediate = 1
    case relative = 2
}

enum TileType: Int8 {
    case empty = 0
    case wall = 1
    case block = 2
    case paddle = 3
    case ball = 4
}

// Use a struct for better memory performance
struct GameOutput {
    let x: Int16
    let y: Int16
    let value: Int16

    var isTile: Bool { x >= 0 && y >= 0 }
    var isScore: Bool { x == -1 && y == 0 }
}

// Optimize GameState with smaller integer types
final class GameState {
    private(set) var score: Int = 0
    private(set) var paddleX: Int16 = 0
    private(set) var ballX: Int16 = 0

    @inline(__always)  // Inline small, frequently called functions
    func update(output: GameOutput) {
        if output.isScore {
            score = Int(output.value)
        } else if let tileType = TileType(rawValue: Int8(output.value)) {
            switch tileType {
            case .paddle: paddleX = output.x
            case .ball: ballX = output.x
            default: break
            }
        }
    }

    @inline(__always)
    var joystickPosition: Int8 {
        paddleX < ballX ? 1 : (paddleX > ballX ? -1 : 0)
    }
}

// Optimize IntcodeComputer for performance
final class IntcodeComputer {
    // Use ContiguousArray for better performance with value types
    private var memory: ContiguousArray<Int>
    private var ip = 0
    private var relativeBase = 0
    private let gameState: GameState?
    private var outputs = ContiguousArray<Int>()

    // Cache frequently accessed values
    private let memorySize: Int

    init(program: [Int], gameState: GameState? = nil, insertQuarters: Bool = false) {
        var program = ContiguousArray(program)
        if insertQuarters { program[0] = 2 }
        // Only allocate needed memory - reduce from 10000 to actual needed size
        self.memorySize = program.count + 5000  // Reduced buffer size
        self.memory = program
        memory.reserveCapacity(memorySize)
        memory.append(contentsOf: repeatElement(0, count: memorySize - program.count))
        self.gameState = gameState
    }

    // Optimize parameter access with inline functions
    @inline(__always)
    private func getParam(mode: ParameterMode, paramNum: Int) -> Int {
        let value = memory[ip + paramNum]
        switch mode {
        case .position: return memory[value]
        case .immediate: return value
        case .relative: return memory[value + relativeBase]
        }
    }

    @inline(__always)
    private func getWriteAddress(mode: ParameterMode, paramNum: Int) -> Int {
        let value = memory[ip + paramNum]
        return mode == .relative ? value + relativeBase : value
    }

    @inline(__always)
    private func processOutput(_ value: Int) {
        outputs.append(value)
        if outputs.count == 3, let gameState = gameState {
            let output = GameOutput(
                x: Int16(outputs[0]),
                y: Int16(outputs[1]),
                value: Int16(outputs[2])
            )
            gameState.update(output: output)
            outputs.removeAll(keepingCapacity: true)
        }
    }

    // Use lookup table for instruction parsing
    private static let powerOfTen: [Int] = [1, 10, 100, 1000, 10000]

    func execute() -> ContiguousArray<Int> {
        while true {
            let instruction = memory[ip]
            let opcode = Opcode(rawValue: Int8(instruction % 100))!

            // Optimize mode calculation
            let modes = (0...2).map {
                ParameterMode(rawValue: Int8((instruction / Self.powerOfTen[$0 + 2]) % 10))!
            }

            switch opcode {
            case .add:
                memory[getWriteAddress(mode: modes[2], paramNum: 3)] =
                    getParam(mode: modes[0], paramNum: 1) + getParam(mode: modes[1], paramNum: 2)
                ip += 4

            case .multiply:
                memory[getWriteAddress(mode: modes[2], paramNum: 3)] =
                    getParam(mode: modes[0], paramNum: 1) * getParam(mode: modes[1], paramNum: 2)
                ip += 4

            case .input:
                memory[getWriteAddress(mode: modes[0], paramNum: 1)] =
                    Int(gameState?.joystickPosition ?? 0)
                ip += 2

            case .output:
                processOutput(getParam(mode: modes[0], paramNum: 1))
                ip += 2

            case .jumpIfTrue:
                ip =
                    getParam(mode: modes[0], paramNum: 1) != 0
                    ? getParam(mode: modes[1], paramNum: 2)
                    : ip + 3

            case .jumpIfFalse:
                ip =
                    getParam(mode: modes[0], paramNum: 1) == 0
                    ? getParam(mode: modes[1], paramNum: 2)
                    : ip + 3

            case .lessThan:
                memory[getWriteAddress(mode: modes[2], paramNum: 3)] =
                    getParam(mode: modes[0], paramNum: 1) < getParam(mode: modes[1], paramNum: 2)
                    ? 1 : 0
                ip += 4

            case .equals:
                memory[getWriteAddress(mode: modes[2], paramNum: 3)] =
                    getParam(mode: modes[0], paramNum: 1) == getParam(mode: modes[1], paramNum: 2)
                    ? 1 : 0
                ip += 4

            case .adjustRelativeBase:
                relativeBase += getParam(mode: modes[0], paramNum: 1)
                ip += 2

            case .halt:
                return outputs
            }
        }
    }
}

func solveArcadeGame(program: [Int]) -> (blockCount: Int, finalScore: Int) {
    // Part 1: Count blocks
    let outputs = IntcodeComputer(program: program).execute()
    let blockCount = stride(from: 2, to: outputs.count, by: 3)
        .reduce(0) { count, index in
            count + (outputs[index] == Int(TileType.block.rawValue) ? 1 : 0)
        }

    // Part 2: Play game
    let gameState = GameState()
    _ = IntcodeComputer(program: program, gameState: gameState, insertQuarters: true).execute()

    return (blockCount, gameState.score)
}

func main() {
    guard CommandLine.arguments.count > 1,
        let content = try? String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
    else {
        print("Usage: \(CommandLine.arguments[0]) <input_file>")
        return
    }

    // Optimize program loading
    let program = content.trimmingCharacters(in: .whitespacesAndNewlines)
        .split(separator: ",")
        .map { Int($0)! }  // Use forced unwrap since we know the input format

    let (blockCount, finalScore) = solveArcadeGame(program: program)
    print("Part 1: \(blockCount)")
    print("Part 2: \(finalScore)")
}

main()
