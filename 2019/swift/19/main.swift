import Foundation

class IntcodeComputer {
    private var memory: [Int]
    private var ip = 0
    private var relativeBase = 0
    private var inputs: [Int]
    private var outputValue: Int?

    init(program: [Int], inputs: [Int] = []) {
        self.memory = program + Array(repeating: 0, count: program.count * 9)
        self.inputs = inputs
    }

    private func getParameterMode(_ instruction: Int, _ paramNum: Int) -> Int {
        return (instruction / Int(pow(10.0, Double(paramNum + 1)))) % 10
    }

    private func getParameter(_ paramNum: Int, _ instruction: Int) -> Int {
        let mode = getParameterMode(instruction, paramNum)
        let value = memory[ip + paramNum]

        switch mode {
        case 0: return memory[value]
        case 1: return value
        case 2: return memory[relativeBase + value]
        default: fatalError("Unknown parameter mode: \(mode)")
        }
    }

    private func getWriteAddress(_ paramNum: Int, _ instruction: Int) -> Int {
        let mode = getParameterMode(instruction, paramNum)
        let value = memory[ip + paramNum]

        switch mode {
        case 0: return value
        case 2: return relativeBase + value
        default: fatalError("Invalid mode for write parameter: \(mode)")
        }
    }

    func execute() -> Int? {
        while memory[ip] != 99 {
            let instruction = memory[ip]
            let opcode = instruction % 100

            switch opcode {
            case 1, 2:
                let param1 = getParameter(1, instruction)
                let param2 = getParameter(2, instruction)
                let writeAddr = getWriteAddress(3, instruction)
                memory[writeAddr] = (opcode == 1) ? param1 + param2 : param1 * param2
                ip += 4

            case 3:
                guard !inputs.isEmpty else { fatalError("No input available") }
                let writeAddr = getWriteAddress(1, instruction)
                memory[writeAddr] = inputs.removeFirst()
                ip += 2

            case 4:
                outputValue = getParameter(1, instruction)
                ip += 2
                return outputValue

            case 5, 6:
                let param1 = getParameter(1, instruction)
                let param2 = getParameter(2, instruction)
                ip = (opcode == 5 && param1 != 0 || opcode == 6 && param1 == 0) ? param2 : ip + 3

            case 7, 8:
                let param1 = getParameter(1, instruction)
                let param2 = getParameter(2, instruction)
                let writeAddr = getWriteAddress(3, instruction)
                memory[writeAddr] =
                    (opcode == 7) ? (param1 < param2 ? 1 : 0) : (param1 == param2 ? 1 : 0)
                ip += 4

            case 9:
                let param1 = getParameter(1, instruction)
                relativeBase += param1
                ip += 2

            default:
                fatalError("Unknown opcode: \(opcode)")
            }
        }
        return nil
    }
}

func checkPoint(program: [Int], x: Int, y: Int) -> Int {
    let computer = IntcodeComputer(program: program, inputs: [x, y])
    return computer.execute() ?? 0
}

func part1(program: [Int]) -> Int {
    return (0..<50).flatMap { y in (0..<50).map { x in checkPoint(program: program, x: x, y: y) } }
        .reduce(0, +)
}

func part2(program: [Int]) -> Int {
    var x = 0
    var y = 0

    func squareFitsAt(x: Int, y: Int) -> Bool {
        return checkPoint(program: program, x: x, y: y) == 1
            && checkPoint(program: program, x: x + 99, y: y) == 1
            && checkPoint(program: program, x: x, y: y + 99) == 1
            && checkPoint(program: program, x: x + 99, y: y + 99) == 1
    }

    while true {
        while checkPoint(program: program, x: x + 99, y: y) == 0 {
            y += 1
        }

        while checkPoint(program: program, x: x, y: y + 99) == 0 {
            x += 1
        }

        if squareFitsAt(x: x, y: y) {
            break
        }
        y += 1
    }

    return (x * 10000) + y
}

func main() {
    guard CommandLine.arguments.count > 1,
        let content = try? String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
    else {
        print("Usage: \(CommandLine.arguments[0]) <input_file>")
        return
    }

    let program = content.split(separator: ",").compactMap { Int($0) }

    let affectedPoints = part1(program: program)
    print("Part 1: \(affectedPoints)")

    let resultPart2 = part2(program: program)
    print("Part 2: \(resultPart2)")
}

main()
