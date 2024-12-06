import Foundation

// Represents the Intcode computer
class IntcodeComputer {
    var memory: [Int]
    var ip = 0  // instruction pointer
    var relativeBase = 0
    var inputs: [Int] = []
    var outputs: [Int] = []

    init(program: String) {
        self.memory = program.split(separator: ",").compactMap { Int($0) }
        memory.append(contentsOf: Array(repeating: 0, count: 10000))  // Extend memory
    }

    private func getValue(mode: Int, parameter: Int) -> Int {
        switch mode {
        case 0: return memory[parameter]  // position mode
        case 1: return parameter  // immediate mode
        case 2: return memory[relativeBase + parameter]  // relative mode
        default: fatalError("Unknown parameter mode: \(mode)")
        }
    }

    private func getAddress(mode: Int, parameter: Int) -> Int {
        switch mode {
        case 0: return parameter  // position mode
        case 2: return relativeBase + parameter  // relative mode
        default: fatalError("Invalid mode for address: \(mode)")
        }
    }

    func run() -> Int? {
        while true {
            let instruction = memory[ip]
            let opcode = instruction % 100
            let modes = [
                (instruction / 100) % 10, (instruction / 1000) % 10, (instruction / 10000) % 10,
            ]

            switch opcode {
            case 1, 2, 7, 8:  // add, multiply, less than, equals
                executeBinaryOperation(opcode: opcode, modes: modes)
            case 3:  // input
                if inputs.isEmpty { return nil }  // waiting for input
                let address = getAddress(mode: modes[0], parameter: memory[ip + 1])
                memory[address] = inputs.removeFirst()
                ip += 2
            case 4:  // output
                let value = getValue(mode: modes[0], parameter: memory[ip + 1])
                outputs.append(value)
                ip += 2
            case 5, 6:  // jump-if-true, jump-if-false
                executeJumpOperation(opcode: opcode, modes: modes)
            case 9:  // adjust relative base
                let value = getValue(mode: modes[0], parameter: memory[ip + 1])
                relativeBase += value
                ip += 2
            case 99:  // halt
                return outputs.last
            default:
                fatalError("Unknown opcode: \(opcode)")
            }
        }
    }

    private func executeBinaryOperation(opcode: Int, modes: [Int]) {
        let param1 = memory[ip + 1]
        let param2 = memory[ip + 2]
        let param3 = memory[ip + 3]
        let value1 = getValue(mode: modes[0], parameter: param1)
        let value2 = getValue(mode: modes[1], parameter: param2)
        let address = getAddress(mode: modes[2], parameter: param3)

        switch opcode {
        case 1: memory[address] = value1 + value2  // add
        case 2: memory[address] = value1 * value2  // multiply
        case 7: memory[address] = (value1 < value2) ? 1 : 0  // less than
        case 8: memory[address] = (value1 == value2) ? 1 : 0  // equals
        default: break
        }
        ip += 4
    }

    private func executeJumpOperation(opcode: Int, modes: [Int]) {
        let param1 = memory[ip + 1]
        let param2 = memory[ip + 2]
        let value1 = getValue(mode: modes[0], parameter: param1)
        let value2 = getValue(mode: modes[1], parameter: param2)

        if (opcode == 5 && value1 != 0) || (opcode == 6 && value1 == 0) {
            ip = value2
        } else {
            ip += 3
        }
    }
}

func solve(_ input: String, part2: Bool = false) -> Int {
    let computer = IntcodeComputer(program: input)
    let program: String

    if part2 {
        program = """
            NOT A J
            NOT B T
            OR T J
            NOT C T
            OR T J
            AND D J
            NOT E T
            NOT T T
            OR H T
            AND T J
            RUN
            """
    } else {
        program = """
            NOT A J
            NOT C T
            AND D T
            OR T J
            WALK
            """
    }

    let ascii = Array(program.utf8) + [10]  // Append newline
    computer.inputs = ascii.map { Int($0) }

    if let damage = computer.run() {
        return damage
    }

    while !computer.outputs.isEmpty {
        let value = computer.outputs.removeFirst()
        if value < 128 {
            print(String(UnicodeScalar(UInt8(value))), terminator: "")
        } else {
            return value
        }
    }

    return 0
}

func main() {
    guard CommandLine.arguments.count > 1,
        let content = try? String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
    else {
        print("Usage: \(CommandLine.arguments[0]) <input_file>")
        return
    }

    let input = content.trimmingCharacters(in: .whitespacesAndNewlines)
    print("Part 1: \(solve(input))")
    print("Part 2: \(solve(input, part2: true))")
}

main()
