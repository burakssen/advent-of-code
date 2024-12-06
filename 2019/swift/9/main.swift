import Foundation

class IntcodeComputer {
    private var memory: [Int64]
    private var ip = 0  // Instruction pointer
    private var relativeBase = 0
    private var inputs: [Int64]
    private var outputs: [Int64] = []

    init(program: [Int64], inputs: [Int64] = []) {
        memory = Array(repeating: Int64(0), count: 10000)
        memory.replaceSubrange(0..<program.count, with: program)
        self.inputs = inputs
    }

    private func getParameterMode(modes: Int, index: Int) -> Int {
        return (modes / Int(pow(10.0, Double(index)))) % 10
    }

    private func getParameterAddress(index: Int, modes: Int) -> Int {
        let mode = getParameterMode(modes: modes, index: index)
        let value = memory[ip + 1 + index]
        switch mode {
        case 0: return Int(value)  // Position mode
        case 1: return ip + 1 + index  // Immediate mode
        case 2: return relativeBase + Int(value)  // Relative mode
        default: fatalError("Unknown parameter mode: \(mode)")
        }
    }

    private func getValue(index: Int, modes: Int) -> Int64 {
        return memory[getParameterAddress(index: index, modes: modes)]
    }

    private func setValue(index: Int, modes: Int, value: Int64) {
        memory[getParameterAddress(index: index, modes: modes)] = value
    }

    func execute() -> [Int64] {
        while true {
            let instruction = Int(memory[ip])
            let opcode = instruction % 100
            let modes = instruction / 100

            switch opcode {
            case 1: processBinaryOperation(modes: modes, operation: +)
            case 2: processBinaryOperation(modes: modes, operation: *)
            case 3:
                setValue(index: 0, modes: modes, value: inputs.removeFirst())
                ip += 2
            case 4:
                outputs.append(getValue(index: 0, modes: modes))
                ip += 2
            case 5:
                ip =
                    getValue(index: 0, modes: modes) != 0
                    ? Int(getValue(index: 1, modes: modes)) : ip + 3
            case 6:
                ip =
                    getValue(index: 0, modes: modes) == 0
                    ? Int(getValue(index: 1, modes: modes)) : ip + 3
            case 7: processBinaryOperation(modes: modes) { $0 < $1 ? 1 : 0 }
            case 8: processBinaryOperation(modes: modes) { $0 == $1 ? 1 : 0 }
            case 9:
                relativeBase += Int(getValue(index: 0, modes: modes))
                ip += 2
            case 99: return outputs
            default: fatalError("Unknown opcode: \(opcode)")
            }
        }
    }

    private func processBinaryOperation(modes: Int, operation: (Int64, Int64) -> Int64) {
        let result = operation(getValue(index: 0, modes: modes), getValue(index: 1, modes: modes))
        setValue(index: 2, modes: modes, value: result)
        ip += 4
    }
}

func main() {
    guard CommandLine.arguments.count >= 2 else {
        print("Usage: swift main.swift <input_file>")
        return
    }

    guard let content = try? String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
    else {
        print("Could not read input file")
        return
    }

    let program = content.trimmingCharacters(in: .whitespacesAndNewlines)
        .split(separator: ",")
        .compactMap { Int64($0) }

    let part1: Int64 = IntcodeComputer(program: program, inputs: [1]).execute()[0]
    print("Part 1:", part1)

    let part2 = IntcodeComputer(program: program, inputs: [2]).execute()[0]
    print("Part 2:", part2)
}

main()
