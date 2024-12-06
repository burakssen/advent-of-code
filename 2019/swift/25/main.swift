import Foundation

class IntcodeComputer {
    private var memory: [Int]
    private var ip = 0  // Instruction Pointer
    private var relativeBase = 0
    private var inputQueue: [Int] = []
    private var outputQueue: [Int] = []

    init(program: [Int]) {
        self.memory = program + Array(repeating: 0, count: 10_000_000)  // Expand memory
    }

    func run(inputs: [Int] = []) -> [Int] {
        inputQueue.append(contentsOf: inputs)
        while memory[ip] != 99 {
            let instruction = memory[ip]
            let opcode = instruction % 100
            let modes = [
                (instruction / 100) % 10, (instruction / 1000) % 10, (instruction / 10000) % 10,
            ]

            switch opcode {
            case 1:  // Add
                let a = getValue(mode: modes[0], parameter: memory[ip + 1])
                let b = getValue(mode: modes[1], parameter: memory[ip + 2])
                let target = getAddress(mode: modes[2], parameter: memory[ip + 3])
                memory[target] = a + b
                ip += 4
            case 2:  // Multiply
                let a = getValue(mode: modes[0], parameter: memory[ip + 1])
                let b = getValue(mode: modes[1], parameter: memory[ip + 2])
                let target = getAddress(mode: modes[2], parameter: memory[ip + 3])
                memory[target] = a * b
                ip += 4
            case 3:  // Input
                if inputQueue.isEmpty { return outputQueue }  // Pause for more input
                let target = getAddress(mode: modes[0], parameter: memory[ip + 1])
                memory[target] = inputQueue.removeFirst()
                ip += 2
            case 4:  // Output
                let value = getValue(mode: modes[0], parameter: memory[ip + 1])
                outputQueue.append(value)
                ip += 2
            case 5:  // Jump-if-true
                let condition = getValue(mode: modes[0], parameter: memory[ip + 1])
                let target = getValue(mode: modes[1], parameter: memory[ip + 2])
                ip = condition != 0 ? target : ip + 3
            case 6:  // Jump-if-false
                let condition = getValue(mode: modes[0], parameter: memory[ip + 1])
                let target = getValue(mode: modes[1], parameter: memory[ip + 2])
                ip = condition == 0 ? target : ip + 3
            case 7:  // Less than
                let a = getValue(mode: modes[0], parameter: memory[ip + 1])
                let b = getValue(mode: modes[1], parameter: memory[ip + 2])
                let target = getAddress(mode: modes[2], parameter: memory[ip + 3])
                memory[target] = (a < b) ? 1 : 0
                ip += 4
            case 8:  // Equals
                let a = getValue(mode: modes[0], parameter: memory[ip + 1])
                let b = getValue(mode: modes[1], parameter: memory[ip + 2])
                let target = getAddress(mode: modes[2], parameter: memory[ip + 3])
                memory[target] = (a == b) ? 1 : 0
                ip += 4
            case 9:  // Adjust relative base
                let offset = getValue(mode: modes[0], parameter: memory[ip + 1])
                relativeBase += offset
                ip += 2
            default:
                fatalError("Unknown opcode: \(opcode)")
            }
        }
        return outputQueue
    }

    private func getValue(mode: Int, parameter: Int) -> Int {
        let address: Int
        switch mode {
        case 0: address = parameter
        case 1: return parameter
        case 2: address = relativeBase + parameter
        default: fatalError("Unknown parameter mode: \(mode)")
        }

        guard address >= 0 && address < memory.count else {
            fatalError("Memory access out of bounds at \(address) (getValue)")
        }
        return memory[address]
    }

    private func getAddress(mode: Int, parameter: Int) -> Int {
        let address: Int
        switch mode {
        case 0: address = parameter
        case 2: address = relativeBase + parameter
        default: fatalError("Invalid mode for address: \(mode)")
        }

        guard address >= 0 && address < memory.count else {
            fatalError("Memory access out of bounds at \(address) (getAddress)")
        }

        return address
    }
}

func main() {
    guard CommandLine.arguments.count > 1,
        let content = try? String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
    else {
        print("Usage: \(CommandLine.arguments[0]) <input_file>")
        return
    }

    let program = content.split(separator: ",").map { Int($0)! }

    let computer = IntcodeComputer(program: program)

    let commands = [
        "north",
        "east",
        "south",
        "take hypercube",
        "north",
        "west",
        "north",
        "east",
        "take tambourine",
        "west",
        "west",
        "take spool of cat6",
        "north",
        "take weather machine",
        "west",
        "west",
        "west",
    ]

    for command in commands {
        let _: [Int] = computer.run()
        let input = command.map { Int($0.asciiValue!) } + [10]
        let output = computer.run(inputs: input)
        let outputString = output.map { String(UnicodeScalar($0)!) }.joined()
        outputString.split(separator: "\n").forEach {
            if $0.contains("Oh, hello!") {
                let words = $0.split(separator: " ")
                let number = words.first { Int($0) != nil }
                print("Part 1:", number!)
            }
        }
    }
}

main()
