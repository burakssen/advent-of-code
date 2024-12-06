import Foundation

class IntcodeComputer {
    var memory: [Int]
    var ip = 0
    var relativeBase = 0
    var inputs: [Int] = []
    var outputs: [Int] = []

    init(program: String) {
        self.memory = program.split(separator: ",").compactMap { Int($0) }
        memory.append(contentsOf: Array(repeating: 0, count: 10000))
    }

    private func getValue(mode: Int, parameter: Int) -> Int {
        switch mode {
        case 0: return memory[parameter]
        case 1: return parameter
        case 2: return memory[relativeBase + parameter]
        default: fatalError("Unknown parameter mode: \(mode)")
        }
    }

    private func getAddress(mode: Int, parameter: Int) -> Int {
        switch mode {
        case 0: return parameter
        case 2: return relativeBase + parameter
        default: fatalError("Invalid mode for address: \(mode)")
        }
    }

    func run() -> Int? {
        while true {
            let instruction = memory[ip]
            let opcode = instruction % 100
            let modes = [
                (instruction / 100) % 10,
                (instruction / 1000) % 10,
                (instruction / 10000) % 10,
            ]

            switch opcode {
            case 1, 2, 7, 8:
                executeBinaryOperation(opcode: opcode, modes: modes)
            case 3:
                if inputs.isEmpty { return nil }
                let address = getAddress(mode: modes[0], parameter: memory[ip + 1])
                memory[address] = inputs.removeFirst()
                ip += 2
            case 4:
                let value = getValue(mode: modes[0], parameter: memory[ip + 1])
                outputs.append(value)
                ip += 2
            case 5, 6:
                executeJumpOperation(opcode: opcode, modes: modes)
            case 9:
                let value = getValue(mode: modes[0], parameter: memory[ip + 1])
                relativeBase += value
                ip += 2
            case 99:
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
        case 1: memory[address] = value1 + value2
        case 2: memory[address] = value1 * value2
        case 7: memory[address] = (value1 < value2) ? 1 : 0
        case 8: memory[address] = (value1 == value2) ? 1 : 0
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

class Network {
    var computers: [IntcodeComputer]
    var queues: [[Int]] = Array(repeating: [], count: 50)
    var nat: (x: Int, y: Int)?
    var lastNatY: Int?

    init(program: String) {
        computers = (0..<50).map { _ in IntcodeComputer(program: program) }

        // Initialize computers with their network addresses
        for (index, computer) in computers.enumerated() {
            computer.inputs.append(index)
        }
    }

    func runPartOne() -> Int {
        while true {
            for i in 0..<computers.count {
                let computer = computers[i]

                // If no input, provide -1
                if computer.inputs.isEmpty {
                    computer.inputs.append(-1)
                }

                _ = computer.run()

                // Process outputs in groups of 3
                while computer.outputs.count >= 3 {
                    let address = computer.outputs.removeFirst()
                    let x = computer.outputs.removeFirst()
                    let y = computer.outputs.removeFirst()

                    if address == 255 {
                        return y
                    }

                    computers[address].inputs.append(x)
                    computers[address].inputs.append(y)
                }
            }
        }
    }

    func runPartTwo() -> Int {
        while true {
            var networkIdle = true

            for i in 0..<computers.count {
                let computer = computers[i]

                // If no input, provide -1
                if computer.inputs.isEmpty {
                    computer.inputs.append(-1)
                } else {
                    networkIdle = false
                }

                _ = computer.run()

                // Process outputs in groups of 3
                while computer.outputs.count >= 3 {
                    let address = computer.outputs.removeFirst()
                    let x = computer.outputs.removeFirst()
                    let y = computer.outputs.removeFirst()

                    if address == 255 {
                        nat = (x, y)
                    } else {
                        networkIdle = false
                        computers[address].inputs.append(x)
                        computers[address].inputs.append(y)
                    }
                }
            }

            // Check for network idle state
            if networkIdle, let nat = nat {
                if let lastY = lastNatY, lastY == nat.y {
                    return nat.y
                }

                lastNatY = nat.y
                computers[0].inputs.append(nat.x)
                computers[0].inputs.append(nat.y)
            }
        }
    }
}

func main() {
    guard CommandLine.arguments.count > 1,
        let content = try? String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
    else {
        print("Usage: \(CommandLine.arguments[0]) <input_file>")
        return
    }

    let network = Network(program: content.trimmingCharacters(in: .whitespacesAndNewlines))

    print("Part 1: \(network.runPartOne())")

    // Reinitialize network for Part Two
    let network2 = Network(program: content.trimmingCharacters(in: .whitespacesAndNewlines))
    print("Part 2: \(network2.runPartTwo())")
}

main()
