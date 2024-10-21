import Foundation

// Helper to get the parameter value based on mode
func get_parameter_value(memory: [Int], index: Int, mode: Int) -> Int {
    return mode == 0 ? memory[memory[index]] : memory[index]
}

// Function to run the Intcode program with variable input
func run_intcode_program(memory: inout [Int], input: Int) -> Int {
    var instruction_pointer = 0
    var output = 0

    while instruction_pointer < memory.count {
        let instruction = memory[instruction_pointer]
        let opcode = instruction % 100

        // Extract parameter modes
        let param_modes = [
            (instruction / 100) % 10,
            (instruction / 1000) % 10
        ]

        switch opcode {
        case 1: // Addition
            let param1 = get_parameter_value(memory: memory, index: instruction_pointer + 1, mode: param_modes[0])
            let param2 = get_parameter_value(memory: memory, index: instruction_pointer + 2, mode: param_modes[1])
            let target = memory[instruction_pointer + 3]
            memory[target] = param1 + param2
            instruction_pointer += 4

        case 2: // Multiplication
            let param1 = get_parameter_value(memory: memory, index: instruction_pointer + 1, mode: param_modes[0])
            let param2 = get_parameter_value(memory: memory, index: instruction_pointer + 2, mode: param_modes[1])
            let target = memory[instruction_pointer + 3]
            memory[target] = param1 * param2
            instruction_pointer += 4

        case 3: // Input
            let target = memory[instruction_pointer + 1]
            memory[target] = input
            instruction_pointer += 2

        case 4: // Output
            output = get_parameter_value(memory: memory, index: instruction_pointer + 1, mode: param_modes[0])
            instruction_pointer += 2

        case 5: // Jump-if-true
            let param1 = get_parameter_value(memory: memory, index: instruction_pointer + 1, mode: param_modes[0])
            let param2 = get_parameter_value(memory: memory, index: instruction_pointer + 2, mode: param_modes[1])
            instruction_pointer = param1 != 0 ? param2 : instruction_pointer + 3

        case 6: // Jump-if-false
            let param1 = get_parameter_value(memory: memory, index: instruction_pointer + 1, mode: param_modes[0])
            let param2 = get_parameter_value(memory: memory, index: instruction_pointer + 2, mode: param_modes[1])
            instruction_pointer = param1 == 0 ? param2 : instruction_pointer + 3

        case 7: // Less than
            let param1 = get_parameter_value(memory: memory, index: instruction_pointer + 1, mode: param_modes[0])
            let param2 = get_parameter_value(memory: memory, index: instruction_pointer + 2, mode: param_modes[1])
            let target = memory[instruction_pointer + 3]
            memory[target] = param1 < param2 ? 1 : 0
            instruction_pointer += 4

        case 8: // Equals
            let param1 = get_parameter_value(memory: memory, index: instruction_pointer + 1, mode: param_modes[0])
            let param2 = get_parameter_value(memory: memory, index: instruction_pointer + 2, mode: param_modes[1])
            let target = memory[instruction_pointer + 3]
            memory[target] = param1 == param2 ? 1 : 0
            instruction_pointer += 4

        case 99: // Halt
            return output

        default:
            fatalError("Unknown opcode encountered: \(opcode)")
        }
    }

    return output
}

func main() {
    guard let filename = CommandLine.arguments.dropFirst().first,
          let content = try? String(contentsOfFile: filename, encoding: .utf8) else {
        print("Usage: swift main.swift <input_file>")
        return
    }

    // Parse the input
    var memory = content.trimmingCharacters(in: .whitespacesAndNewlines)
        .split(separator: ",")
        .compactMap { Int($0) }

    // Part 1: Run the Intcode program with input 1
    let diagnostic_code_part1 = run_intcode_program(memory: &memory, input: 1)
    print("Part 1: \(diagnostic_code_part1)")

    // Reset and run Part 2 with input 5
    memory = content.trimmingCharacters(in: .whitespacesAndNewlines)
        .split(separator: ",")
        .compactMap { Int($0) }

    let diagnostic_code_part2 = run_intcode_program(memory: &memory, input: 5)
    print("Part 2: \(diagnostic_code_part2)")
}

main()
