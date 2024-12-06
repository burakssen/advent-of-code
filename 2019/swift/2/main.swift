import Foundation

typealias Memory = [Int]

enum Opcode: Int {
    case addition = 1
    case multiplication = 2
    case halt = 99
}

func runIntcodeProgram(_ program: Memory) -> Memory {
    var memory = program
    var instructionPointer = 0

    while true {
        guard let opcode = Opcode(rawValue: memory[instructionPointer]) else {
            print("Unknown opcode: \(memory[instructionPointer])")
            return memory
        }

        switch opcode {
        case .addition:
            performOperation(&memory, at: instructionPointer, operation: +)
            instructionPointer += 4
        case .multiplication:
            performOperation(&memory, at: instructionPointer, operation: *)
            instructionPointer += 4
        case .halt:
            return memory
        }
    }
}

private func performOperation(_ memory: inout Memory, at pointer: Int, operation: (Int, Int) -> Int) {
    let input1Index = memory[pointer + 1]
    let input2Index = memory[pointer + 2]
    let outputIndex = memory[pointer + 3]
    memory[outputIndex] = operation(memory[input1Index], memory[input2Index])
}

func findNounAndVerb(for targetOutput: Int, with originalProgram: Memory) -> (noun: Int, verb: Int)? {
    for noun in 0...99 {
        for verb in 0...99 {
            var program = originalProgram
            program[1] = noun
            program[2] = verb

            if runIntcodeProgram(program)[0] == targetOutput {
                return (noun, verb)
            }
        }
    }
    return nil
}

func main() {
    let args = CommandLine.arguments
    guard args.count >= 2, let filename = args[1] as String? else {
        print("Usage: swift main.swift <input_file>")
        return
    }

    guard let content = try? String(contentsOfFile: filename, encoding: .utf8) else {
        print("Could not read file \(filename)")
        return
    }

    let program = content.split(separator: ",").compactMap { Int($0) }

    // Part 1: Run with fixed values
    var modifiedProgram = program
    modifiedProgram[1] = 12
    modifiedProgram[2] = 2
    let finalMemoryPart1 = runIntcodeProgram(modifiedProgram)
    print("Part 1: \(finalMemoryPart1[0])")

    // Part 2: Find noun and verb for target output
    let targetOutput = 19690720
    if let (noun, verb) = findNounAndVerb(for: targetOutput, with: program) {
        let result = 100 * noun + verb
        print("Part 2: \(result)")
    } else {
        print("No combination of noun and verb produces the target output.")
    }
}

main()
