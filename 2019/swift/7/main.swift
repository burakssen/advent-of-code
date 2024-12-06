import Foundation

enum Parameter {
    case position(Int)
    case immediate(Int)

    static func parse(from opcode: Int, position: Int, value: Int) -> Parameter {
        let mode = (opcode / Int(pow(10.0, Double(position + 1)))) % 10
        return mode == 0 ? .position(value) : .immediate(value)
    }

    func read(_ memory: [Int]) -> Int {
        switch self {
        case .position(let pos): return memory[pos]
        case .immediate(let val): return val
        }
    }
}

enum Instruction {
    case add(Parameter, Parameter, Int)
    case multiply(Parameter, Parameter, Int)
    case input(Int)
    case output(Parameter)
    case jumpIfTrue(Parameter, Parameter)
    case jumpIfFalse(Parameter, Parameter)
    case lessThan(Parameter, Parameter, Int)
    case equals(Parameter, Parameter, Int)
    case halt

    static func decode(_ memory: [Int], at pc: Int) -> Instruction {
        let opcode = memory[pc]
        let p1 = { Parameter.parse(from: opcode, position: 1, value: memory[pc + 1]) }
        let p2 = { Parameter.parse(from: opcode, position: 2, value: memory[pc + 2]) }
        let dest = { memory[pc + 3] }

        switch opcode % 100 {
        case 1: return .add(p1(), p2(), dest())
        case 2: return .multiply(p1(), p2(), dest())
        case 3: return .input(memory[pc + 1])
        case 4: return .output(p1())
        case 5: return .jumpIfTrue(p1(), p2())
        case 6: return .jumpIfFalse(p1(), p2())
        case 7: return .lessThan(p1(), p2(), dest())
        case 8: return .equals(p1(), p2(), dest())
        case 99: return .halt
        default: fatalError("Invalid opcode \(opcode) at position \(pc)")
        }
    }

    var size: Int {
        switch self {
        case .add, .multiply, .lessThan, .equals: return 4
        case .jumpIfTrue, .jumpIfFalse: return 3
        case .input, .output: return 2
        case .halt: return 1
        }
    }
}

final class IntcodeComputer {
    private var memory: [Int]
    private var pc: Int = 0
    private var inputs: [Int]
    private var inputIndex: Int = 0

    init(program: [Int], inputs: [Int] = []) {
        self.memory = program
        self.inputs = inputs
    }

    enum ExecutionResult {
        case needInput
        case output(Int)
        case halt
        case jump(Int)
        case cont
    }

    func execute() -> [Int] {
        var outputs: [Int] = []
        while true {
            switch step() {
            case .output(let value): outputs.append(value)
            case .halt: return outputs
            case .needInput: fatalError("Insufficient input")
            default: continue
            }
        }
    }

    func executeUntilOutput(with input: Int) -> Int? {
        inputs.append(input)
        while true {
            switch step() {
            case .output(let value): return value
            case .halt: return nil
            case .needInput: fatalError("Insufficient input")
            default: continue
            }
        }
    }

    private func step() -> ExecutionResult {
        let instruction = Instruction.decode(memory, at: pc)
        let result = executeInstruction(instruction)

        switch result {
        case .cont, .output:
            pc += instruction.size
        case .jump(let address):
            pc = address
        case .needInput, .halt:
            break
        }
        return result
    }

    private func executeInstruction(_ instruction: Instruction) -> ExecutionResult {
        switch instruction {
        case .add(let a, let b, let dest):
            memory[dest] = a.read(memory) + b.read(memory)
        case .multiply(let a, let b, let dest):
            memory[dest] = a.read(memory) * b.read(memory)
        case .input(let dest):
            guard inputIndex < inputs.count else { return .needInput }
            memory[dest] = inputs[inputIndex]
            inputIndex += 1
        case .output(let value):
            return .output(value.read(memory))
        case .jumpIfTrue(let test, let target):
            if test.read(memory) != 0 { return .jump(target.read(memory)) }
        case .jumpIfFalse(let test, let target):
            if test.read(memory) == 0 { return .jump(target.read(memory)) }
        case .lessThan(let a, let b, let dest):
            memory[dest] = a.read(memory) < b.read(memory) ? 1 : 0
        case .equals(let a, let b, let dest):
            memory[dest] = a.read(memory) == b.read(memory) ? 1 : 0
        case .halt:
            return .halt
        }
        return .cont
    }
}

struct AmplifierCircuit {
    static func findMaxSignal(program: [Int], phaseRange: ClosedRange<Int>, feedback: Bool = false)
        -> Int
    {
        phaseRange.permutations().map { phases in
            feedback
                ? runFeedbackLoop(program: program, phases: phases)
                : runChain(program: program, phases: phases)
        }.max() ?? 0
    }

    private static func runChain(program: [Int], phases: [Int]) -> Int {
        phases.reduce(0) { signal, phase in
            IntcodeComputer(program: program, inputs: [phase, signal]).execute()[0]
        }
    }

    private static func runFeedbackLoop(program: [Int], phases: [Int]) -> Int {
        let amplifiers = phases.map { IntcodeComputer(program: program, inputs: [$0]) }
        var signal = 0

        while true {
            var halted = true
            for amp in amplifiers {
                if let output = amp.executeUntilOutput(with: signal) {
                    signal = output
                    halted = false
                }
            }
            if halted { break }
        }
        return signal
    }
}

extension ClosedRange where Bound == Int {
    func permutations() -> [[Int]] {
        Array(self).permutations()
    }
}

extension Array where Element == Int {
    func permutations() -> [[Int]] {
        guard count > 1 else { return [self] }
        return indices.flatMap { i -> [[Int]] in
            var arr = self
            let element = arr.remove(at: i)
            return arr.permutations().map { [element] + $0 }
        }
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
        .components(separatedBy: ",")
        .compactMap(Int.init)

    print("Part 1:", AmplifierCircuit.findMaxSignal(program: program, phaseRange: 0...4))
    print(
        "Part 2:",
        AmplifierCircuit.findMaxSignal(program: program, phaseRange: 5...9, feedback: true))
}

main()
