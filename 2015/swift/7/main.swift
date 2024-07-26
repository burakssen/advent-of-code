import Foundation

enum InstructionType {
    case AND(String, String, String)
    case OR(String, String, String)
    case LSHIFT(String, String, String)
    case RSHIFT(String, String, String)
    case NOT(String, String)
    case ASSIGN(String, String)
}

var instructions: [InstructionType] = []
var wires: [String: Int] = [:]

func isNumber(_ str: String) -> Bool {
    return Int(str) != nil
}

func eval(target: String) -> Int {
    if let value = wires[target] {
        return value
    }

    let instruction = instructions.first { instruction in
        switch instruction {
            case .ASSIGN(_, let t): return t == target
            case .AND(_, _, let t): return t == target
            case .OR(_, _, let t): return t == target
            case .LSHIFT(_, _, let t): return t == target
            case .RSHIFT(_, _, let t): return t == target
            case .NOT(_, let t): return t == target
        }
    }!

    let result: Int = {
        switch instruction {
            case .ASSIGN(let s, _):
                let sv = isNumber(s) ? Int(s)! : eval(target: s)
                return sv
            case .AND(let a, let b, _):
                let av = isNumber(a) ? Int(a)! : eval(target: a)
                let bv = isNumber(b) ? Int(b)! : eval(target: b)
                return av & bv
            case .OR(let a, let b, _):
                let av = isNumber(a) ? Int(a)! : eval(target: a)
                let bv = isNumber(b) ? Int(b)! : eval(target: b)
                return av | bv
            case .LSHIFT(let a, let b, _):
                let av = isNumber(a) ? Int(a)! : eval(target: a)
                let bv = isNumber(b) ? Int(b)! : eval(target: b)
                return av << bv
            case .RSHIFT(let a, let b, _):
                let av = isNumber(a) ? Int(a)! : eval(target: a)
                let bv = isNumber(b) ? Int(b)! : eval(target: b)
                return av >> bv
            case .NOT(let a, _):
                let av = isNumber(a) ? Int(a)! : eval(target: a)
                return ~av & 0xffff
        }
    }()

    wires[target] = result
    return result
}

func main() {
    let args = CommandLine.arguments
    if args.count < 2 {
        print("Usage: swift main.swift <input.txt>")
        return
    }

    let filename = args[1]
    guard let file = FileHandle(forReadingAtPath: filename) else {
        print("Cannot open file: \(filename)")
        return
    }

    let data = file.readDataToEndOfFile()
    guard let str = String(data: data, encoding: .utf8) else {
        print("Cannot read file: \(filename)")
        return
    }

    let lines = str.split(separator: "\n")
    for line in lines {
        let words = line.split(separator: " ")
        let instruction: InstructionType
        if words.count == 3 {
            instruction = .ASSIGN(String(words[0]), String(words[2]))
        } else if words.count == 4 {
            instruction = .NOT(String(words[1]), String(words[3]))
        } else if words.count == 5 {
            switch words[1] {
                case "AND":
                    instruction = .AND(String(words[0]), String(words[2]), String(words[4]))
                case "OR":
                    instruction = .OR(String(words[0]), String(words[2]), String(words[4]))
                case "LSHIFT":
                    instruction = .LSHIFT(String(words[0]), String(words[2]), String(words[4]))
                case "RSHIFT":
                    instruction = .RSHIFT(String(words[0]), String(words[2]), String(words[4]))
                default:
                    print("Unknown instruction: \(line)")
                    return
            }
        } else {
            print("Unknown instruction: \(line)")
            return
        }
        instructions.append(instruction)
    }

    var a = eval(target: "a")
    print("Part 1: \(a)")
    wires = [:]
    wires = ["b": a]
    a = eval(target: "a")
    print("Part 2: \(a)")
}

main()
