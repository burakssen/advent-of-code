import Foundation

typealias Position = (x: Int, y: Int)
typealias Field = [[String]]

struct Program {
    var memory: [Int]
    var base: Int = 0
}

let directions: [Int: Position] = [
    1: (-1, 0), // Up
    2: (1, 0),  // Down
    3: (0, -1), // Left
    4: (0, 1),  // Right
]

let forbidden: [Int: Int] = [
    1: 2,
    2: 1,
    3: 4,
    4: 3,
]

let tiles = ["▒", ".", "W"]
let numOfOperands = [0, 3, 3, 1, 1, 2, 2, 3, 3, 1]

func run(_ program: inout Program, move: Int, pos: Position, pc: Int, field: inout Field) {
    var i = pc

    while program.memory[i] != 99 {
        let (instruction, _, baseTmp, operands) = decodeInstruction(&program, &i)

        switch instruction {
        case 1:
            program.memory[baseTmp[2] + program.memory[i + 3]] = operands[0] + operands[1]
        case 2:
            program.memory[baseTmp[2] + program.memory[i + 3]] = operands[0] * operands[1]
        case 3:
            program.memory[baseTmp[0] + program.memory[i + 1]] = move
        case 4:
            handleOutput(&program, move: move, pos: pos, operands: operands, &field, &i)
            return // Early return after handling output
        case 5:
            i = operands[0] != 0 ? operands[1] - 3 : i
        case 6:
            i = operands[0] == 0 ? operands[1] - 3 : i
        case 7:
            program.memory[baseTmp[2] + program.memory[i + 3]] = operands[0] < operands[1] ? 1 : 0
        case 8:
            program.memory[baseTmp[2] + program.memory[i + 3]] = operands[0] == operands[1] ? 1 : 0
        case 9:
            program.base += operands[0]
        default:
            fatalError("Unknown instruction: \(instruction)")
        }

        i += numOfOperands[instruction] + 1
    }
}

func decodeInstruction(_ program: inout Program, _ i: inout Int) -> (Int, [Int], [Int], [Int]) {
    let opcode = program.memory[i]
    let modes = Array(String(format: "%05d", opcode).prefix(3).map { Int(String($0))! }.reversed())
    let instruction = Int(String(format: "%05d", opcode).suffix(2))!

    let baseTmp = modes.prefix(numOfOperands[instruction]).map { $0 == 2 ? program.base : 0 }
    let operands = (0..<numOfOperands[instruction]).map { idx in
        let value = program.memory[i + idx + 1]
        return modes[idx] == 1 ? value : program.memory[baseTmp[idx] + value]
    }

    return (instruction, modes, baseTmp, operands)
}

func handleOutput(_ program: inout Program, move: Int, pos: Position, operands: [Int], _ field: inout Field, _ i: inout Int) {
    let newPos = Position(x: pos.x + directions[move]!.x, y: pos.y + directions[move]!.y)
    guard newPos.x >= 0, newPos.x < field.count, newPos.y >= 0, newPos.y < field[newPos.x].count else { return }

    field[newPos.x][newPos.y] = tiles[operands[0]]

    if operands[0] > 0 {
        for x in 1...4 where x != forbidden[move] {
            var newProgram = program
            run(&newProgram, move: x, pos: newPos, pc: i + numOfOperands[3] + 1, field: &field)
        }
    }
}

func findPath(_ x: Int, _ y: Int, _ length: Int, _ field: inout Field) -> (Int, Int, Int) {
    guard field[x][y] != "▒" && field[x][y] != "W" else {
        return field[x][y] == "▒" ? (0, 0, 0) : (x, y, length)
    }

    field[x][y] = "▒"

    var maxPath = (0, 0, 0)
    for direction in directions.values {
        var fieldCopy = field
        let result = findPath(x + direction.x, y + direction.y, length + 1, &fieldCopy)
        if abs(result.0) + abs(result.1) > abs(maxPath.0) + abs(maxPath.1) {
            maxPath = result
        }
    }

    field[x][y] = " " // Restore state after recursion
    return maxPath
}

func distance(_ x1: Int, _ y1: Int, _ x2: Int, _ y2: Int, _ length: Int, _ field: inout Field) -> Int {
    guard field[x1][y1] != "▒" else { return 0 }
    guard !(x1 == x2 && y1 == y2) else { return length }

    field[x1][y1] = "▒"

    var maxDistance = 0
    for direction in directions.values {
        var fieldCopy = field
        let dist = distance(x1 + direction.x, y1 + direction.y, x2, y2, length + 1, &fieldCopy)
        maxDistance = max(maxDistance, dist)
    }

    field[x1][y1] = " " // Restore state after recursion
    return maxDistance
}

func main() {
    guard CommandLine.arguments.count > 1,
          let content = try? String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8) else {
        print("Usage: \(CommandLine.arguments[0]) <input_file>")
        return
    }

    let program = Program(
        memory: content.split(separator: ",").compactMap { Int($0) } + Array(repeating: 0, count: 10000)
    )

    var field: Field = Array(repeating: Array(repeating: " ", count: 41), count: 41)
    let start: Position = (21, 21)

    for move in 1...4 {
        var programCopy = program
        run(&programCopy, move: move, pos: start, pc: 0, field: &field)
    }

    field[start.x][start.y] = "S"
    var fieldCopy = field
    let found = findPath(start.x, start.y, 0, &fieldCopy)

    print("Part 1:", found.2)

    let maxDistance = (0..<field.count).flatMap { x in
        (0..<field[x].count).map { y in
            field[x][y] == "." ? distance(found.0, found.1, x, y, 0, &field) : 0
        }
    }.max() ?? 0

    print("Part 2:", maxDistance)
}

main()
