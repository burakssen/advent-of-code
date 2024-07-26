import Foundation

protocol Moveable {
    mutating func move(direction: Character)
}

struct Position: Equatable {
    var x: Int
    var y: Int

    static func == (lhs: Position, rhs: Position) -> Bool {
        return lhs.x == rhs.x && lhs.y == rhs.y
    }
}

extension Position: Moveable {
    mutating func move(direction: Character) {
        switch direction {
        case "^":
            self.y += 1
        case "v":
            self.y -= 1
        case "<":
            self.x -= 1
        case ">":
            self.x += 1
        default:
            break
        }
    }
}

func main() {
    let arguments = CommandLine.arguments

    if arguments.count < 2 {
        print("Usage: swift main.swift <input_file>")
        return
    }

    let inputFilePath = arguments[1]
    let input = try! String(contentsOfFile: inputFilePath)
    let lines = input.split(separator: "\n")

    var current_pos = Position(x: 0, y: 0)

    // hashmap of visited positions
    var visited = [Position]()
    visited.append(current_pos) 


    for line in lines {
        for char in line {
            current_pos.move(direction: char)
            if !visited.contains(current_pos) {
                visited.append(current_pos)
            }
        }
    }

    print("Part 1: \(visited.count)")
}

main()