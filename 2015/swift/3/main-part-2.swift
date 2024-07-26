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

    var santa = Position(x: 0, y: 0)
    var robo_santa = Position(x: 0, y: 0)

    // hashmap of visited positions
    var visited = [Position]()
    visited.append(santa) 

    var santa_turn = true

    for line in lines {
        for char in line {
            if santa_turn {
                santa.move(direction: char)
                if !visited.contains(santa) {
                    visited.append(santa)
                }
            } else {
                robo_santa.move(direction: char)
                if !visited.contains(robo_santa) {
                    visited.append(robo_santa)
                }
            }
            santa_turn = !santa_turn
        }
    }

    print("Part 2: \(visited.count)")
}

main()