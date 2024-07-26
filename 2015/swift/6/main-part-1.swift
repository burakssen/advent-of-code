import Foundation

struct Action {
    var act: String
    var start: (Int, Int)
    var end: (Int, Int)
}

func get_action(_ line: String) -> Action {
    let parts = line.components(separatedBy: " ")
    if parts[0] == "turn" {
        let action = parts[1]
        let start = parts[2].components(separatedBy: ",").map { Int($0)! }
        let end = parts[4].components(separatedBy: ",").map { Int($0)! }
        return Action(act: action, start: (start[0], start[1]), end: (end[0], end[1]))
    } else {
        let action = parts[0]
        let start = parts[1].components(separatedBy: ",").map { Int($0)! }
        let end = parts[3].components(separatedBy: ",").map { Int($0)! }
        return Action(act: action, start: (start[0], start[1]), end: (end[0], end[1]))
    }
}

func main(){
    let args = CommandLine.arguments
    if args.count < 2 {
        print("Usage: swift main-part1.swift <input>")
        return
    }

    let filename = args[1]

    let input = try! String(contentsOfFile: filename)


    let lines = input.components(separatedBy: "\n")
    
    var grid = Array(repeating: Array(repeating: false, count: 1000), count: 1000)
    
    for line in lines {
        let action = get_action(line)
        for i in action.start.0...action.end.0 {
            for j in action.start.1...action.end.1 {
                if action.act == "on" {
                    grid[i][j] = true
                } else if action.act == "off" {
                    grid[i][j] = false
                } else {
                    grid[i][j] = !grid[i][j]
                }
            }
        }
    }

    var count = 0
    for i in 0..<1000 {
        for j in 0..<1000 {
            if grid[i][j] {
                count += 1
            }
        }
    }

    print("Part 1 \(count)")
}

main()