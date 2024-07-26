import Foundation

func main() {
    // get command line arguments
    let arguments = CommandLine.arguments
    if arguments.count < 2 {
        print("Usage: swift main.swift <input_file>")
        return
    }
    let input_file = arguments[1]
    
    // read input file
    let fileURL = URL(fileURLWithPath: input_file)
    let contents = try! String(contentsOf: fileURL)
    let lines = contents.split(separator: "\n")
    var paper_size = 0
    var ribbon_length = 0
    for line in lines {
        // split line by x
        let parts = line.split(separator: "x")
        let l = Int(parts[0])!
        let w = Int(parts[1])!
        let h = Int(parts[2])!

        var sides = [l, w, h].sorted()

        sides.sort()

        ribbon_length += 2*sides[0] + 2*sides[1] + l*w*h

        // calculate surface area
        let area = 2*l*w + 2*w*h + 2*h*l

        // calculate slack
        let slack = min(l*w, w*h, h*l)

        // calculate total
        paper_size += area + slack
    }
    print("Part 1: \(paper_size)")
    print("Part 2: \(ribbon_length)")
}

main()