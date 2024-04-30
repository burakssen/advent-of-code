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
    var total = 0
    for line in lines {
        // split line by x
        let parts = line.split(separator: "x")
        let l = Int(parts[0])!
        let w = Int(parts[1])!
        let h = Int(parts[2])!

        // calculate surface area
        let area = 2*l*w + 2*w*h + 2*h*l

        // calculate slack
        let slack = min(l*w, w*h, h*l)

        // calculate total
        total += area + slack
    }
    print(total)
}

main()