import Foundation

func main(){
    // get command line arguments
    let args = CommandLine.arguments
    // check if there are enough arguments
    if args.count < 2 {
        print("Usage: swift main.swift <input_file>")
        return
    }

    // get the input file
    let input_file = args[1]
    // read the file
    let file = try! String(contentsOfFile: input_file)

    var count: Int = 0;
    // split the file into lines
    let lines = file.split(separator: "\n")
    // iterate over the lines
    for line in lines {
        // iterate over the characters in the line
        for char in line {
            if char == "(" {
                count += 1
            } else if char == ")" {
                count -= 1
            }
        }
    }

    print(count)
}

main()