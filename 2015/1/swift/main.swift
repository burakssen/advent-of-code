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

