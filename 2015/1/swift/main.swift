import Foundation

func main(){
    // get command line arguments
    let args = CommandLine.arguments
    // check if there are enough arguments
    if args.count < 2 {
        print("Usage: swift main.swift <input_file>")
        return
    }

}main()