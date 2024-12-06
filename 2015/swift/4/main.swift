import Foundation
import CryptoKit

func main(){
    let args = CommandLine.arguments

    if args.count < 2 {
        print("Usage: swift main.swift <input.txt>")
        return
    }

    let filename = args[1]
    let file = FileHandle(forReadingAtPath: filename)

    if file == nil {
        print("Cannot open file")
        return
    }

    let data = file!.readDataToEndOfFile()
    let prefix_text = String(data: data, encoding: .utf8)!

    var i = 0
    var leading_5_found = false

    while(true){
        let input = prefix_text + String(i)
        let hash = Insecure.MD5.hash(data: Data(input.utf8))

        let hash_string = hash.map { String(format: "%02hhx", $0) }.joined()

        if (hash_string.starts(with: "000000") ){
            print("Part 2: \(i)")
            
            break
        }

        if(hash_string.starts(with: "00000") && !leading_5_found){
            print("Part 1: \(i)")
            leading_5_found = true
        }

        i+=1
    }

}

main()