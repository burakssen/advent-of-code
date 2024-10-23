import Foundation

// Part 1 functions remain the same
func parseLayers(input: String, width: Int, height: Int) -> [[String]] {
    let pixelsPerLayer = width * height
    let chars = Array(input.trimmingCharacters(in: .whitespacesAndNewlines))
    var layers: [[String]] = []

    var currentIndex = 0
    while currentIndex < chars.count {
        let endIndex = min(currentIndex + pixelsPerLayer, chars.count)
        let layer = Array(chars[currentIndex..<endIndex]).map(String.init)
        layers.append(layer)
        currentIndex += pixelsPerLayer
    }

    return layers
}

func countDigit(_ digit: String, in layer: [String]) -> Int {
    return layer.filter { $0 == digit }.count
}

// Part 2 functions
func decodeImage(layers: [[String]], width: Int, height: Int) -> [[String]] {
    var finalImage = Array(repeating: Array(repeating: "", count: width), count: height)

    for y in 0..<height {
        for x in 0..<width {
            let pixelIndex = y * width + x

            // Find first non-transparent pixel
            for layer in layers {
                let pixel = layer[pixelIndex]
                if pixel != "2" {  // not transparent
                    finalImage[y][x] = pixel
                    break
                }
            }
        }
    }

    return finalImage
}

func printImage(_ image: [[String]]) {
    for row in image {
        let printedRow = row.map { $0 == "1" ? "█" : " " }  // Use block character for white pixels
        print(" ", printedRow.joined())
    }
}

func main() {
    guard CommandLine.arguments.count >= 2 else {
        print("Usage: swift main.swift <input_file>")
        return
    }

    guard let content = try? String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
    else {
        print("Could not read input file")
        return
    }

    let width = 25
    let height = 6
    let layers = parseLayers(input: content, width: width, height: height)

    // Part 1
    var fewestZeros = Int.max
    var targetLayer: [String] = []

    for layer in layers {
        let zeroCount = countDigit("0", in: layer)
        if zeroCount < fewestZeros {
            fewestZeros = zeroCount
            targetLayer = layer
        }
    }

    let oneCount = countDigit("1", in: targetLayer)
    let twoCount = countDigit("2", in: targetLayer)
    let part1Result = oneCount * twoCount
    print("Part 1: \(part1Result)")
    // Part 2
    print("Part 2:")
    let finalImage = decodeImage(layers: layers, width: width, height: height)
    printImage(finalImage)
}

main()
