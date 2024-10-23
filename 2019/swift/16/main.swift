import Foundation

// FFT implementation for Part 1
func fft(signal: [Int], phases: Int) -> [Int] {
    var input = signal
    let length = signal.count
    let patterns = (0..<length).map { buildPattern(for: $0 + 1, length: length) }

    for _ in 0..<phases {
        input = performFFT(input: input, patterns: patterns)
    }
    return input
}

// Perform one phase of FFT
func performFFT(input: [Int], patterns: [[Int]]) -> [Int] {
    let length = input.count
    var output = [Int](repeating: 0, count: length)

    DispatchQueue.concurrentPerform(iterations: length) { i in
        let sum = (0..<length).reduce(0) { acc, j in
            acc + input[j] * patterns[i][j]
        }
        output[i] = abs(sum) % 10
    }
    return output
}

// Build the FFT pattern for a given position
func buildPattern(for position: Int, length: Int) -> [Int] {
    let basePattern = [0, 1, 0, -1]
    return Array((0..<(length + 1)).map { basePattern[($0 / position) % 4] }.dropFirst())
}

// Optimized FFT implementation for Part 2
func fftOptimizedPart2(signal: [Int], phases: Int, messageOffset: Int) -> [Int] {
    var input = signal

    for _ in 0..<phases {
        var sum = 0
        for i in stride(from: input.count - 1, through: messageOffset, by: -1) {
            sum = (sum + input[i]) % 10
            input[i] = sum
        }
    }
    return input
}

// Main function to handle FFT processing
func main() {
    guard CommandLine.arguments.count > 1,
        let content = try? String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
    else {
        print("Usage: \(CommandLine.arguments[0]) <input_file>")
        return
    }

    let signal = content.trimmingCharacters(in: .whitespacesAndNewlines)
        .compactMap { $0.wholeNumberValue }

    // Part One
    let resultPartOne = fft(signal: signal, phases: 100)
    print("Part 1: \(resultPartOne.prefix(8).map(String.init).joined())")

    // Part Two
    let messageOffset = signal.prefix(7).reduce(0) { $0 * 10 + $1 }
    let repeatedSignal = Array(repeating: signal, count: 10_000).flatMap { $0 }

    guard messageOffset >= repeatedSignal.count / 2 else {
        fatalError("Message offset must be in the latter half of the signal")
    }

    let finalResult = fftOptimizedPart2(
        signal: repeatedSignal, phases: 100, messageOffset: messageOffset)
    let message = finalResult[messageOffset..<messageOffset + 8].map(String.init).joined()
    print("Part 2: \(message)")
}

// Entry point
main()
