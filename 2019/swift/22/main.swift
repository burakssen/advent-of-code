import Foundation
import Numerics

typealias BigInt = Int128

extension BigInt {
    func mod(_ n: BigInt) -> BigInt {
        let result = self % n
        return result < 0 ? result + n : result
    }
}

/// Perform a new stack shuffle on the index `i`.
func newStack(_ i: BigInt, N: BigInt) -> BigInt {
    return N - i - 1
}

/// Perform a cut operation on the index `i` by `n`.
func cut(_ i: BigInt, _ n: BigInt, N: BigInt) -> BigInt {
    return (i - n).mod(N)
}

/// Perform a deal operation on the index `i` with increment `n`.
func deal(_ i: BigInt, _ n: BigInt, N: BigInt) -> BigInt {
    return (i * n).mod(N)
}

/// Tracks the position after all shuffle instructions.
func trackPosition(_ i: BigInt, shuffles: [[String]], N: BigInt) -> BigInt {
    var position = i
    for shuffle in shuffles {
        switch shuffle[0] {
        case "cut":
            position = cut(position, BigInt(shuffle[1])!, N: N)
        case "deal" where shuffle[2] == "increment":
            position = deal(position, BigInt(shuffle[3])!, N: N)
        case "deal" where shuffle.last == "stack":
            position = newStack(position, N: N)
        default:
            break
        }
    }
    return position
}

// MARK: - Part 1

/// Solves Part 1 by tracking the position after shuffling.
func solvePart1(shuffles: [[String]], N: BigInt) {
    let result = trackPosition(2019, shuffles: shuffles, N: N)
    print("Part 1: \(result)")
}

// MARK: - Part 2

/// Extended Euclidean algorithm to find the greatest common divisor.
func egcd(_ a: BigInt, _ b: BigInt) -> (BigInt, BigInt, BigInt) {
    var a = a
    var b = b
    var x: BigInt = 0
    var y: BigInt = 1
    var u: BigInt = 1
    var v: BigInt = 0

    while a != 0 {
        let q = b / a
        let r = b % a
        let m = x - u * q
        let n = y - v * q
        b = a
        a = r
        x = u
        y = v
        u = m
        v = n
    }
    return (b, x, y)
}

/// Finds the modular inverse of `a` modulo `m`.
func modinv(_ a: BigInt, _ m: BigInt) -> BigInt? {
    let (g, x, _) = egcd(a, m)
    return g == 1 ? (x % m + m) % m : nil
}

/// Solves Part 2 by computing the polynomial based on shuffle instructions.
func solvePart2(shuffles: [[String]], N2: BigInt, REP: BigInt) {
    let p0 = trackPosition(0, shuffles: shuffles, N: N2)
    let p1 = trackPosition(1, shuffles: shuffles, N: N2)

    let a1 = (p1 - p0).mod(N2)
    let b1 = p0
    guard let aT = modinv(a1, N2) else { return }

    let bT = (-aT * b1).mod(N2)

    func poly(_ x: BigInt) -> BigInt {
        let powAT = powMod(aT, REP, N2)
        let term1 = (powAT * x).mod(N2)
        let term2 = ((powAT - 1) * modinv(aT - 1, N2)!).mod(N2)
        let term3 = (term2 * bT).mod(N2)
        return (term1 + term3).mod(N2)
    }

    print("Part 2: \(poly(2020).mod(N2))")
}

func main() {

    // Read shuffle instructions
    guard CommandLine.arguments.count > 1,
        let content = try? String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
    else {
        print("Usage: \(CommandLine.arguments[0]) <input_file>")
        exit(1)
    }

    let shuffles = content.split(separator: "\n").map { $0.split(separator: " ").map(String.init) }

    let N: BigInt = 10007
    let N2: BigInt = 119_315_717_514_047
    let REP: BigInt = 101_741_582_076_661

    solvePart1(shuffles: shuffles, N: N)
    solvePart2(shuffles: shuffles, N2: N2, REP: REP)
}

// MARK: - Power Modulo Function

/// Computes `base^exp % mod` efficiently using exponentiation by squaring.
func powMod(_ base: BigInt, _ exp: BigInt, _ mod: BigInt) -> BigInt {
    var result: BigInt = 1
    var base = base.mod(mod)
    var exp = exp

    while exp > 0 {
        if exp % 2 == 1 {
            result = (result * base).mod(mod)
        }
        base = (base * base).mod(mod)
        exp /= 2
    }
    return result
}

main()
