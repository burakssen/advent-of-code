import Foundation

// Struct to represent a 3D vector (position or velocity)
struct Vector3 {
    var x, y, z: Int

    static func + (lhs: Vector3, rhs: Vector3) -> Vector3 {
        Vector3(x: lhs.x + rhs.x, y: lhs.y + rhs.y, z: lhs.z + rhs.z)
    }

    static func - (lhs: Vector3, rhs: Vector3) -> Vector3 {
        Vector3(x: lhs.x - rhs.x, y: lhs.y - rhs.y, z: lhs.z - rhs.z)
    }

    mutating func add(_ other: Vector3) {
        self = self + other
    }

    func energy() -> Int {
        abs(x) + abs(y) + abs(z)
    }
}

// Struct to represent a Moon with position and velocity
struct Moon {
    var position: Vector3
    var velocity: Vector3 = Vector3(x: 0, y: 0, z: 0)

    mutating func applyVelocity() {
        position.add(velocity)
    }

    func totalEnergy() -> Int {
        position.energy() * velocity.energy()
    }
}

// Function to apply gravity to a pair of moons
func applyGravity(_ moon1: inout Moon, _ moon2: inout Moon) {
    let dx = moon1.position.x - moon2.position.x
    let dy = moon1.position.y - moon2.position.y
    let dz = moon1.position.z - moon2.position.z

    if dx > 0 {
        moon1.velocity.x -= 1
        moon2.velocity.x += 1
    } else if dx < 0 {
        moon1.velocity.x += 1
        moon2.velocity.x -= 1
    }

    if dy > 0 {
        moon1.velocity.y -= 1
        moon2.velocity.y += 1
    } else if dy < 0 {
        moon1.velocity.y += 1
        moon2.velocity.y -= 1
    }

    if dz > 0 {
        moon1.velocity.z -= 1
        moon2.velocity.z += 1
    } else if dz < 0 {
        moon1.velocity.z += 1
        moon2.velocity.z -= 1
    }
}

// Simulate one step of gravity and velocity for moons
func simulateStep(_ moons: inout [Moon]) {
    let count = moons.count
    for i in 0..<count {
        for j in (i + 1)..<count {
            var moon1 = moons[i]
            var moon2 = moons[j]
            applyGravity(&moon1, &moon2)
            moons[i] = moon1
            moons[j] = moon2
        }
    }

    for i in 0..<count {
        moons[i].applyVelocity()
    }
}

// Compute total energy of all moons
func totalEnergy(_ moons: [Moon]) -> Int {
    moons.reduce(0) { $0 + $1.totalEnergy() }
}

// Find the cycle length for a given axis
func findCycleLength(_ moons: inout [Moon], axis: KeyPath<Vector3, Int>) -> Int {
    var seen = Set<[Int]>()
    var step = 0

    while true {
        let state = moons.flatMap { [$0.position[keyPath: axis], $0.velocity[keyPath: axis]] }
        if seen.contains(state) { return step }
        seen.insert(state)
        simulateStep(&moons)
        step += 1
    }
}

// Compute Least Common Multiple (LCM)
func lcm(_ a: Int, _ b: Int) -> Int {
    a * b / gcd(a, b)
}

// Compute Greatest Common Divisor (GCD)
func gcd(_ a: Int, _ b: Int) -> Int {
    var a = a
    var b = b
    while b != 0 { (a, b) = (b, a % b) }
    return a
}

// Main program
func main() {
    guard CommandLine.arguments.count > 1 else {
        print("Usage: \(CommandLine.arguments[0]) <input_file>")
        return
    }

    let filename = CommandLine.arguments[1]
    guard let content = try? String(contentsOfFile: filename, encoding: .utf8) else {
        print("Could not read input file")
        return
    }

    // Parse initial positions
    let initialPositions = content.split(separator: "\n").map { line -> Vector3 in
        let parts = line.replacingOccurrences(
            of: "[<>,xyz=]", with: "", options: .regularExpression
        )
        .split(separator: " ").compactMap { Int($0) }
        return Vector3(x: parts[0], y: parts[1], z: parts[2])
    }

    // Simulate 1000 steps for part 1
    var moons = initialPositions.map { Moon(position: $0) }
    (0..<1000).forEach { _ in simulateStep(&moons) }
    print("Part 1: \(totalEnergy(moons))")

    // Find cycle length for each axis for part 2
    var moonsX = initialPositions.map { Moon(position: $0) }
    let cycleX = findCycleLength(&moonsX, axis: \.x)
    let cycleY = findCycleLength(&moonsX, axis: \.y)
    let cycleZ = findCycleLength(&moonsX, axis: \.z)
    print("Part 2: \(lcm(lcm(cycleX, cycleY), cycleZ))")
}

main()
