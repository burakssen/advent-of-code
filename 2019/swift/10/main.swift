import Foundation

// Represents a point in 2D space
struct Point: Hashable {
    let x: Int
    let y: Int

    func angle(to other: Point) -> Double {
        let dx = Double(other.x - x)
        let dy = Double(other.y - y)
        // Use atan2 to get angle in radians, convert to degrees
        // Adjust to make 0 degrees point up and increase clockwise
        var angle = (atan2(dx, -dy) * 180.0 / .pi)
        if angle < 0 {
            angle += 360
        }
        return angle
    }

    func distance(to other: Point) -> Double {
        let dx = Double(other.x - x)
        let dy = Double(other.y - y)
        return sqrt(dx * dx + dy * dy)
    }
}

class AsteroidField {
    var asteroids: Set<Point> = []
    var width = 0
    var height = 0

    init(input: String) {
        let lines = input.split(separator: "\n")
        height = lines.count
        width = lines[0].count
        for (y, line) in lines.enumerated() {
            for (x, char) in line.enumerated() {
                if char == "#" {
                    asteroids.insert(Point(x: x, y: y))
                }
            }
        }
    }

    func countVisibleAsteroids(from station: Point) -> Int {
        var visibleCount = 0
        var angleMap: [Double: Double] = [:]

        for asteroid in asteroids {
            if asteroid == station { continue }
            let angle = station.angle(to: asteroid)
            let distance = station.distance(to: asteroid)

            // If we haven't seen this angle before, or if this asteroid is closer
            // than the previous one at this angle, it's visible
            if angleMap[angle] == nil || distance < angleMap[angle]! {
                if angleMap[angle] == nil {
                    visibleCount += 1
                }
                angleMap[angle] = distance
            }
        }
        return visibleCount
    }

    func findBestLocation() -> (location: Point, count: Int) {
        var bestLocation = Point(x: 0, y: 0)
        var maxVisible = 0

        for asteroid in asteroids {
            let visible = countVisibleAsteroids(from: asteroid)
            if visible > maxVisible {
                maxVisible = visible
                bestLocation = asteroid
            }
        }
        return (bestLocation, maxVisible)
    }

    // New method for Part 2
    func vaporizeAsteroids(from station: Point) -> [Point] {
        var vaporizedOrder: [Point] = []
        var remainingAsteroids = asteroids
        remainingAsteroids.remove(station)

        while !remainingAsteroids.isEmpty {
            // Group asteroids by angle
            var asteroidsAtAngle: [Double: [(point: Point, distance: Double)]] = [:]

            for asteroid in remainingAsteroids {
                let angle = station.angle(to: asteroid)
                let distance = station.distance(to: asteroid)
                if asteroidsAtAngle[angle] == nil {
                    asteroidsAtAngle[angle] = []
                }
                asteroidsAtAngle[angle]?.append((asteroid, distance))
            }

            // Sort asteroids at each angle by distance
            for angle in asteroidsAtAngle.keys {
                asteroidsAtAngle[angle]?.sort { $0.distance < $1.distance }
            }

            // Get all angles and sort them
            let angles = asteroidsAtAngle.keys.sorted()

            // Vaporize the closest asteroid at each angle
            for angle in angles {
                if let closestAsteroid = asteroidsAtAngle[angle]?.first?.point {
                    vaporizedOrder.append(closestAsteroid)
                    remainingAsteroids.remove(closestAsteroid)
                }
            }
        }

        return vaporizedOrder
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

    let field = AsteroidField(input: content)
    let result = field.findBestLocation()
    print("Part 1: \(result.count)")

    // Part 2
    let vaporizedAsteroids = field.vaporizeAsteroids(from: result.location)
    if vaporizedAsteroids.count >= 200 {
        let asteroid200 = vaporizedAsteroids[199]  // 0-based index for 200th
        let answer = asteroid200.x * 100 + asteroid200.y
        print("Part 2: \(answer)")
    } else {
        print("Not enough asteroids to vaporize 200!")
    }
}

main()
