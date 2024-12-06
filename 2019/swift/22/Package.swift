// swift-tools-version:6.0
import PackageDescription

let package = Package(
    name: "CardShuffling",
    platforms: [
        .macOS(.v15)
    ],
    dependencies: [
        // Swift Numerics for BigInt support
        .package(url: "https://github.com/apple/swift-numerics.git", from: "1.0.0")
    ],
    targets: [
        .executableTarget(
            name: "CardShuffling",
            dependencies: [
                // Add BigInt as a dependency
                .product(name: "Numerics", package: "swift-numerics")
            ],
            path: "."
        )
    ]
)
