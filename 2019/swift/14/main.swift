import Foundation

// Represents a chemical quantity pair
struct ChemicalQuantity: Hashable {
    let chemical: String
    let quantity: Int
}

// Represents a chemical reaction
struct Reaction {
    let inputs: [ChemicalQuantity]
    let output: ChemicalQuantity
}

class ChemicalCalculator {
    private var reactions: [String: Reaction] = [:]
    private var surplus: [String: Int] = [:]

    init(input: String) {
        parseReactions(input)
    }

    private func parseReactions(_ input: String) {
        input
            .split(separator: "\n")
            .map(String.init)
            .filter { !$0.isEmpty }
            .forEach { line in
                let parts = line.split(separator: "=>").map {
                    $0.trimmingCharacters(in: .whitespaces)
                }
                guard parts.count == 2 else { return }

                let inputParts = parts[0].split(separator: ",").map {
                    $0.trimmingCharacters(in: .whitespaces)
                }
                let outputComponents = parts[1].split(separator: " ").map { String($0) }

                guard let outputQuantity = Int(outputComponents[0]), outputComponents.count == 2
                else { return }
                let output = ChemicalQuantity(
                    chemical: outputComponents[1], quantity: outputQuantity)

                let inputs = inputParts.compactMap { part -> ChemicalQuantity? in
                    let components = part.split(separator: " ").map { String($0) }
                    guard let quantity = Int(components[0]), components.count == 2 else {
                        return nil
                    }
                    return ChemicalQuantity(chemical: components[1], quantity: quantity)
                }

                reactions[output.chemical] = Reaction(inputs: inputs, output: output)
            }
    }

    private func calculateOre(for chemical: String, quantity: Int) -> Int {
        if chemical == "ORE" {
            return quantity
        }

        let availableSurplus = surplus[chemical] ?? 0
        let neededQuantity = max(0, quantity - availableSurplus)
        surplus[chemical] = max(0, availableSurplus - quantity)

        guard neededQuantity > 0, let reaction = reactions[chemical] else {
            guard reactions[chemical] != nil else {
                fatalError("No reaction found for \(chemical)")
            }
            return 0
        }

        let reactionRuns =
            (neededQuantity + reaction.output.quantity - 1) / reaction.output.quantity
        let producedQuantity = reactionRuns * reaction.output.quantity

        surplus[chemical, default: 0] += producedQuantity - neededQuantity

        return reaction.inputs.reduce(0) { total, input in
            total + calculateOre(for: input.chemical, quantity: input.quantity * reactionRuns)
        }
    }

    func minimumOreForFuel() -> Int {
        surplus.removeAll()
        return calculateOre(for: "FUEL", quantity: 1)
    }

    func maximumFuelFromOre(_ targetOre: Int) -> Int {
        let orePerFuel = minimumOreForFuel()
        var (low, high) = (targetOre / orePerFuel, targetOre / orePerFuel * 2)

        while low < high {
            surplus.removeAll()
            let mid = (low + high + 1) / 2
            let oreNeeded = calculateOre(for: "FUEL", quantity: mid)

            if oreNeeded <= targetOre {
                low = mid
            } else {
                high = mid - 1
            }
        }

        return low
    }
}

func main() {
    guard CommandLine.arguments.count > 1,
        let content = try? String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
    else {
        print("Usage: \(CommandLine.arguments[0]) <input_file>")
        return
    }

    let calculator = ChemicalCalculator(input: content)

    print("Part 1: \(calculator.minimumOreForFuel())")
    print("Part 2: \(calculator.maximumFuelFromOre(1_000_000_000_000))")
}

main()
