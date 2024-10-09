use std::collections::HashMap;
use std::fs;

#[derive(Debug)]
struct Node {
    weight: i32,
    children: Vec<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let filename = std::env::args()
        .nth(1)
        .ok_or("Please provide a file path")?;
    let content = fs::read_to_string(filename)?;

    let nodes: HashMap<String, Node> = content
        .lines()
        .map(parse_line)
        .map(|(name, weight, children)| (name.to_string(), Node { weight, children }))
        .collect();

    let part1_result = part1(&nodes);
    println!("Part 1: {}", part1_result);
    println!("Part 2: {}", part2(&nodes, &part1_result));

    Ok(())
}

fn parse_line(line: &str) -> (&str, i32, Vec<String>) {
    let parts: Vec<&str> = line.split_whitespace().collect();
    let weight = parts[1]
        .trim_matches(|c| c == '(' || c == ')')
        .parse()
        .unwrap();
    let children = parts.get(3..).map_or(Vec::new(), |c| {
        c.iter()
            .map(|s| s.trim_matches(|c| c == ',' || c == ' ').to_string())
            .collect()
    });
    (parts[0], weight, children)
}

fn part1(nodes: &HashMap<String, Node>) -> String {
    nodes
        .keys()
        .find(|name| !nodes.values().any(|node| node.children.contains(name)))
        .unwrap()
        .clone()
}

fn part2(nodes: &HashMap<String, Node>, root: &str) -> i32 {
    fn get_weight(nodes: &HashMap<String, Node>, name: &str) -> i32 {
        let node = &nodes[name];
        node.weight
            + node
                .children
                .iter()
                .map(|c| get_weight(nodes, c))
                .sum::<i32>()
    }

    fn find_unbalanced(nodes: &HashMap<String, Node>, name: &str) -> Option<i32> {
        let node = &nodes[name];
        let mut weights: Vec<(String, i32)> = node
            .children
            .iter()
            .map(|c| (c.clone(), get_weight(nodes, c)))
            .collect();

        if weights.is_empty() {
            return None;
        }

        weights.sort_by_key(|&(_, w)| w);

        if weights[0].1 != weights.last().unwrap().1 {
            if weights[0].1 == weights[1].1 {
                // Last element is unbalanced
                let unbalanced = weights.pop().unwrap();
                if let Some(balanced_weight) = find_unbalanced(nodes, &unbalanced.0) {
                    return Some(balanced_weight);
                }
                let diff = unbalanced.1 - weights[0].1;
                return Some(nodes[&unbalanced.0].weight - diff);
            } else {
                // First element is unbalanced
                let unbalanced = weights.remove(0);
                if let Some(balanced_weight) = find_unbalanced(nodes, &unbalanced.0) {
                    return Some(balanced_weight);
                }
                let diff = weights[0].1 - unbalanced.1;
                return Some(nodes[&unbalanced.0].weight + diff);
            }
        }

        // All weights are balanced, check children
        for (child, _) in weights {
            if let Some(balanced_weight) = find_unbalanced(nodes, &child) {
                return Some(balanced_weight);
            }
        }

        None
    }

    find_unbalanced(nodes, root).unwrap()
}
