use std::{collections::HashMap, env, error::Error, fs};

fn main() -> Result<(), Box<dyn Error>> {
    let content = fs::read_to_string(env::args().nth(1).ok_or("Please provide a file path")?)?;
    let layers = parse_input(&content)?;

    println!("Part 1: {}", calculate_severity(&layers));
    println!("Part 2: {}", find_min_delay(&layers));

    Ok(())
}

fn parse_input(content: &str) -> Result<HashMap<u32, u32>, Box<dyn Error>> {
    content
        .lines()
        .map(|line| {
            let mut parts = line.split(": ").map(str::parse::<u32>);
            match (parts.next(), parts.next()) {
                (Some(Ok(depth)), Some(Ok(range))) => Ok((depth, range)),
                _ => Err("Invalid input format".into()),
            }
        })
        .collect::<Result<HashMap<_, _>, _>>()
}

fn calculate_severity(layers: &HashMap<u32, u32>) -> u32 {
    layers
        .iter()
        .filter_map(|(&depth, &range)| {
            if caught_at(depth, range) {
                Some(depth * range)
            } else {
                None
            }
        })
        .sum()
}

fn caught_at(depth: u32, range: u32) -> bool {
    depth % ((range - 1) * 2) == 0
}

fn find_min_delay(layers: &HashMap<u32, u32>) -> u32 {
    (0..)
        .find(|&delay| {
            !layers
                .iter()
                .any(|(&depth, &range)| caught_at(depth + delay, range))
        })
        .unwrap()
}
