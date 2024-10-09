use std::collections::HashMap;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Error: Please provide a file path as an argument.");
        return;
    }

    // Parse the argument
    let filename = &args[1];

    // Read the file
    let content = fs::read_to_string(filename).expect("Failed to read file");

    let mut part1_count = 0;
    let mut part2_count = 0;

    for line in content.lines() {
        if is_valid(line, false) {
            part1_count += 1;
        }
        if is_valid(line, true) {
            part2_count += 1;
        }
    }

    println!("Part 1: {}", part1_count);
    println!("Part 2: {}", part2_count);
}

fn is_valid(pass: &str, part2: bool) -> bool {
    let mut map = HashMap::new();
    for word in pass.split_whitespace() {
        let mut chars = word.chars().collect::<Vec<char>>();

        if part2 {
            chars.sort();
        }

        let sorted_word = chars.iter().collect::<String>();
        if map.contains_key(&sorted_word) {
            return false;
        }
        map.insert(sorted_word, true);
    }
    return true;
}
