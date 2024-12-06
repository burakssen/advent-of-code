use std::collections::HashMap;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Error: Please provide a file path as an argument.");
        return;
    }

    let filename = &args[1];
    let content = fs::read_to_string(filename).expect("Failed to read file");
    let mut memory: Vec<i32> = content
        .split_whitespace()
        .map(|x| x.trim().parse().expect("Failed to parse number"))
        .collect();

    let part1_result = solve(&mut memory);
    let part2_result = solve(&mut memory);
    println!("Part 1: {}", part1_result);
    println!("Part 2: {}", part2_result);
}

fn solve(memory: &mut Vec<i32>) -> i32 {
    let mut memory_states = HashMap::new();

    while memory_states.insert(memory.clone(), true).is_none() {
        let (max_index, max) =
            memory
                .iter()
                .enumerate()
                .fold((0, memory[0]), |(max_idx, max), (i, &value)| {
                    if value > max {
                        (i, value)
                    } else {
                        (max_idx, max)
                    }
                });

        memory[max_index] = 0;
        let mut i = (max_index + 1) % memory.len();
        for _ in 0..max {
            memory[i] += 1;
            i = (i + 1) % memory.len();
        }
    }

    memory_states.len() as i32
}
