use std::{collections::HashMap, env, error::Error, fs};

fn perform_dance(programs: &mut Vec<char>, moves: &str) {
    for move_str in moves.split(',') {
        match &move_str[..1] {
            "s" => programs.rotate_right(move_str[1..].parse().unwrap()),
            "x" => {
                let pos: Vec<_> = move_str[1..]
                    .split('/')
                    .map(|s| s.parse().unwrap())
                    .collect();
                programs.swap(pos[0], pos[1]);
            }
            "p" => {
                let a = move_str.chars().nth(1).unwrap();
                let b = move_str.chars().nth(3).unwrap();
                let (pos_a, pos_b) = (
                    programs.iter().position(|&c| c == a).unwrap(),
                    programs.iter().position(|&c| c == b).unwrap(),
                );
                programs.swap(pos_a, pos_b);
            }
            _ => unreachable!("Invalid move: {}", move_str),
        }
    }
}

fn dance_billion_times(initial_programs: &[char], moves: &str) -> String {
    let mut programs = initial_programs.to_vec();
    let mut seen_states = HashMap::new();
    let target = 1_000_000_000;

    for i in 0..target {
        if let Some(&previous_i) = seen_states.get(&programs) {
            let cycle_length = i - previous_i;
            let remaining = (target - i) % cycle_length;
            return (0..remaining)
                .fold(programs.clone(), |mut p, _| {
                    perform_dance(&mut p, moves);
                    p
                })
                .iter()
                .collect();
        }
        seen_states.insert(programs.clone(), i);
        perform_dance(&mut programs, moves);
    }

    programs.iter().collect()
}

fn main() -> Result<(), Box<dyn Error>> {
    let content = fs::read_to_string(env::args().nth(1).ok_or("Please provide a file path")?)?;

    // Part 1
    let mut programs: Vec<char> = ('a'..='p').collect();
    perform_dance(&mut programs, &content);
    println!("Part 1: {}", programs.iter().collect::<String>());

    // Part 2
    let initial_programs: Vec<char> = ('a'..='p').collect();
    let result = dance_billion_times(&initial_programs, &content);
    println!("Part 2: {}", result);

    Ok(())
}
