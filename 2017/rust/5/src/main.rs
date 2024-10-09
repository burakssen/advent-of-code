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

    // Parse the content
    let arr: Vec<i32> = content
        .split_whitespace()
        .map(|x| x.parse::<i32>().unwrap())
        .collect();

    // Part 1
    let part1_result = part1(arr.clone());
    println!("Part 1: {}", part1_result);
    let part2_result = part2(arr.clone());
    println!("Part 2: {}", part2_result);
}

fn part1(arr: Vec<i32>) -> i32 {
    let mut i = 0;
    let mut steps = 0;
    let mut arr = arr;
    while i >= 0 && i < arr.len() as i32 {
        let jump = arr[i as usize];
        arr[i as usize] += 1;
        i += jump;
        steps += 1;
    }
    steps
}

fn part2(arr: Vec<i32>) -> i32 {
    let mut i = 0;
    let mut steps = 0;
    let mut arr = arr;
    while i >= 0 && i < arr.len() as i32 {
        let jump = arr[i as usize];
        if jump >= 3 {
            arr[i as usize] -= 1;
        } else {
            arr[i as usize] += 1;
        }
        i += jump;
        steps += 1;
    }
    steps
}
