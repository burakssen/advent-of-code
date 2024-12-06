use std::env;
use std::fs;

fn main() {
    // Get command line arguments
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Error: Please provide a file path as an argument.");
        return;
    }

    // Parse the argument
    let filename = &args[1];

    // Read the file
    let contents = fs::read_to_string(filename).expect("Failed to read file");

    let mut sum_part1 = 0;
    let mut sum_part2 = 0;
    // loop over the lines of the file
    for line in contents.lines() {
        let words: Vec<&str> = line.split_whitespace().collect();
        let numbers: Vec<i32> = words.iter().map(|s| s.parse().unwrap()).collect();
        let max = numbers.iter().max().unwrap();
        let min = numbers.iter().min().unwrap();

        for i in 0..numbers.len() {
            for j in 0..numbers.len() {
                if i == j {
                    continue;
                }
                if numbers[i] % numbers[j] == 0 {
                    sum_part2 += numbers[i] / numbers[j];
                }
            }
        }

        sum_part1 += max - min;
    }

    println!("Part 1: {}", sum_part1);
    println!("Part 2: {}", sum_part2);
}
