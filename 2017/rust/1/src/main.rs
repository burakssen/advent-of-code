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

    let digit_sequence: Vec<u8> = contents.trim().bytes().collect();
    let len = digit_sequence.len();

    // Check if the length is even, and return an error if not
    if len % 2 != 0 {
        eprintln!("Error: The length of the digit sequence must be even.");
        return;
    }

    let mut sum_part1 = 0;
    let mut sum_part2 = 0;

    for i in 0..len {
        let current_digit = digit_sequence[i] - b'0'; // Convert directly to a number
        let next_digit = digit_sequence[(i + 1) % len] - b'0'; // Convert to number for next digit
        let halfway_digit = digit_sequence[(i + len / 2) % len] - b'0'; // Convert to number for halfway digit

        if current_digit == next_digit {
            sum_part1 += current_digit as i32; // Accumulate for part 1
        }

        if current_digit == halfway_digit {
            sum_part2 += current_digit as i32; // Accumulate for part 2
        }
    }

    println!("Part 1: {}", sum_part1);
    println!("Part 2: {}", sum_part2);
}
