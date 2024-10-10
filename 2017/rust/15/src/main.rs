use std::{env, error::Error, fs};

const FACTOR_A: u64 = 16807;
const FACTOR_B: u64 = 48271;
const MODULUS: u64 = 2147483647;
const MASK: u64 = 0xFFFF; // Mask for the lowest 16 bits

fn generate_next(mut value: u64, factor: u64, multiple: u64) -> u64 {
    loop {
        value = (value * factor) % MODULUS;
        if value % multiple == 0 {
            return value;
        }
    }
}

fn count_matches(start_a: u64, start_b: u64, rounds: u32, multiple_a: u64, multiple_b: u64) -> u32 {
    let (mut gen_a, mut gen_b, mut count) = (start_a, start_b, 0);

    for _ in 0..rounds {
        gen_a = generate_next(gen_a, FACTOR_A, multiple_a);
        gen_b = generate_next(gen_b, FACTOR_B, multiple_b);

        if gen_a & MASK == gen_b & MASK {
            count += 1;
        }
    }

    count
}

fn main() -> Result<(), Box<dyn Error>> {
    let content = fs::read_to_string(env::args().nth(1).ok_or("Please provide a file path")?)?;
    let mut lines = content.lines();

    let start_a = lines
        .next()
        .and_then(|line| line.split_whitespace().last()?.parse().ok())
        .ok_or("Invalid input for generator A")?;
    let start_b = lines
        .next()
        .and_then(|line| line.split_whitespace().last()?.parse().ok())
        .ok_or("Invalid input for generator B")?;

    println!(
        "Part 1: {}",
        count_matches(start_a, start_b, 40_000_000, 1, 1)
    );
    println!(
        "Part 2: {}",
        count_matches(start_a, start_b, 5_000_000, 4, 8)
    );

    Ok(())
}
