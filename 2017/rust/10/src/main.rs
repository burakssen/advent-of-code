use std::{env, fs};

fn knot_hash_round(
    list: &mut Vec<u8>,
    lengths: &[usize],
    current_position: &mut usize,
    skip_size: &mut usize,
) {
    for &length in lengths {
        // Reverse the sublist
        let mut start = *current_position;
        let mut end = (*current_position + length - 1) % 256;
        for _ in 0..length / 2 {
            list.swap(start, end);
            start = (start + 1) % 256;
            end = if end == 0 { 255 } else { end - 1 };
        }

        // Move the current position
        *current_position = (*current_position + length + *skip_size) % 256;
        *skip_size += 1;
    }
}

fn dense_hash(sparse_hash: &[u8]) -> Vec<u8> {
    sparse_hash
        .chunks(16)
        .map(|chunk| chunk.iter().fold(0, |acc, &x| acc ^ x))
        .collect()
}

fn to_hex(bytes: &[u8]) -> String {
    bytes.iter().map(|&b| format!("{:02x}", b)).collect()
}

fn knot_hash(input: &str) -> String {
    let mut lengths: Vec<usize> = input.bytes().map(|b| b as usize).collect();
    lengths.extend_from_slice(&[17, 31, 73, 47, 23]);

    let mut list: Vec<u8> = (0..256).map(|i| i as u8).collect();
    let mut current_position = 0;
    let mut skip_size = 0;

    for _ in 0..64 {
        knot_hash_round(&mut list, &lengths, &mut current_position, &mut skip_size);
    }

    let dense = dense_hash(&list);
    to_hex(&dense)
}

fn part1(input: &str) -> usize {
    let lengths: Vec<usize> = input
        .trim()
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect();

    let mut list: Vec<u8> = (0..256).map(|i| i as u8).collect();
    let mut current_position = 0;
    let mut skip_size = 0;

    knot_hash_round(&mut list, &lengths, &mut current_position, &mut skip_size);

    list[0] as usize * list[1] as usize
}

fn part2(input: &str) -> String {
    knot_hash(input.trim())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let content = fs::read_to_string(env::args().nth(1).ok_or("Please provide a file path")?)?;

    println!("Part 1: {}", part1(&content));
    println!("Part 2: {}", part2(&content));

    Ok(())
}
