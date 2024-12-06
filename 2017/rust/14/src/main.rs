use std::{env, error::Error, fs};

fn knot_hash(input: &str) -> String {
    let mut list: Vec<u8> = (0..=255).collect();
    let mut pos = 0;
    let mut skip = 0;
    let lengths = input
        .bytes()
        .chain([17, 31, 73, 47, 23])
        .collect::<Vec<_>>();

    for _ in 0..64 {
        for &len in &lengths {
            for i in 0..len as usize / 2 {
                list.swap((pos + i) % 256, (pos + len as usize - 1 - i) % 256);
            }
            pos = (pos + len as usize + skip) % 256;
            skip += 1;
        }
    }

    list.chunks(16)
        .map(|chunk| format!("{:02x}", chunk.iter().fold(0, |acc, &x| acc ^ x)))
        .collect()
}

fn count_used_squares(key: &str) -> usize {
    (0..128)
        .map(|row| {
            knot_hash(&format!("{}-{}", key, row))
                .chars()
                .map(|c| c.to_digit(16).unwrap().count_ones() as usize)
                .sum::<usize>()
        })
        .sum()
}

fn generate_grid(key: &str) -> Vec<Vec<bool>> {
    (0..128)
        .map(|row| {
            knot_hash(&format!("{}-{}", key, row))
                .chars()
                .flat_map(|c| {
                    (0..4)
                        .rev()
                        .map(move |i| (c.to_digit(16).unwrap() & (1 << i)) != 0)
                })
                .collect()
        })
        .collect()
}

fn count_regions(grid: &mut Vec<Vec<bool>>) -> usize {
    let mut regions = 0;
    for i in 0..128 {
        for j in 0..128 {
            if grid[i][j] {
                regions += 1;
                dfs(grid, i, j);
            }
        }
    }
    regions
}

fn dfs(grid: &mut Vec<Vec<bool>>, i: usize, j: usize) {
    if !grid[i][j] {
        return;
    }
    grid[i][j] = false;
    for &(di, dj) in [(0, 1), (1, 0), (0, usize::MAX), (usize::MAX, 0)].iter() {
        let (ni, nj) = (i.wrapping_add(di), j.wrapping_add(dj));
        if ni < 128 && nj < 128 {
            dfs(grid, ni, nj);
        }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let key = fs::read_to_string(env::args().nth(1).ok_or("Please provide a file path")?)?
        .trim()
        .to_string();
    let mut grid = generate_grid(&key);

    println!("Part 1: {}", count_used_squares(&key));
    println!("Part 2: {}", count_regions(&mut grid));
    Ok(())
}
