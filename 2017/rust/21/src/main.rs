use std::{collections::HashMap, env, error::Error, fs};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Pattern(Vec<Vec<char>>);

impl Pattern {
    fn new(s: &str) -> Self {
        Pattern(s.split('/').map(|row| row.chars().collect()).collect())
    }

    fn rotate(&self) -> Self {
        let n = self.0.len();
        Pattern(
            (0..n)
                .map(|j| (0..n).rev().map(|i| self.0[i][j]).collect())
                .collect(),
        )
    }

    fn flip(&self) -> Self {
        Pattern(
            self.0
                .iter()
                .map(|row| row.iter().rev().cloned().collect())
                .collect(),
        )
    }
}

fn parse_rules(input: &str) -> HashMap<Pattern, Pattern> {
    input
        .lines()
        .filter_map(|line| {
            let mut parts = line.split(" => ");
            Some((Pattern::new(parts.next()?), Pattern::new(parts.next()?)))
        })
        .collect()
}

fn enhance(pattern: &Pattern, rules: &HashMap<Pattern, Pattern>) -> Pattern {
    let (chunk_size, new_chunk_size) = if pattern.0.len() % 2 == 0 {
        (2, 3)
    } else {
        (3, 4)
    };
    let chunks_count = pattern.0.len() / chunk_size;
    let new_size = chunks_count * new_chunk_size;
    let mut new_grid = vec![vec!['.'; new_size]; new_size];

    for i in 0..chunks_count {
        for j in 0..chunks_count {
            let chunk = Pattern(
                pattern.0[i * chunk_size..(i + 1) * chunk_size]
                    .iter()
                    .map(|row| row[j * chunk_size..(j + 1) * chunk_size].to_vec())
                    .collect(),
            );

            let enhanced = find_matching_rule(&chunk, rules);
            for (x, row) in enhanced.0.iter().enumerate() {
                new_grid[i * new_chunk_size + x][j * new_chunk_size..(j + 1) * new_chunk_size]
                    .copy_from_slice(row);
            }
        }
    }
    Pattern(new_grid)
}

fn find_matching_rule(chunk: &Pattern, rules: &HashMap<Pattern, Pattern>) -> Pattern {
    let mut current = chunk.clone();
    for _ in 0..4 {
        if let Some(result) = rules.get(&current).or_else(|| rules.get(&current.flip())) {
            return result.clone();
        }
        current = current.rotate();
    }
    panic!("No matching rule found");
}

fn count_on_pixels(pattern: &Pattern) -> usize {
    pattern
        .0
        .iter()
        .flat_map(|row| row.iter())
        .filter(|&&c| c == '#')
        .count()
}

fn main() -> Result<(), Box<dyn Error>> {
    let content = fs::read_to_string(env::args().nth(1).ok_or("Provide a file path")?)?;
    let rules = parse_rules(&content);
    let initial = Pattern::new(".#./..#/###");

    println!(
        "Part 1: {}",
        count_on_pixels(&(0..5).fold(initial.clone(), |p, _| enhance(&p, &rules)))
    );
    println!(
        "Part 2: {}",
        count_on_pixels(&(0..18).fold(initial, |p, _| enhance(&p, &rules)))
    );

    Ok(())
}
