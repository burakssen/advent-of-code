use std::{env, fs};

#[derive(Debug, PartialEq, Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let grid: Vec<Vec<char>> =
        fs::read_to_string(env::args().nth(1).ok_or("Provide a file path")?)?
            .lines()
            .map(|line| line.chars().collect())
            .collect();

    let (mut x, mut y) = (
        grid[0]
            .iter()
            .position(|&ch| ch == '|')
            .ok_or("No starting point found")?,
        0,
    );
    let (mut direction, mut letters, mut steps) = (Direction::Down, String::new(), 0);

    loop {
        steps += 1;
        match grid[y][x] {
            'A'..='Z' => letters.push(grid[y][x]),
            '+' => direction = next_direction(&grid, x, y, direction),
            ' ' => break,
            _ => {}
        }
        let (next_x, next_y) = next_position(x, y, direction);
        if next_y >= grid.len() || next_x >= grid[next_y].len() || grid[next_y][next_x] == ' ' {
            break;
        }
        (x, y) = (next_x, next_y);
    }

    println!("Part 1: {}", letters);
    println!("Part 2: {}", steps);
    Ok(())
}

fn next_direction(grid: &[Vec<char>], x: usize, y: usize, current: Direction) -> Direction {
    [
        Direction::Up,
        Direction::Down,
        Direction::Left,
        Direction::Right,
    ]
    .into_iter()
    .find(|&dir| {
        dir != opposite(current) && {
            let (nx, ny) = next_position(x, y, dir);
            ny < grid.len() && nx < grid[ny].len() && grid[ny][nx] != ' '
        }
    })
    .unwrap_or_else(|| panic!("No valid direction found at ({}, {})", x, y))
}

fn next_position(x: usize, y: usize, direction: Direction) -> (usize, usize) {
    match direction {
        Direction::Up => (x, y.saturating_sub(1)),
        Direction::Down => (x, y + 1),
        Direction::Left => (x.saturating_sub(1), y),
        Direction::Right => (x + 1, y),
    }
}

fn opposite(direction: Direction) -> Direction {
    match direction {
        Direction::Up => Direction::Down,
        Direction::Down => Direction::Up,
        Direction::Left => Direction::Right,
        Direction::Right => Direction::Left,
    }
}
