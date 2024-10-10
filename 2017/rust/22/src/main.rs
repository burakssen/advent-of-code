use std::{env, error::Error, fs};

#[derive(Clone, Copy, PartialEq)]
enum NodeStatus {
    Clean,
    Weakened,
    Infected,
    Flagged,
}

struct VirusCarrier {
    x: isize,
    y: isize,
    direction: usize,
}

impl VirusCarrier {
    fn new(x: isize, y: isize) -> Self {
        VirusCarrier { x, y, direction: 0 }
    }

    fn turn_left(&mut self) {
        self.direction = (self.direction + 3) % 4;
    }

    fn turn_right(&mut self) {
        self.direction = (self.direction + 1) % 4;
    }

    fn reverse(&mut self) {
        self.direction = (self.direction + 2) % 4;
    }

    fn move_forward(&mut self) {
        match self.direction {
            0 => self.y -= 1, // Up
            1 => self.x += 1, // Right
            2 => self.y += 1, // Down
            3 => self.x -= 1, // Left
            _ => unreachable!(),
        }
    }
}

fn read_grid(file_path: &str) -> Result<Vec<Vec<NodeStatus>>, Box<dyn Error>> {
    let content = fs::read_to_string(file_path)?;
    Ok(content
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| {
                    if c == '#' {
                        NodeStatus::Infected
                    } else {
                        NodeStatus::Clean
                    }
                })
                .collect()
        })
        .collect())
}

fn expand_grid(grid: &mut Vec<Vec<NodeStatus>>, carrier: &mut VirusCarrier) {
    if carrier.y < 0 {
        grid.insert(0, vec![NodeStatus::Clean; grid[0].len()]);
        carrier.y = 0;
    } else if carrier.y as usize >= grid.len() {
        grid.push(vec![NodeStatus::Clean; grid[0].len()]);
    }
    if carrier.x < 0 {
        for row in grid.iter_mut() {
            row.insert(0, NodeStatus::Clean);
        }
        carrier.x = 0;
    } else if carrier.x as usize >= grid[0].len() {
        for row in grid.iter_mut() {
            row.push(NodeStatus::Clean);
        }
    }
}

fn part_one(grid: &mut Vec<Vec<NodeStatus>>, carrier: &mut VirusCarrier) -> usize {
    let mut infection_count = 0;

    for _ in 0..10_000 {
        let current_node = grid[carrier.y as usize][carrier.x as usize];
        if current_node == NodeStatus::Infected {
            carrier.turn_right();
        } else {
            carrier.turn_left();
        }

        if current_node == NodeStatus::Clean {
            grid[carrier.y as usize][carrier.x as usize] = NodeStatus::Infected;
            infection_count += 1;
        } else {
            grid[carrier.y as usize][carrier.x as usize] = NodeStatus::Clean;
        }

        carrier.move_forward();
        expand_grid(grid, carrier);
    }

    infection_count
}

fn part_two(grid: &mut Vec<Vec<NodeStatus>>, carrier: &mut VirusCarrier) -> usize {
    let mut infection_count = 0;

    for _ in 0..10_000_000 {
        let current_node = grid[carrier.y as usize][carrier.x as usize];
        match current_node {
            NodeStatus::Clean => {
                carrier.turn_left();
                grid[carrier.y as usize][carrier.x as usize] = NodeStatus::Weakened;
            }
            NodeStatus::Weakened => {
                grid[carrier.y as usize][carrier.x as usize] = NodeStatus::Infected;
                infection_count += 1;
            }
            NodeStatus::Infected => {
                carrier.turn_right();
                grid[carrier.y as usize][carrier.x as usize] = NodeStatus::Flagged;
            }
            NodeStatus::Flagged => {
                carrier.reverse();
                grid[carrier.y as usize][carrier.x as usize] = NodeStatus::Clean;
            }
        }

        carrier.move_forward();
        expand_grid(grid, carrier);
    }

    infection_count
}

fn main() -> Result<(), Box<dyn Error>> {
    let file_path = env::args().nth(1).ok_or("Provide a file path")?;
    let mut grid = read_grid(&file_path)?;

    let mut carrier = VirusCarrier::new(grid.len() as isize / 2, grid[0].len() as isize / 2);
    let infection_count_part1 = part_one(&mut grid, &mut carrier);
    println!("Part 1: {}", infection_count_part1);

    grid = read_grid(&file_path)?; // Reset grid for Part 2
    carrier = VirusCarrier::new(grid.len() as isize / 2, grid[0].len() as isize / 2);
    let infection_count_part2 = part_two(&mut grid, &mut carrier);
    println!("Part 2: {}", infection_count_part2);

    Ok(())
}
