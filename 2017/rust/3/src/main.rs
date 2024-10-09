use std::collections::HashMap;
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
    let input = fs::read_to_string(filename).expect("Failed to read file");
    let n: i32 = input.trim().parse().expect("Failed to parse input");

    println!("Part 1: {}", calculate_steps(n));
    println!("Part 2: {}", calculate_first_larger_value(n));
}

fn calculate_steps(n: i32) -> i32 {
    if n == 1 {
        return 0;
    }

    // Calculate the ring number
    let ring = ((n as f64).sqrt().ceil() as i32) / 2;

    // Calculate the side length
    let side_length = 2 * ring + 1;

    // Calculate the max value in the ring
    let max_value = side_length.pow(2);

    // Calculate the distance from the middle of the ring
    let distance_from_middle = ring;

    // Find which side of the ring the number is on
    for i in 0..4 {
        let corner_value = max_value - i * (side_length - 1);
        if n > corner_value - side_length + 1 {
            let steps_from_middle = (n - (corner_value - distance_from_middle)).abs();
            return ring + steps_from_middle;
        }
    }

    -1
}

fn calculate_first_larger_value(n: i32) -> i32 {
    let mut grid: HashMap<(i32, i32), i32> = HashMap::new();
    let mut x = 0;
    let mut y = 0;
    let mut value = 1;
    grid.insert((x, y), value);

    let directions = [(1, 0), (0, 1), (-1, 0), (0, -1)];
    let mut dir_index = 0;
    let mut steps = 0;
    let mut steps_in_direction = 1;

    while value <= n {
        steps += 1;
        x += directions[dir_index].0;
        y += directions[dir_index].1;

        value = calculate_sum(&grid, x, y);
        grid.insert((x, y), value);

        if steps == steps_in_direction {
            dir_index = (dir_index + 1) % 4;
            steps = 0;
            if dir_index % 2 == 0 {
                steps_in_direction += 1;
            }
        }
    }

    value
}

fn calculate_sum(grid: &HashMap<(i32, i32), i32>, x: i32, y: i32) -> i32 {
    let neighbors = [
        (x - 1, y - 1),
        (x, y - 1),
        (x + 1, y - 1),
        (x - 1, y),
        (x + 1, y),
        (x - 1, y + 1),
        (x, y + 1),
        (x + 1, y + 1),
    ];

    neighbors.iter().filter_map(|&pos| grid.get(&pos)).sum()
}
