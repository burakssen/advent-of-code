use std::{env, fs};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let content = fs::read_to_string(env::args().nth(1).ok_or("Please provide a file path")?)?;
    let directions = content.trim().split(',');

    let (mut x, mut y, mut z, mut max_distance): (i32, i32, i32, i32) = (0, 0, 0, 0);

    for dir in directions {
        match dir {
            "n" => {
                y -= 1;
                z += 1;
            }
            "ne" => {
                x += 1;
                y -= 1;
            }
            "se" => {
                x += 1;
                z -= 1;
            }
            "s" => {
                y += 1;
                z -= 1;
            }
            "sw" => {
                x -= 1;
                y += 1;
            }
            "nw" => {
                x -= 1;
                z += 1;
            }
            _ => panic!("Unknown direction: {}", dir),
        }

        max_distance = max_distance.max((x.abs() + y.abs() + z.abs()) / 2);
    }

    println!("Part 1: {}", (x.abs() + y.abs() + z.abs()) / 2);
    println!("Part 2: {}", max_distance);

    Ok(())
}
