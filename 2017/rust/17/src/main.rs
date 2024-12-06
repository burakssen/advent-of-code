use std::{env, error::Error, fs};

fn main() -> Result<(), Box<dyn Error>> {
    // Read the step size from the input file
    let step_size: usize =
        fs::read_to_string(env::args().nth(1).ok_or("Please provide a file path")?)?
            .trim()
            .parse()?;

    // Part 1: Simulate the buffer for 2017 insertions
    let mut buffer = vec![0];
    let mut current_position = 0;
    for value in 1..=2017 {
        current_position = (current_position + step_size) % buffer.len() + 1;
        buffer.insert(current_position, value);
    }
    println!("Part 1: {}", buffer[(current_position + 1) % buffer.len()]);

    // Part 2: Track the value after 0 for 50,000,000 insertions
    let mut value_after_zero = 0;
    current_position = 0;
    for value in 1..=50_000_000 {
        current_position = (current_position + step_size) % value + 1;
        if current_position == 1 {
            value_after_zero = value;
        }
    }
    println!("Part 2: {}", value_after_zero);

    Ok(())
}
