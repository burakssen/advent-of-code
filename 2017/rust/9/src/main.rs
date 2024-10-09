use std::{env, fs};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let content = fs::read_to_string(env::args().nth(1).ok_or("Please provide a file path")?)?;
    let (score, garbage_chars) = process_stream(&content);
    println!("Part 1: {}\nPart 2: {}", score, garbage_chars);
    Ok(())
}

fn process_stream(content: &str) -> (i32, usize) {
    let (mut total_score, mut current_score, mut garbage_count) = (0, 0, 0);
    let (mut in_garbage, mut skip_next) = (false, false);

    for c in content.chars() {
        if skip_next {
            skip_next = false;
            continue;
        }
        match c {
            '!' => skip_next = true,
            '<' if !in_garbage => in_garbage = true,
            '>' if in_garbage => in_garbage = false,
            '{' if !in_garbage => {
                current_score += 1;
                total_score += current_score;
            }
            '}' if !in_garbage => current_score = current_score.max(1) - 1,
            _ if in_garbage => garbage_count += 1,
            _ => {}
        }
    }
    (total_score, garbage_count)
}
