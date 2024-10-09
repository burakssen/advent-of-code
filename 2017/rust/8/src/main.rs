use std::{collections::HashMap, env, fs};

#[derive(Debug)]
struct Operation {
    left: String,
    right: String,
    operator: String,
    condition: (String, String, String),
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let content = fs::read_to_string(env::args().nth(1).ok_or("Please provide a file path")?)?;
    let mut registers = HashMap::new();
    let mut max = 0;

    for op in content.lines().map(parse_line) {
        let condition_left = *registers.get(&op.condition.0).unwrap_or(&0);
        let condition_right = op
            .condition
            .2
            .parse()
            .unwrap_or_else(|_| *registers.get(&op.condition.2).unwrap_or(&0));

        if eval_condition(&op.condition.1, condition_left, condition_right) {
            let left = *registers.get(&op.left).unwrap_or(&0);
            let right = op
                .right
                .parse()
                .unwrap_or_else(|_| *registers.get(&op.right).unwrap_or(&0));
            let value = match op.operator.as_str() {
                "inc" => left + right,
                "dec" => left - right,
                _ => 0,
            };

            if value > max {
                max = value;
            }
            registers.insert(op.left, value);
        }
    }

    println!("Part 1: {}", registers.values().max().unwrap());
    println!("Part 2: {}", max);
    Ok(())
}

fn parse_line(line: &str) -> Operation {
    let parts: Vec<&str> = line.split_whitespace().collect();
    Operation {
        left: parts[0].to_string(),
        right: parts[2].to_string(),
        operator: parts[1].to_string(),
        condition: (
            parts[4].to_string(),
            parts[5].to_string(),
            parts[6].to_string(),
        ),
    }
}

fn eval_condition(op: &str, left: i32, right: i32) -> bool {
    match op {
        "==" => left == right,
        "!=" => left != right,
        "<" => left < right,
        "<=" => left <= right,
        ">" => left > right,
        ">=" => left >= right,
        _ => false,
    }
}
