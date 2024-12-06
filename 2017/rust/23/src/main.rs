use std::{collections::HashMap, env, error::Error, fs};

fn main() -> Result<(), Box<dyn Error>> {
    let content = fs::read_to_string(env::args().nth(1).ok_or("Provide a file path")?)?;
    let instructions: Vec<_> = content.lines().collect();

    let mut registers = HashMap::new();
    let mut pc = 0;
    let mut mul_count = 0;

    while pc < instructions.len() {
        let parts: Vec<_> = instructions[pc].split_whitespace().collect();
        match parts[0] {
            "set" => {
                *registers
                    .entry(parts[1].chars().next().unwrap())
                    .or_default() = get_value(parts[2], &registers)
            }
            "sub" => {
                *registers
                    .entry(parts[1].chars().next().unwrap())
                    .or_default() -= get_value(parts[2], &registers)
            }
            "mul" => {
                *registers
                    .entry(parts[1].chars().next().unwrap())
                    .or_default() *= get_value(parts[2], &registers);
                mul_count += 1;
            }
            "jnz" if get_value(parts[1], &registers) != 0 => {
                pc = (pc as isize + get_value(parts[2], &registers) as isize) as usize;
                continue;
            }
            _ => (),
        }
        pc += 1;
    }
    println!("Part 1: {}", mul_count);

    let (b, c) = (93 * 100 + 100000, 93 * 100 + 117000);
    println!(
        "Part 2: {}",
        (b..=c).step_by(17).filter(|&num| !is_prime(num)).count()
    );
    Ok(())
}

fn get_value(input: &str, registers: &HashMap<char, i64>) -> i64 {
    input
        .parse()
        .unwrap_or_else(|_| *registers.get(&input.chars().next().unwrap()).unwrap_or(&0))
}

fn is_prime(n: i64) -> bool {
    n > 1 && (2..=(n as f64).sqrt() as i64).all(|i| n % i != 0)
}
