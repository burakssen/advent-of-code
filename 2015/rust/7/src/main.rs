use std::collections::HashMap;

enum Instruction {
    AND(String, String, String),
    OR(String, String, String),
    NOT(String, String),
    LSHIFT(String, String, String),
    RSHIFT(String, String, String),
    ASSIGN(String, String),
}

fn main() {
    // get command line arguments
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        println!("Usage: {} <input.txt>", args[0]);
        return;
    }

    let filename: &str = &args[1];
    let contents = std::fs::read_to_string(filename).expect("Failed to read file");
    let lines = contents.lines();

    let mut instructions: Vec<Instruction> = Vec::new();
    let mut wires: HashMap<String, u16> = HashMap::new();

    for line in lines {
        let parts: Vec<&str> = line.split_whitespace().collect();
        let instruction = match parts.len() {
            3 => Instruction::ASSIGN(parts[0].to_string(), parts[2].to_string()),
            4 => Instruction::NOT(parts[1].to_string(), parts[3].to_string()),
            5 => {
                let op = parts[1];
                let arg1 = parts[0];
                let arg2 = parts[2];
                let dest = parts[4];
                match op {
                    "AND" => Instruction::AND(arg1.to_string(), arg2.to_string(), dest.to_string()),
                    "OR" => Instruction::OR(arg1.to_string(), arg2.to_string(), dest.to_string()),
                    "LSHIFT" => {
                        Instruction::LSHIFT(arg1.to_string(), arg2.to_string(), dest.to_string())
                    }
                    "RSHIFT" => {
                        Instruction::RSHIFT(arg1.to_string(), arg2.to_string(), dest.to_string())
                    }
                    _ => panic!("Unknown operator: {}", op),
                }
            }
            _ => panic!("Invalid instruction: {}", line),
        };

        instructions.push(instruction);
    }

    let a = eval("a", &instructions, &mut wires);
    println!("Part 1: {}", a);
    wires.clear();
    wires.insert("b".to_string(), a);
    let a = eval("a", &instructions, &mut wires);
    println!("Part 2: {}", a);
}

fn eval(target: &str, instructions: &[Instruction], wires: &mut HashMap<String, u16>) -> u16 {
    if let Some(&value) = wires.get(target) {
        return value;
    }

    let instruction = instructions.iter().find(|i| match i {
        Instruction::ASSIGN(_, dest)
        | Instruction::NOT(_, dest)
        | Instruction::AND(_, _, dest)
        | Instruction::OR(_, _, dest)
        | Instruction::LSHIFT(_, _, dest)
        | Instruction::RSHIFT(_, _, dest) => dest == target,
    });

    let instruction = match instruction {
        Some(instruction) => instruction,
        None => panic!("No instruction found for target: {}", target),
    };

    let (op1, op2) = match instruction {
        Instruction::ASSIGN(op1, _) | Instruction::NOT(op1, _) => {
            let op1 = op1
                .parse()
                .unwrap_or_else(|_| eval(op1, instructions, wires));
            (op1, 0)
        }
        Instruction::AND(op1, op2, _)
        | Instruction::OR(op1, op2, _)
        | Instruction::LSHIFT(op1, op2, _)
        | Instruction::RSHIFT(op1, op2, _) => {
            let op1 = op1
                .parse()
                .unwrap_or_else(|_| eval(op1, instructions, wires));
            let op2 = op2
                .parse()
                .unwrap_or_else(|_| eval(op2, instructions, wires));
            (op1, op2)
        }
    };

    let result = match instruction {
        Instruction::ASSIGN(_, _) => op1,
        Instruction::NOT(_, _) => !op1,
        Instruction::AND(_, _, _) => op1 & op2,
        Instruction::OR(_, _, _) => op1 | op2,
        Instruction::LSHIFT(_, _, _) => op1 << op2,
        Instruction::RSHIFT(_, _, _) => op1 >> op2,
    };

    wires.insert(target.to_string(), result);
    result
}
