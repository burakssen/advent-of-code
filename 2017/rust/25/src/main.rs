use std::{collections::HashMap, env, error::Error, fs, str::FromStr};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Direction {
    Left,
    Right,
}

impl FromStr for Direction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "left" => Ok(Direction::Left),
            "right" => Ok(Direction::Right),
            _ => Err(format!("Invalid direction: {}", s)),
        }
    }
}

#[derive(Debug, Clone)]
struct Rule {
    write: u8,
    move_direction: Direction,
    next_state: usize,
}

#[derive(Debug)]
struct State {
    rules: [Option<Rule>; 2],
}

#[derive(Debug)]
struct TuringMachine {
    states: Vec<State>,
    tape: HashMap<i32, u8>,
    cursor: i32,
    current_state: usize,
}

impl TuringMachine {
    fn new(blueprint: &str) -> Result<(Self, usize), Box<dyn Error>> {
        let mut lines = blueprint.lines();
        let start_state = lines
            .next()
            .and_then(|line| line.split_whitespace().last())
            .ok_or("Missing or invalid start state")?
            .trim_end_matches('.')
            .to_string();
        let steps = lines
            .next()
            .and_then(|line| line.split_whitespace().find_map(|s| s.parse().ok()))
            .ok_or("Missing or invalid steps")?;

        let mut states = Vec::new();
        let mut state_map = HashMap::new();
        let mut current_state = String::new();
        let mut current_value = 0;
        let mut rule_parts = Vec::new();

        for line in lines {
            let line = line.trim_start();
            if line.starts_with("In state ") {
                current_state = line[9..line.len() - 1].to_string();
                if !state_map.contains_key(&current_state) {
                    state_map.insert(current_state.clone(), states.len());
                    states.push(State {
                        rules: [None, None],
                    });
                }
            } else if line.starts_with("If the current value is") {
                current_value = line
                    .split_whitespace()
                    .last()
                    .and_then(|v| v.trim_end_matches(':').parse().ok())
                    .ok_or("Invalid value")?;
                rule_parts.clear();
            } else if line.starts_with("- Write the value")
                || line.starts_with("- Move one slot to the")
                || line.starts_with("- Continue with state")
            {
                rule_parts.push(line);
                if rule_parts.len() == 3 {
                    let write: u8 = rule_parts[0]
                        .split_whitespace()
                        .last()
                        .and_then(|v| v.trim_end_matches('.').parse().ok())
                        .ok_or("Invalid write value")?;
                    let move_direction: Direction = rule_parts[1]
                        .split_whitespace()
                        .last()
                        .and_then(|v| v.trim_end_matches('.').parse().ok())
                        .ok_or("Invalid move direction")?;
                    let next_state = rule_parts[2]
                        .split_whitespace()
                        .last()
                        .map(|v| v.trim_end_matches('.').to_string())
                        .ok_or("Invalid next state")?;

                    let next_state_index = *state_map.entry(next_state).or_insert_with(|| {
                        states.push(State {
                            rules: [None, None],
                        });
                        states.len() - 1
                    });

                    let state_index = *state_map.get(&current_state).unwrap();
                    states[state_index].rules[current_value] = Some(Rule {
                        write,
                        move_direction,
                        next_state: next_state_index,
                    });
                }
            }
        }

        Ok((
            TuringMachine {
                states,
                tape: HashMap::new(),
                cursor: 0,
                current_state: *state_map.get(&start_state).unwrap(),
            },
            steps,
        ))
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let content = fs::read_to_string(env::args().nth(1).ok_or("Provide a file path")?)?;
    let (mut tm, steps) = TuringMachine::new(&content)?;

    for _ in 0..steps {
        let current_value = *tm.tape.entry(tm.cursor).or_insert(0) as usize;
        let rule = &tm.states[tm.current_state].rules[current_value];

        if let Some(rule) = rule {
            *tm.tape.entry(tm.cursor).or_insert(0) = rule.write;
            tm.cursor += if rule.move_direction == Direction::Left {
                -1
            } else {
                1
            };
            tm.current_state = rule.next_state;
        } else {
            return Err("Invalid state or rule".into());
        }
    }

    println!("Part 1: {}", tm.tape.values().filter(|&&v| v == 1).count());
    Ok(())
}
