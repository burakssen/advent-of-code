use std::collections::{HashMap, VecDeque};
use std::{env, fs};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let binding = fs::read_to_string(env::args().nth(1).ok_or("Provide a file path")?)?;
    let instructions: Vec<&str> = binding.lines().collect();

    // Part 1
    let mut registers = HashMap::new();
    let mut last_sound = 0;
    let mut idx = 0;

    while (0..instructions.len()).contains(&(idx as usize)) {
        let parts: Vec<&str> = instructions[idx as usize].split_whitespace().collect();
        let (instruction, x) = (parts[0], parts[1].chars().next().unwrap());
        let y = parts
            .get(2)
            .map(|&s| {
                s.parse::<i64>()
                    .unwrap_or_else(|_| *registers.get(&s.chars().next().unwrap()).unwrap_or(&0))
            })
            .unwrap_or(0);

        match instruction {
            "snd" => last_sound = *registers.get(&x).unwrap_or(&0),
            "set" => {
                registers.insert(x, y);
            }
            "add" => {
                *registers.entry(x).or_insert(0) += y;
            }
            "mul" => {
                *registers.entry(x).or_insert(0) *= y;
            }
            "mod" => {
                *registers.entry(x).or_insert(0) %= y;
            }
            "rcv" if *registers.get(&x).unwrap_or(&0) != 0 => {
                println!("Part 1: {}", last_sound);
                break;
            }
            "jgz" if *registers.get(&x).unwrap_or(&0) > 0 => idx += y as isize - 1,
            _ => {}
        }
        idx += 1;
    }

    // Part 2
    let mut programs = [
        Program::new(0, &instructions),
        Program::new(1, &instructions),
    ];
    let mut queues = [VecDeque::new(), VecDeque::new()];
    let mut send_count1 = 0;

    loop {
        let (queue0, queue1) = queues.split_at_mut(1);
        let waiting = programs.iter_mut().enumerate().all(|(i, prog)| {
            let (send_queue, recv_queue) = if i == 0 {
                (&mut queue1[0], &mut queue0[0])
            } else {
                (&mut queue0[0], &mut queue1[0])
            };
            prog.execute(send_queue, recv_queue, &mut send_count1)
        });
        if waiting {
            break;
        }
    }

    println!("Part 2: {}", send_count1);
    Ok(())
}

struct Program<'a> {
    registers: HashMap<char, i64>,
    instructions: &'a [&'a str],
    pc: i64,
    id: i64,
}

impl<'a> Program<'a> {
    fn new(id: i64, instructions: &'a [&str]) -> Self {
        let mut registers = HashMap::new();
        registers.insert('p', id);
        Self {
            registers,
            instructions,
            pc: 0,
            id,
        }
    }

    fn get_value(&self, operand: &str) -> i64 {
        operand.parse().unwrap_or_else(|_| {
            *self
                .registers
                .get(&operand.chars().next().unwrap())
                .unwrap_or(&0)
        })
    }

    fn execute(
        &mut self,
        send_queue: &mut VecDeque<i64>,
        recv_queue: &mut VecDeque<i64>,
        send_count: &mut i64,
    ) -> bool {
        if !self.instructions.get(self.pc as usize).is_some() {
            return true;
        }

        let parts: Vec<&str> = self.instructions[self.pc as usize]
            .split_whitespace()
            .collect();
        let (instruction, x) = (parts[0], parts[1].chars().next().unwrap());
        let y = parts.get(2).map(|&s| self.get_value(s)).unwrap_or(0);

        match instruction {
            "snd" => {
                send_queue.push_back(self.get_value(parts[1]));
                if self.id == 1 {
                    *send_count += 1;
                }
            }
            "set" => {
                self.registers.insert(x, y);
            }
            "add" => {
                *self.registers.entry(x).or_insert(0) += y;
            }
            "mul" => {
                *self.registers.entry(x).or_insert(0) *= y;
            }
            "mod" => {
                *self.registers.entry(x).or_insert(0) %= y;
            }
            "rcv" => {
                if let Some(value) = recv_queue.pop_front() {
                    self.registers.insert(x, value);
                } else {
                    return true;
                }
            }
            "jgz" if self.get_value(parts[1]) > 0 => {
                self.pc += y - 1;
            }
            _ => {}
        }

        self.pc += 1;
        false
    }
}
