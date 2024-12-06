trait Movement {
    fn move_pos(&mut self, direction: char);
}

struct Position {
    x: i32,
    y: i32,
}

impl Movement for Position {
    fn move_pos(&mut self, direction: char) {
        match direction {
            '^' => self.y += 1,
            'v' => self.y -= 1,
            '<' => self.x -= 1,
            '>' => self.x += 1,
            _ => panic!("Invalid direction"),
        }
    }
}

fn main() {
    // get command line arguments
    let args: Vec<String> = std::env::args().collect();
    // check if there are enough arguments
    if args.len() < 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    // read the input file
    let input = std::fs::read_to_string(&args[1]).expect("Failed to read input file");

    // split the input into lines
    let lines: Vec<&str> = input.lines().collect();

    let mut pos = Position { x: 0, y: 0 };

    // create hashmap to store the number of houses visited
    let mut houses = std::collections::HashMap::new();
    // insert the starting position
    houses.insert((pos.x, pos.y), 1);

    for c in lines[0].chars() {
        pos.move_pos(c);
        let count = houses.entry((pos.x, pos.y)).or_insert(1);
        *count += 1;
    }

    let mut count = 0;
    for _house in houses.iter() {
        count += 1;
    }
    println!("Part 1: {}", count);
}
