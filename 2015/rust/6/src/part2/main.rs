struct Action {
    act: String,
    start: (i32, i32),
    end: (i32, i32),
}

impl Action {
    fn new(act: String, start: (i32, i32), end: (i32, i32)) -> Action {
        Action { act, start, end }
    }
}

struct LightGrid {
    grid: Vec<Vec<i32>>,
}

impl LightGrid {
    fn new() -> LightGrid {
        LightGrid {
            grid: vec![vec![0; 1000]; 1000],
        }
    }

    fn apply_action(&mut self, action: Action) {
        for i in action.start.0..=action.end.0 {
            for j in action.start.1..=action.end.1 {
                match action.act.as_str() {
                    "turn on" => self.grid[i as usize][j as usize] += 1,
                    "turn off" => {
                        self.grid[i as usize][j as usize] =
                            std::cmp::max(0, self.grid[i as usize][j as usize] - 1)
                    }
                    "toggle" => {
                        self.grid[i as usize][j as usize] += 2;
                    }
                    _ => (),
                }
            }
        }
    }
}

fn get_action(line: &str) -> Action {
    let words: Vec<&str> = line.split_whitespace().collect();
    let act: String;
    let start: (i32, i32);
    let end: (i32, i32);

    if words[0] == "turn" {
        act = format!("{} {}", words[0], words[1]);
        start = (
            words[2].split(",").collect::<Vec<&str>>()[0]
                .parse()
                .unwrap(),
            words[2].split(",").collect::<Vec<&str>>()[1]
                .parse()
                .unwrap(),
        );
        end = (
            words[4].split(",").collect::<Vec<&str>>()[0]
                .parse()
                .unwrap(),
            words[4].split(",").collect::<Vec<&str>>()[1]
                .parse()
                .unwrap(),
        );
    } else {
        act = words[0].to_string();
        start = (
            words[1].split(",").collect::<Vec<&str>>()[0]
                .parse()
                .unwrap(),
            words[1].split(",").collect::<Vec<&str>>()[1]
                .parse()
                .unwrap(),
        );
        end = (
            words[3].split(",").collect::<Vec<&str>>()[0]
                .parse()
                .unwrap(),
            words[3].split(",").collect::<Vec<&str>>()[1]
                .parse()
                .unwrap(),
        );
    }

    Action::new(act, start, end)
}

fn main() {
    // get command line arguments
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: cargo run --bin part2 <input.txt>");
        return;
    }

    // read input file
    let filename = &args[1];
    let contents =
        std::fs::read_to_string(filename).expect("Something went wrong reading the file");

    let lines: Vec<&str> = contents.lines().collect();

    let mut light_grid = LightGrid::new();

    for line in lines {
        let action = get_action(line);
        light_grid.apply_action(action);
    }

    let mut count = 0;
    for i in 0..1000 {
        for j in 0..1000 {
            count += light_grid.grid[i][j];
        }
    }

    println!("Part 2: {}", count);
}
