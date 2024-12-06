use std::env;
fn main() {
    // get command line arguments
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }

    let input_file = &args[1];

    // read the file
    let contents =
        std::fs::read_to_string(input_file).expect("Something went wrong reading the file");

    let mut floor = 0;
    let mut count = 0;
    let mut basement = 0;
    // loop through the lines
    for line in contents.lines() {
        // loop through the characters
        for c in line.chars() {
            if c == '(' {
                floor += 1;
            } else if c == ')' {
                floor -= 1;
            }

            count += 1;
            if floor == -1 && basement == 0 {
                basement = count;
            }
        }
    }
    println!("Part 1: {}", floor);
    println!("Part 2: {}", basement);
}
