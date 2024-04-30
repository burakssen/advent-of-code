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
    let contents = std::fs::read_to_string(input_file)
        .expect("Something went wrong reading the file");

    let mut count = 0;
    // loop through the lines
    for line in contents.lines() {
        // loop through the characters
        for c in line.chars() {
            if c == '(' {
                count += 1;
            }
            else if c == ')' {
                count -= 1;
            }
        
        }
    }
    println!("{}", count);
}