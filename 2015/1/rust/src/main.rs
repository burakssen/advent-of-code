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

}