use std::env;
fn main() {
    // get command line arguments
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
}