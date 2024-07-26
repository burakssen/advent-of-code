use md5;

fn main() {
    // get command line arguments
    let args: Vec<String> = std::env::args().collect();
    // check if there are enough arguments
    if args.len() < 2 {
        // print usage
        println!("Usage: {} <input.txt>", args[0]);
        // exit with error
        std::process::exit(1);
    }

    // read the file
    let prefix = std::fs::read_to_string(&args[1]).expect("Failed to read file");

    let mut i = 0;
    let mut leading_5_found = false;

    loop {
        let input = format!("{}{}", prefix, i);
        let hash = md5::compute(input);
        let hash_str = format!("{:x}", hash);

        if hash_str.starts_with("000000") {
            println!("Part 2: {}", i);
            break;
        }

        // check if the hash has 5 leading zeros
        if hash_str.starts_with("00000") && !leading_5_found {
            // print the hash
            println!("Part 1: {}", i);
            // set the flag
            leading_5_found = true;
        }

        i += 1;
    }
}
