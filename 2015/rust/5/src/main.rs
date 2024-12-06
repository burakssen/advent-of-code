fn main() {
    // get command line arguments
    let args: Vec<String> = std::env::args().collect();
    // check if there are enough arguments
    if args.len() < 2 {
        // print usage
        println!("Usage: cargo run <input.txt>");
        // exit with error
        std::process::exit(1);
    }

    // read the file
    let contents = std::fs::read_to_string(&args[1]).expect("Failed to read file");
    // print the file contents
    let mut part1_count = 0;
    let mut part2_count = 0;
    for line in contents.lines() {
        if check_good_string_part1(&line.to_string()) {
            part1_count += 1;
        }

        if check_good_string_part2(&line.to_string()) {
            part2_count += 1;
        }
    }

    println!("Part 1: {}", part1_count);
    println!("Part 2: {}", part2_count);
}

fn check_good_string_part1(s: &String) -> bool {
    let mut vowels = 0;
    let mut double = false;
    let mut bad = false;
    let mut last = ' ';
    for c in s.chars() {
        if c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' {
            vowels += 1;
        }
        if c == last {
            double = true;
        }
        if (last == 'a' && c == 'b')
            || (last == 'c' && c == 'd')
            || (last == 'p' && c == 'q')
            || (last == 'x' && c == 'y')
        {
            bad = true;
        }
        last = c;
    }
    vowels >= 3 && double && !bad
}

fn check_good_string_part2(s: &String) -> bool {
    let mut double = false;
    let mut repeat = false;
    let mut last = ' ';
    let mut last_last = ' ';
    for c in s.chars() {
        if c == last_last {
            repeat = true;
        }
        if last != ' ' {
            let pair = format!("{}{}", last, c);
            if s.matches(&pair).count() > 1 {
                double = true;
            }
        }
        last_last = last;
        last = c;
    }
    double && repeat
}
