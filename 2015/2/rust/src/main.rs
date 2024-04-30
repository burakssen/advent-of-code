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

    let mut total = 0;
    for line in lines {
        // split line into words by x
        let words: Vec<&str> = line.split('x').collect();
        // convert words to integers
        let mut sides: Vec<i32> = words.iter().map(|x| x.parse().unwrap()).collect();
        // sort sides in ascending order
        sides.sort();
        // calculate the area of the smallest side
        let area = sides[0] * sides[1];
        // calculate the total area
        let total_area = 2 * sides[0] * sides[1] + 2 * sides[1] * sides[2] + 2 * sides[2] * sides[0];

        total += total_area + area;
    }

    println!("{}", total);
}
