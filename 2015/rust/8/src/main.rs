use std::env;
use std::fs::File;
use std::io::{self, BufRead};

// Count the characters in the code and the number of characters in the string representation.
fn count_chars(s: &str, code_chars: &mut usize, string_chars: &mut usize) {
    *code_chars += s.len();

    let bytes = s.as_bytes();
    let mut i = 1; // Skip first quote

    while i < bytes.len() - 1 {
        // Skip last quote
        if bytes[i] == b'\\' {
            if i + 1 < bytes.len() - 1 {
                match bytes[i + 1] {
                    b'\\' | b'"' => {
                        *string_chars += 1;
                        i += 1;
                    }
                    b'x' => {
                        if i + 3 < bytes.len() - 1 {
                            *string_chars += 1;
                            i += 3;
                        }
                    }
                    _ => *string_chars += 1,
                }
            }
        } else {
            *string_chars += 1;
        }
        i += 1;
    }
}

// Encode the string with escaping and return the encoded string and its length.
fn encode_string(s: &str) -> (String, usize) {
    let mut encoded = String::new();
    encoded.push('"');

    for ch in s.chars() {
        if ch == '"' || ch == '\\' {
            encoded.push('\\');
        }
        encoded.push(ch);
    }

    encoded.push('"');
    (encoded.clone(), encoded.len()) // Return a clone of the encoded string and its length
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("missing input file");
        std::process::exit(1);
    }

    let path = &args[1];
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);

    let mut total_code_chars = 0;
    let mut total_string_chars = 0;
    let mut total_orig_chars = 0;
    let mut total_enc_chars = 0;

    for line in reader.lines() {
        let line = line?;
        let line = line.trim();

        // Part 1: Count characters
        count_chars(line, &mut total_code_chars, &mut total_string_chars);

        // Part 2: Encode string
        total_orig_chars += line.len();

        let (_, enc_len) = encode_string(line);
        total_enc_chars += enc_len;
    }

    println!("Part 1: {}", total_code_chars - total_string_chars);
    println!("Part 2: {}", total_enc_chars - total_orig_chars);

    Ok(())
}
