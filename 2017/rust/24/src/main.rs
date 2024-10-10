use std::{cell::RefCell, collections::HashSet, env, fs, io, rc::Rc};

type Component = (u32, u32);

fn parse_input(content: &str) -> Vec<Component> {
    content
        .lines()
        .map(|line| {
            let mut parts = line.split('/').map(|p| p.parse().unwrap());
            (parts.next().unwrap(), parts.next().unwrap())
        })
        .collect()
}

fn build_bridges(
    components: &[Component],
    used: &Rc<RefCell<HashSet<usize>>>,
    port: u32,
    strength: u32,
    length: u32,
) -> (u32, u32, u32) {
    components
        .iter()
        .enumerate()
        .filter(|&(i, &(a, b))| !used.borrow().contains(&i) && (a == port || b == port))
        .fold(
            (strength, length, strength),
            |(max_str, max_len, max_str_long), (i, &(a, b))| {
                used.borrow_mut().insert(i);
                let new_port = if a == port { b } else { a };
                let (new_str, new_len, new_str_long) =
                    build_bridges(components, used, new_port, strength + a + b, length + 1);
                used.borrow_mut().remove(&i);
                (
                    max_str.max(new_str),
                    max_len.max(new_len),
                    if new_len > max_len {
                        new_str_long
                    } else if new_len == max_len {
                        max_str_long.max(new_str_long)
                    } else {
                        max_str_long
                    },
                )
            },
        )
}

fn main() -> io::Result<()> {
    let content = fs::read_to_string(
        env::args()
            .nth(1)
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Provide a file path"))?,
    )?;
    let components = parse_input(&content);
    let (part1, _, part2) =
        build_bridges(&components, &Rc::new(RefCell::new(HashSet::new())), 0, 0, 0);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
    Ok(())
}
