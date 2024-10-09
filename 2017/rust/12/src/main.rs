use std::{
    collections::{HashMap, HashSet, VecDeque},
    env,
    error::Error,
    fs,
};

fn main() -> Result<(), Box<dyn Error>> {
    let content = fs::read_to_string(env::args().nth(1).ok_or("Please provide a file path")?)?;
    let graph: HashMap<_, _> = content
        .lines()
        .map(|line| {
            let (id, conns) = line.split_once(" <-> ").unwrap();
            (
                id.parse().unwrap(),
                conns.split(", ").map(|s| s.parse().unwrap()).collect(),
            )
        })
        .collect();

    println!("Part 1: {}", bfs(0, &graph).len());

    let mut visited = HashSet::new();
    let mut group_count = 0;
    for &node in graph.keys() {
        if visited.insert(node) {
            group_count += 1;
            visited.extend(bfs(node, &graph));
        }
    }

    println!("Part 2: {}", group_count);
    Ok(())
}

fn bfs(start: usize, graph: &HashMap<usize, Vec<usize>>) -> HashSet<usize> {
    let mut visited = HashSet::from([start]);
    let mut queue = VecDeque::from([start]);

    while let Some(current) = queue.pop_front() {
        if let Some(neighbors) = graph.get(&current) {
            neighbors
                .iter()
                .filter(|&&n| visited.insert(n))
                .for_each(|&n| queue.push_back(n));
        }
    }

    visited
}
