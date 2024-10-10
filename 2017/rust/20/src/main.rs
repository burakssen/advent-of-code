use std::{collections::HashMap, env, error::Error, fs};

#[derive(Debug, Clone)]
struct Particle {
    position: (i32, i32, i32),
    velocity: (i32, i32, i32),
    acceleration: (i32, i32, i32),
}

fn main() -> Result<(), Box<dyn Error>> {
    let content = fs::read_to_string(env::args().nth(1).ok_or("Provide a file path")?)?;
    let mut particles = parse_input(&content);

    // Part 1: Find the particle closest to <0,0,0>
    for _ in 0..1000 {
        update_particles(&mut particles);
    }
    let closest_particle_index = particles
        .iter()
        .enumerate()
        .map(|(i, p)| (i, manhattan_distance(p.position)))
        .min_by_key(|(_, d)| *d)
        .map(|(index, _)| index)
        .unwrap();

    println!("Part 1: {}", closest_particle_index);

    // Part 2: Remove particles that collide
    particles = parse_input(&content); // Reset particles for part 2
    for _ in 0..1000 {
        update_particles(&mut particles);
        particles = remove_colliding_particles(particles);
    }

    println!("Part 2: {}", particles.len());

    Ok(())
}

fn parse_input(input: &str) -> Vec<Particle> {
    input
        .lines()
        .map(|line| {
            let mut parts = line.split(", ").map(parse_tuple);
            Particle {
                position: parts.next().unwrap(),
                velocity: parts.next().unwrap(),
                acceleration: parts.next().unwrap(),
            }
        })
        .collect()
}

fn parse_tuple(input: &str) -> (i32, i32, i32) {
    let (x, y, z) = input[3..input.len() - 1]
        .split(',')
        .map(|n| n.trim().parse().unwrap())
        .collect::<Vec<_>>()
        .as_slice()
        .try_into()
        .map(|v: Vec<i32>| (v[0], v[1], v[2]))
        .unwrap(); // Assuming well-formed input
    (x, y, z)
}

fn update_particles(particles: &mut Vec<Particle>) {
    for particle in particles.iter_mut() {
        particle.velocity = (
            particle.velocity.0 + particle.acceleration.0,
            particle.velocity.1 + particle.acceleration.1,
            particle.velocity.2 + particle.acceleration.2,
        );
        particle.position = (
            particle.position.0 + particle.velocity.0,
            particle.position.1 + particle.velocity.1,
            particle.position.2 + particle.velocity.2,
        );
    }
}

fn manhattan_distance(position: (i32, i32, i32)) -> i32 {
    position.0.abs() + position.1.abs() + position.2.abs()
}

fn remove_colliding_particles(particles: Vec<Particle>) -> Vec<Particle> {
    let mut positions = HashMap::new();

    for (index, particle) in particles.iter().enumerate() {
        positions
            .entry(particle.position)
            .or_insert_with(Vec::new)
            .push(index);
    }

    positions
        .into_iter()
        .filter(|(_, indices)| indices.len() == 1)
        .flat_map(|(_, indices)| indices)
        .map(|index| particles[index].clone())
        .collect()
}
