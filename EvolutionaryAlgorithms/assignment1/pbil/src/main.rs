#![feature(non_ascii_idents)]

use std::fs::File;
use std::io::{prelude::*, BufRead, BufReader};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use rand::distributions::Uniform;
use rand::prelude::*;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn load_data() -> Result<(Vec<Vec<u8>>, Vec<u8>)> {
    let ruleset = {
        let reader = BufReader::new(File::open("ClassificationRules.txt")?);
        let mut ruleset = vec![];
        for line in reader.lines().filter_map(std::result::Result::ok) {
            let rules = line
                .split_whitespace()
                .filter_map(|v| v.parse::<f32>().ok())
                .map(|v| v as u8)
                .collect::<Vec<u8>>();
            ruleset.push(rules);
        }
        ruleset
    };

    let model = {
        let mut file = File::open("ImageExpertReduced.txt")?;
        let mut line = String::new();
        file.read_to_string(&mut line)?;

        line.split_whitespace()
            .filter_map(|v| v.parse::<f32>().ok())
            .map(|v| v as u8)
            .collect::<Vec<u8>>()
    };

    Ok((ruleset, model))
}

fn random_population(p: &[f32], n: usize) -> Vec<Vec<bool>> {
    let mut population = vec![];

    let mut rng = rand::thread_rng();
    for _ in 0..n {
        let invididual = rng
            .sample_iter(Uniform::new(0.0, 1.0))
            .zip(p)
            .map(|(v, &p)| v < p)
            .collect::<Vec<_>>();
        population.push(invididual)
    }

    population
}

fn pbil<F, G>(f: F, mut c: G, n: usize, d: usize, θ1: f32, θ2: f32, θ3: f32) -> Vec<bool>
where
    F: Fn(&[bool]) -> f32,
    G: FnMut(f32) -> bool,
{
    let mut p = std::iter::repeat(0.5).take(d).collect::<Vec<_>>();
    let mut pop = random_population(&p, n);
    let mut rng = rand::thread_rng();

    let mut best_value = std::f32::NEG_INFINITY;
    let mut best_individual = vec![];

    loop {
        let (value, individual) = pop
            .into_iter()
            .map(|ind| {
                let v = f(&ind);
                (v, ind)
            })
            .max_by(|a, b| a.partial_cmp(&b).unwrap())
            .unwrap();

        for k in 0..d {
            p[k] = p[k] * (1.0 - θ1) + (if individual[k] { θ1 } else { 0.0 });
        }

        for k in 0..d {
            if rng.gen_range(0.0, 1.0) < θ2 {
                p[k] = p[k] * (1.0 - θ3) + (rng.gen_range(0usize, 1usize) as f32) * θ3;
            }
        }

        pop = random_population(&p, n);

        println!("{}", value);
        if value > best_value {
            best_value = value;
            best_individual = individual;
        }

        if c(value) {
            break;
        }
    }

    println!("Best score: {}", best_value);
    println!("{:?}", best_individual);

    best_individual
}

fn one_max(mask: &[bool]) -> f32 {
    mask.iter().filter(|&i| *i).count() as _
}

fn deceptive_one_max(mask: &[bool]) -> f32 {
    let cnt = mask.iter().filter(|&i| *i).count();
    (if cnt == 0 { mask.len() + 1 } else { cnt }) as f32
}

fn k_deceptive_one_max(k: usize) -> impl Fn(&[bool]) -> f32 {
    move |mask| mask.chunks(k).map(deceptive_one_max).sum::<f32>()
}

fn equals_to(to: f32) -> impl Fn(f32) -> bool {
    move |v| (v - to).abs() <= 1e-5
}

fn main() -> Result<()> {
    let (ruleset, model) = load_data()?;
    let d = model.len();

    let evaluator = move |mask: &[bool]| -> f32 {
        let mut result = 0;

        for i in 1..model.len() {
            let mut a = [0usize; 3];

            for rule in ruleset.iter().enumerate().filter(|(i, _)| mask[*i]) {
                a[(rule.1[i] - 1) as usize] += 1;
            }

            let cat = a.iter().enumerate().max_by_key(|v| v.1).unwrap().0 + 1;
            if cat == model[i] as _ {
                result += 1;
            }
        }

        result as _
    };

    // let stop = Arc::new(AtomicBool::new(false));
    // let s = stop.clone();

    // ctrlc::set_handler(move || stop.store(true, Ordering::SeqCst))?;
    // let halt = move |_| s.load(Ordering::SeqCst);

    // pbil(evaluator, halt, 1000, d, 0.05, 0.05, 0.02);
    // pbil(evaluator, halt, 2000, d, 0.02, 0.05, 0.05);

    // pbil(one_max, equals_to(100.0), 5000, 100, 0.1, 0.02, 0.05);
    // pbil(deceptive_one_max, equals_to(11.0), 50, 10, 0.05, 0.15, 0.2); // Fajnie
    // działa nawet dla malych populacji
    // pbil(k_deceptive_one_max(3),
    // equals_to(16.0), 50, 12, 0.05, 0.15, 0.2); pbil(k_deceptive_one_max(5),
    // equals_to(60.0), 500, 50, 0.05, 0.15, 0.2);
    pbil(
        k_deceptive_one_max(30),
        equals_to(124.0),
        500,
        120,
        0.05,
        0.15,
        0.2,
    );

    Ok(())
}
