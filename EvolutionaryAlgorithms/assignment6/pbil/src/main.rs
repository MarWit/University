#![feature(non_ascii_idents)]

use std::cell::RefCell;
use std::fs::File;
use std::io::{prelude::*, BufRead, BufReader};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use rand::distributions::Uniform;
use rand::prelude::*;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

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

fn umda<F, G>(f: F, mut c: G, n: usize, d: usize, m: usize) -> Vec<bool>
where
    F: Fn(&[bool]) -> f32,
    G: FnMut(f32) -> bool,
{
    let mut p = std::iter::repeat(0.5).take(d).collect::<Vec<_>>();
    let mut pop = random_population(&p, n);

    let mut best_value = std::f32::NEG_INFINITY;
    let mut best_individual = vec![];

    for i in 1.. {
        let mut scores = pop
            .iter()
            .enumerate()
            .map(|(i, v)| (f(v), i))
            .collect::<Vec<_>>();
        scores.sort_by(|a, b| b.partial_cmp(&a).unwrap());
        scores.truncate(m);

        if scores[0].0 > best_value {
            best_value = scores[0].0;
            best_individual = pop[scores[0].1].clone();
        }

        for j in 0..d {
            p[j] = scores
                .iter()
                .map(|(_, i)| if pop[*i][j] { 1.0 } else { 0.0 })
                .sum::<f32>()
                / m as f32;
        }

        // print!(
        //     "\r{}\t{:?} ({:?}){:20 }",
        //     best_value, best_individual, p, " "
        // );
        eprintln!("{} {}", i, best_value);

        if c(best_value) {
            break;
        }

        pop = random_population(&p, n);
    }

    println!();

    best_individual
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

    for i in 1.. {
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

        if value > best_value {
            best_value = value;
            best_individual = individual;
        }

        eprintln!("{} {}", i, best_value);

        if c(value) {
            break;
        }
    }

    // println!();
    // println!("Best score: {}", best_value);
    // println!("{:?}", best_individual);

    best_individual
}

fn cga<F, G>(f: F, mut c: G, d: usize, θ: f32) -> Vec<bool>
where
    F: Fn(&[bool]) -> f32,
    G: FnMut(f32) -> bool,
{
    let mut p = std::iter::repeat(0.5).take(d).collect::<Vec<_>>();
    let mut x = random_population(&p, 2);
    let mut scores = x.iter().map(|v| f(&v)).collect::<Vec<_>>();

    for i in 1.. {
        let (better, worse) = if scores[0] > scores[1] {
            (0, 1)
        } else {
            (1, 0)
        };
        for k in 0..d {
            if x[better][k] && !x[worse][k] {
                p[k] += θ;
            }
            if !x[better][k] && x[worse][k] {
                p[k] -= θ;
            }
        }

        x = random_population(&p, 2);
        scores = x.iter().map(|v| f(&v)).collect::<Vec<_>>();
        let better_score = if scores[0] > scores[1] {
            scores[0]
        } else {
            scores[1]
        };

        eprintln!("{} {}", i, better_score);
        if c(better_score) {
            break;
        }

        // print!("\r{:02}\t{:?}{:20 }", better_score, x, " ");
    }

    println!();

    x[if scores[0] > scores[1] { 0 } else { 1 }].clone()
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

fn max_iters(iters: usize) -> impl Fn(f32) -> bool {
    let cnt = RefCell::new(iters);
    move |_| {
        if *cnt.borrow() == 0 {
            true
        } else {
            *cnt.borrow_mut() -= 1;
            false
        }
    }
}

fn main() -> Result<()> {
    // let stop = Arc::new(AtomicBool::new(false));
    // let s = stop.clone();

    // ctrlc::set_handler(move || stop.store(true, Ordering::SeqCst))?;
    // let halt = move |_| s.load(Ordering::SeqCst);

    // pbil(evaluator, halt, 1000, d, 0.05, 0.05, 0.02);
    // pbil(evaluator, halt, 2000, d, 0.02, 0.05, 0.05);

    // pbil(one_max, equals_to(100.0), 500, 100, 0.1, 0.02, 0.05);
    // umda(one_max, equals_to(100.0), 500, 100, 20);
    // cga(one_max, equals_to(100.0), 100, 0.01);

    for _ in 0..1000 {
        // let r = pbil(deceptive_one_max, max_iters(100), 500, 10, 0.1, 0.2, 0.1);
        // let r = umda(deceptive_one_max, max_iters(100), 2000, 10, 20);
        let r = cga(deceptive_one_max, max_iters(100), 10, 0.01);
        println!("{}", deceptive_one_max(&r));
    }

    // pbil(
    //     k_deceptive_one_max(5),
    //     max_iters(10000),
    //     500,
    //     50,
    //     0.3,
    //     0.3,
    //     0.1,
    // );
    // umda(k_deceptive_one_max(5), max_iters(10000), 500, 50, 5);
    // umda(k_deceptive_one_max(3), equals_to(16.0), 100, 12, 20);
    // umda(k_deceptive_one_max(5), equals_to(60.0), 1000, 50, 500);

    // cga(k_deceptive_one_max(5), equals_to(16.0), 12, 0.5);
    // cga(k_deceptive_one_max(5), max_iters(10000), 50, 0.02);

    Ok(())
}
