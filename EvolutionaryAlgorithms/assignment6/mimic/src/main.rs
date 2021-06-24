use std::collections::HashSet;
use std::iter::FromIterator;

use ndarray::{s, Array, Array1, Array2, ArrayView1};
use ndarray_rand::RandomExt;
use rand::prelude::*;
use rand_distr::{Uniform, WeightedIndex};

type Scalar = f32;

fn mimic<F>(f: F, d: usize, population_size: usize)
where
    F: Fn(ArrayView1<u8>) -> Scalar + Send + Sync,
{
    let uniform = Uniform::new_inclusive(0, 1);
    let mut population = Array2::random((population_size, d), uniform);

    let n = if (population_size % 2) == 0 {
        ((population_size - 1) / 2, population_size / 2)
    } else {
        (population_size / 2, population_size / 2)
    };

    let mut scores = population
        .outer_iter()
        .map(&f)
        .enumerate()
        .collect::<Vec<_>>();
    scores.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());

    loop {
        print!(
            "\r{:.2}  {:?}",
            scores[0].1,
            population.slice(s![scores[0].0, ..])
        );

        let median = (scores[n.0].1 + scores[n.1].1) / 2.0;
        let median_cnt = if scores[n.1].1 <= median {
            n.1 + 1
        } else {
            n.0 + 1
        };

        let mut probs = vec![vec![0.0; d]; d];

        for x1 in 0..d {
            for x2 in 0..d {
                let mut max_cnt = 0;
                let mut cnt = 0;
                for (idx, _) in scores.iter().take(median_cnt) {
                    if population[[*idx, x2]] == 1 {
                        max_cnt += 1;
                        if population[[*idx, x1]] == 1 {
                            cnt += 1;
                        }
                    }
                }
                if cnt == 0 {
                    probs[x1][x2] = 0.00001;
                } else {
                    probs[x1][x2] = cnt as f32 / max_cnt as f32;
                }
            }
        }

        let mut remaining = (0..d).collect::<HashSet<_>>();
        let mut I = Array1::zeros(d);

        let mut single_probs = vec![0.00001; d];
        for (idx, _) in scores.iter().take(median_cnt) {
            for j in 0..d {
                if population[[*idx, j]] == 1 {
                    single_probs[j] += 1.0;
                }
            }
        }
        single_probs.iter_mut().for_each(|v| *v /= d as f32);
        let sps = single_probs.iter().sum::<f32>();
        let entropy = |i: usize, j: usize| -sps * probs[i][j].log2();

        I[d - 1] = (0..d)
            .min_by(|&a, &b| {
                let ea = -sps * single_probs[a].log2();
                let eb = -sps * single_probs[b].log2();
                ea.partial_cmp(&eb).unwrap()
            })
            .unwrap();
        remaining.remove(&I[d - 1]);

        for k in (0..d - 1).rev() {
            let mut min_idx = *remaining.iter().next().unwrap();
            for &j in &remaining {
                if entropy(j, I[k + 1]) < entropy(min_idx, I[k + 1]) {
                    min_idx = j;
                }
            }

            remaining.remove(&min_idx);
            I[k] = min_idx;
        }

        let uni01 = Uniform::new(0.0, 1.0);
        {
            let row1 = Array::from_iter(
                uni01
                    .sample_iter(rand::thread_rng())
                    .take(population_size)
                    .map(|v| if v < single_probs[I[d - 1]] { 1 } else { 0 }),
            );
            population.slice_mut(s![.., I[d - 1]]).assign(&row1);
        }

        for k in (0..d - 1).rev() {
            let row = Array::from_iter(
                uni01
                    .sample_iter(rand::thread_rng())
                    .take(population_size)
                    .map(|v| if v < probs[I[k]][I[k + 1]] { 1 } else { 0 }),
            );
            population.slice_mut(s![.., I[k]]).assign(&row);
        }

        scores = population
            .outer_iter()
            .map(&f)
            .enumerate()
            .collect::<Vec<_>>();
        scores.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
    }
}

fn one_max(mask: ArrayView1<u8>) -> f32 {
    -(mask.iter().filter(|&i| *i == 1).count() as f32)
}

fn main() {
    mimic(one_max, 20, 20);
}
