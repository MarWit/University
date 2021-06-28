use std::convert::TryFrom;
use std::time::Duration;

use criterion::*;
use rand::distributions::Alphanumeric;
use rand::prelude::*;

use ::algorithms::*;

const KB: usize = 1024;
const SIZES: [usize; 8] = [
    KB,
    2 * KB,
    4 * KB,
    8 * KB,
    16 * KB,
    32 * KB,
    64 * KB,
    128 * KB,
];
const FIBO_SIZES: [usize; 8] = [1597, 2584, 4181, 10946, 17711, 46368, 75025, 196418];

macro_rules! new_benchmark {
    ($name:ident, $bencher:expr, $sizes:expr) => {
        fn $name(c: &mut Criterion) {
            let mut group = c.benchmark_group(stringify!($name));
            for size in $sizes.iter() {
                group.throughput(Throughput::Bytes(*size as u64 * 4));
                group.measurement_time(Duration::from_secs(10));
                group.bench_with_input(
                    BenchmarkId::from_parameter(format!("{}KB", *size * 4 / KB)),
                    size,
                    $bencher,
                );
            }
        }
    };
    ($name:ident, $bencher:expr) => {
        new_benchmark!($name, $bencher, SIZES);
    };
}

fn random_string(size: usize) -> String {
    Alphanumeric
        .sample_iter(rand::thread_rng())
        .take(size)
        .chain(std::iter::once(b'$'))
        .map(|c| c as char)
        .collect()
}

fn fibonacci_string(size: usize) -> String {
    let mut a = "a".to_owned();
    let mut b = "b".to_owned();

    while b.len() < size {
        let new_b = a + &b;
        a = b;
        b = new_b;
    }

    b + "$"
}

fn prepare_random_test(size: usize) -> (Vec<u32>, Vec<u32>) {
    let input = random_string(size);
    let transformed = transformer(input);
    let sa = suffix_array(&transformed, ALPHABET_SIZE);
    (transformed, sa)
}

fn prepare_fibonacci_test(size: usize) -> (Vec<u32>, Vec<u32>) {
    let input = fibonacci_string(size);
    let transformed = transformer(input);
    let sa = suffix_array(&transformed, ALPHABET_SIZE);
    (transformed, sa)
}

new_benchmark!(benchmark_lcp_kasai_random, |b, &size| b.iter_with_setup(
    || prepare_random_test(size),
    |(input, sa)| lcp_kasai(&input, &sa)
));

new_benchmark!(
    benchmark_lcp_kasai_fibonacci,
    |b, &size| b.iter_with_setup(
        || prepare_fibonacci_test(size),
        |(input, sa)| lcp_kasai(&input, &sa)
    ),
    FIBO_SIZES
);

new_benchmark!(benchmark_lcp9_random, |b, &size| b.iter_with_setup(
    || prepare_random_test(size),
    |(input, sa)| lcp9(&input, &sa, ALPHABET_SIZE)
));

new_benchmark!(
    benchmark_lcp9_fibonacci,
    |b, &size| b.iter_with_setup(
        || prepare_fibonacci_test(size),
        |(input, sa)| lcp9(&input, &sa, ALPHABET_SIZE)
    ),
    FIBO_SIZES
);

new_benchmark!(benchmark_lcp6_random, |b, &size| b.iter_with_setup(
    || {
        let (input, sa) = prepare_random_test(size);
        (
            input,
            sa.into_iter()
                .map(i32::try_from)
                .filter_map(Result::ok)
                .collect::<Vec<_>>(),
        )
    },
    |(input, mut sa)| lcp6(&input, &mut sa, ALPHABET_SIZE)
));

new_benchmark!(
    benchmark_lcp6_fibonacci,
    |b, &size| b.iter_with_setup(
        || {
            let (input, sa) = prepare_fibonacci_test(size);
            (
                input,
                sa.into_iter()
                    .map(i32::try_from)
                    .filter_map(Result::ok)
                    .collect::<Vec<_>>(),
            )
        },
        |(input, mut sa)| lcp6(&input, &mut sa, ALPHABET_SIZE)
    ),
    FIBO_SIZES
);

criterion_group!(
    benchmarks,
    benchmark_lcp_kasai_random,
    benchmark_lcp9_random,
    benchmark_lcp6_random,
    benchmark_lcp_kasai_fibonacci,
    benchmark_lcp9_fibonacci,
    benchmark_lcp6_fibonacci,
);

criterion_main!(benchmarks);
