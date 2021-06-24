use std::io::Cursor;

use sequitur::Sequitur;

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use lazy_static::lazy_static;
use lipsum::{lipsum_words, LIBER_PRIMUS};
use rand::prelude::*;
use serde::{de::DeserializeOwned, Serialize};

lazy_static! {
    static ref AVERAGE_LIPSUM_WORD_LENGTH: usize = {
        let lengths = LIBER_PRIMUS
            .split(" ")
            .map(|w| w.len())
            .collect::<Vec<usize>>();
        let len = lengths.len();
        lengths.into_iter().sum::<usize>() / len
    };
}

fn generate_lipsum(size: usize) -> String {
    lipsum_words(1 + size / *AVERAGE_LIPSUM_WORD_LENGTH)
}

fn generate_random(size: usize) -> Vec<u8> {
    rand::thread_rng()
        .sample_iter(rand::distributions::Standard)
        .take(size)
        .collect()
}

fn generate_random_dna_sequence(size: usize) -> Vec<char> {
    const NUCLEOTIDE: [char; 4] = ['A', 'C', 'T', 'G'];

    (0..size)
        .filter_map(|_| NUCLEOTIDE.iter().cloned().choose(&mut rand::thread_rng()))
        .collect::<Vec<_>>()
}

fn compress<V, T>(value: T) -> Vec<u8>
where
    V: Eq + std::hash::Hash + Copy + Serialize + DeserializeOwned,
    T: IntoIterator<Item = V>,
{
    let mut sequitur = Sequitur::<V>::new();
    for e in value {
        sequitur.put(e);
    }

    let mut output = vec![];
    sequitur.compress(&mut output);
    output
}

fn decompress<V>(data: &[u8]) -> Vec<V>
where
    V: Eq + std::hash::Hash + Copy + Serialize + DeserializeOwned,
{
    let mut data_cursor = Cursor::new(data);
    let sequitur = Sequitur::<V>::decompress(&mut data_cursor).unwrap();
    sequitur.rebuild()
}

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

macro_rules! new_benchmark {
    ($name:ident, $bencher:expr) => {
        fn $name(c: &mut Criterion) {
            let mut group = c.benchmark_group(stringify!($name));
            for size in SIZES.iter() {
                group.throughput(Throughput::Bytes(*size as u64));
                group.bench_with_input(
                    BenchmarkId::from_parameter(format!("{}KB", *size / KB)),
                    size,
                    $bencher,
                );
            }
        }
    };
}

new_benchmark!(benchmark_compression_lipsum, |b, &size| b.iter_with_setup(
    || generate_lipsum(size),
    |lipsum| compress(lipsum.chars())
));

new_benchmark!(benchmark_compression_random, |b, &size| b
    .iter_with_setup(|| generate_random(size), |data| compress(data)));

new_benchmark!(benchmark_compression_random_dna_sequence, |b, &size| b
    .iter_with_setup(
        || generate_random_dna_sequence(size),
        |data| compress(data)
    ));

fn benchmark_decompression_lipsum(c: &mut Criterion) {
    let mut group = c.benchmark_group("benchmark_decompression_lipsum");
    for &size in SIZES.iter() {
        let compressed = compress(generate_lipsum(size).chars());
        let compressed_size = compressed.len();

        group.throughput(Throughput::Bytes(compressed_size as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(format!("{}KB ({}KB)", size / KB, compressed_size / KB)),
            &compressed,
            |b, data| b.iter(|| decompress::<char>(data)),
        );
    }
}

fn benchmark_decompression_random(c: &mut Criterion) {
    let mut group = c.benchmark_group("benchmark_decompression_random");
    for &size in SIZES.iter() {
        let compressed = compress(generate_random(size));
        let compressed_size = compressed.len();

        group.throughput(Throughput::Bytes(compressed_size as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(format!("{}KB ({}KB)", size / KB, compressed_size / KB)),
            &compressed,
            |b, data| b.iter(|| decompress::<u8>(data)),
        );
    }
}

fn benchmark_decompression_random_dna_sequence(c: &mut Criterion) {
    let mut group = c.benchmark_group("benchmark_decompression_random_dna_sequence");
    for &size in SIZES.iter() {
        let compressed = compress(generate_random_dna_sequence(size));
        let compressed_size = compressed.len();

        group.throughput(Throughput::Bytes(compressed_size as u64));
        group.bench_with_input(
            BenchmarkId::from_parameter(format!("{}KB ({}KB)", size / KB, compressed_size / KB)),
            &compressed,
            |b, data| b.iter(|| decompress::<char>(data)),
        );
    }
}

criterion_group!(
    compression_benchmarks,
    benchmark_compression_lipsum,
    benchmark_compression_random,
    benchmark_compression_random_dna_sequence
);

criterion_group!(
    decompression_benchmarks,
    benchmark_decompression_lipsum,
    benchmark_decompression_random,
    benchmark_decompression_random_dna_sequence
);

criterion_main!(compression_benchmarks, decompression_benchmarks);
