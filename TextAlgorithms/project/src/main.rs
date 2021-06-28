mod algorithms;

use crate::algorithms::*;

#[cfg(feature = "mem_bench")]
#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

#[cfg(feature = "mem_bench")]
fn main() {
    use jemalloc_ctl::{epoch, stats};
    use rand::{distributions::Alphanumeric, prelude::*};
    use std::env;

    let size = env::var("DATA_LEN")
        .ok()
        .and_then(|v| v.parse::<usize>().ok())
        .unwrap_or(1024);

    println!("DATA_LEN={}KB", size);

    epoch::advance().unwrap();
    eprintln!("initial: {}KB", stats::allocated::read().unwrap() / 1024);

    let mut input = Alphanumeric
        .sample_iter(rand::thread_rng())
        .take(size * 1024)
        .chain(std::iter::once(b'$'))
        .map(|c| transform_char(c as char))
        .collect::<Vec<_>>();
    input.shrink_to_fit();

    epoch::advance().unwrap();
    eprintln!(
        "generated_data: {}KB",
        stats::allocated::read().unwrap() / 1024
    );

    let mut sa = suffix_array(&input, ALPHABET_SIZE);
    // .into_iter()
    // .map(|n| n as i32)
    // .collect::<Vec<_>>();

    epoch::advance().unwrap();
    eprintln!(
        "suffix_array: {}KB",
        stats::allocated::read().unwrap() / 1024
    );

    let sa = lcp_kasai(&input, &sa);
    // let sa = lcp9(&input, &sa, ALPHABET_SIZE);
    // lcp6(&input, &mut sa, ALPHABET_SIZE);
    // std::mem::drop(sa);
    epoch::advance().unwrap();
    eprintln!("after_lcp: {}KB", stats::allocated::read().unwrap() / 1024);
}

#[cfg(not(feature = "mem_bench"))]
fn main() {
    let input = transformer("banana$");
    println!("{:?}", input);

    let mut sa = suffix_array(&input, ALPHABET_SIZE)
        .into_iter()
        .map(|n| n as i32)
        .collect::<Vec<_>>();
    println!("SA = {:#?}", sa);

    // let lcp = lcp_kasai(&input, &sa);
    // println!("LCP = {:#?}", lcp);
    // let lcp = lcp9(&input, &sa, ALPHABET_SIZE);
    lcp6(&input, &mut sa, ALPHABET_SIZE);
    println!("LCP = {:#?}", sa);
    // println!("LCP = {:#?}", sa);
}
