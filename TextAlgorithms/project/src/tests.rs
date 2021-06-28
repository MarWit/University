use rand::distributions::Alphanumeric;
use rand::prelude::*;
use std::convert::TryFrom;

fn random_string(size: usize) -> String {
    Alphanumeric
        .sample_iter(rand::thread_rng())
        .take(size)
        .chain(std::iter::once(b'$'))
        .map(|c| c as char)
        .collect()
}

fn prepare_test(size: usize) -> (Vec<u32>, Vec<u32>) {
    let input = random_string(size);
    let transformed = super::transformer(input);
    let sa = super::suffix_array(&transformed, super::ALPHABET_SIZE);
    (transformed, sa)
}

#[test]
fn test_lcp9() {
    for _ in 0..10_000 {
        let (input, sa) = prepare_test(1024);
        let kasai_out = super::lcp_kasai(&input, &sa);
        let lcp9_out = super::lcp9(&input, &sa, super::ALPHABET_SIZE);

        assert_eq!(kasai_out, lcp9_out);
    }
}

#[test]
fn test_lcp6() {
    for _ in 0..10_000 {
        let (input, sa) = prepare_test(1024);
        let kasai_out = super::lcp_kasai(&input, &sa);
        let mut sa = sa
            .into_iter()
            .map(i32::try_from)
            .filter_map(Result::ok)
            .collect::<Vec<_>>();
        super::lcp6(&input, &mut sa, super::ALPHABET_SIZE);

        assert_eq!(kasai_out, sa);
    }
}
