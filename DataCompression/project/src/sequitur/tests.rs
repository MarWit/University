use std::io::Cursor;

use rand::distributions::Standard;
use rand::prelude::*;

use super::Sequitur;

fn test_single(string: &str) {
    let mut sequitur = Sequitur::new();
    for c in string.chars() {
        sequitur.put(c);
    }

    let rebuilded = sequitur.rebuild().into_iter().collect::<String>();
    assert_eq!(
        string, &rebuilded,
        "input string and rebuilded string differ"
    );

    let mut compressed = vec![];
    sequitur.compress(&mut compressed).unwrap();
    let mut compressed_cursor = Cursor::new(compressed);
    let decompressed = Sequitur::<char>::decompress(&mut compressed_cursor).unwrap();
    let decompressed_rebuilded = decompressed.rebuild().into_iter().collect::<String>();

    assert_eq!(
        string, &decompressed_rebuilded,
        "input string and reencoded rebuilded string differ"
    );
}

#[test]
fn arbitrary_strings() {
    const STRINGS: [&'static str; 8] = [
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris venenatis nibh vel enim \
         tincidunt tempor. Etiam faucibus lorem non luctus luctus. In in nulla nisl. Sed velit \
         nibh, efficitur eget ante eu, blandit dignissim neque. Fusce fringilla bibendum justo in \
         varius. Phasellus interdum lacus non urna consequat lacinia. Ut vel gravida mi, \
         pellentesque gravida nisi. Sed ullamcorper non elit id scelerisque. Donec elementum \
         pellentesque libero, sed ornare nunc tristique in. Suspendisse gravida efficitur \
         pretium. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac \
         turpis egestas. Praesent varius velit sit amet felis mattis, porttitor malesuada justo \
         venenatis. Morbi eget mauris tortor. Nulla in mauris sit amet felis laoreet hendrerit. \
         Donec vulputate efficitur ornare. Duis euismod urna sit amet eros dignissim, non \
         ultricies sapien imperdiet. Curabitur et nisi lorem. Nullam nec nunc eget erat venenatis \
         mollis nec ac nibh. Duis scelerisque vel augue sed condimentum. Aliquam vel justo sed.",
        "Lorem ipsum dolor sit amet",
        "abcdefghijkabcdefghijkabcdefghijkabcdefghijkabcdefghijkabcdefghijk",
        "abcabcabcabc",
        "abbabbabbabaa",
        "abbabbabbabaa",
        "abcdefghijk",
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    ];

    for string in &STRINGS {
        test_single(string);
    }
}

#[test]
fn random_lorem_ipsum() {
    const SIZES: [usize; 13] = [
        10, 20, 40, 80, 120, 250, 500, 1000, 2000, 4000, 8000, 1600, 2500,
    ];

    for &size in &SIZES {
        let random_lipsum = lipsum::lipsum_words(size);
        test_single(&random_lipsum);
    }
}

#[test]
fn random_bytes() {
    const MIN_LEN: usize = 1;
    const MAX_LEN: usize = 8192;
    const SAMPLES: usize = 10000;

    for _ in 0..SAMPLES {
        let len = rand::thread_rng().gen_range(MIN_LEN, MAX_LEN);
        let data = rand::thread_rng().sample_iter(Standard).take(len).collect::<Vec<u8>>();

        let mut sequitur = Sequitur::new();
        for b in &data {
            sequitur.put(*b);
        }

        let rebuilded = sequitur.rebuild().into_iter().collect::<Vec<u8>>();
        assert_eq!(
            data, rebuilded,
            "input bytes and rebuilded bytes differ"
        );

        let mut compressed = vec![];
        sequitur.compress(&mut compressed).unwrap();
        let mut compressed_cursor = Cursor::new(compressed);
        let decompressed = Sequitur::<u8>::decompress(&mut compressed_cursor).unwrap();
        let decompressed_rebuilded = decompressed.rebuild().into_iter().collect::<Vec<u8>>();

        assert_eq!(
            data, decompressed_rebuilded,
            "input bytes and reencoded rebuilded bytes differ"
        );
    }
}
