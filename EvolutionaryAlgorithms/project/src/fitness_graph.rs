use image::RgbaImage;
use std::path::PathBuf;

fn fitness(a: &RgbaImage, b: &RgbaImage) -> f32 {
    a.pixels()
        .zip(b.pixels())
        .map(|(p1, p2)| {
            p1.0.iter()
                .zip(p2.0.iter())
                .map(|(&v, &w)| (v as f32 - w as f32).powi(2))
                .sum::<f32>()
        })
        .sum()
}

pub fn main() {
    const INPUT_FILE: &'static str = "ermine.jpg";
    const FILES_PATH: &'static str = "output/";

    let original = image::io::Reader::open(INPUT_FILE)
        .unwrap()
        .decode()
        .unwrap()
        .into_rgba8();

    let w = original.width() as i32;
    let h = original.height() as i32;
    let worst_score = (255.0 * 255.0) * 3. * w as f32 * h as f32;

    let mut file_list = std::fs::read_dir(FILES_PATH)
        .unwrap()
        .filter_map(|entry| {
            let entry = entry.ok()?;
            if entry.metadata().ok()?.is_file() {
                Some(entry.path())
            } else {
                None
            }
        })
        .collect::<Vec<PathBuf>>();

    file_list.sort();

    let mut i = 1;
    for file in file_list {
        if let Some(current) = image::io::Reader::open(&file)
            .ok()
            .and_then(|f| f.decode().ok())
            .map(|img| img.into_rgba8())
        {
            println!("{} {}", i, fitness(&original, &current) / worst_score);
            i += 1;
        }
    }
}
