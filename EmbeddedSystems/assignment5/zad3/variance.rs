use std::io::{self, BufRead, BufReader};
use std::f32;

// $ rustc variance.rs
// $ ./variance </dev/ttyUSB0

fn main() -> Result<(), Box<std::error::Error>> {
    let reader = BufReader::new( io::stdin() );
    let mut values = Vec::<f32>::default();

    let mut lines = reader.lines();

    for i in 1.. {
        values.clear();

        for line in lines.by_ref() {
            let line = line?;

            if line.is_empty() {
                break;
            }

            values.push( line.parse().map_err( |e| format!( "{}: {}", e, line ) )? );
        }

        let mean = values.iter().sum::<f32>() / values.len() as f32;
        let variance = values
                        .iter()
                        .map( |v| (mean - v).powf( 2. ) )
                        .sum::<f32>() / values.len() as f32;

        println!( "Set of data #{} ({})", i, values.len() );
        println!( "Mean: {}", mean );
        println!( "Variance: {}", variance );

        print!( "\n" );
    }

    Ok( () )
}
