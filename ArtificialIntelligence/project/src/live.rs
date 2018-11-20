use num_traits::FromPrimitive;

use utils::*;

lazy_static! {
    static ref CHORDS: Vec<Vec<Scalar>> = {
        let mut chords = vec![];

        // Major
        for i in 0..12 {
            let mut chord = vec![ 0.0; 12 ];
            chord[ i ] = 1.0;
            chord[ (i + 4) % 12 ] = 1.0;
            chord[ (i + 7) % 12 ] = 1.0;

            chords.push( chord );
        }

        // Minor
        for i in 0..12 {
            let mut chord = vec![ 0.0; 12 ];
            chord[ i ] = 1.0;
            chord[ (i + 3) % 12 ] = 1.0;
            chord[ (i + 7) % 12 ] = 1.0;

            chords.push( chord );
        }

        // Diminished
        for i in 0..12 {
            let mut chord = vec![ 0.0; 12 ];
            chord[ i ] = 1.0;
            chord[ (i + 3) % 12 ] = 1.0;
            chord[ (i + 6) % 12 ] = 1.0;

            chords.push( chord );
        }

        // Augumented
        for i in 0..12 {
            let mut chord = vec![ 0.0; 12 ];
            chord[ i ] = 1.0;
            chord[ (i + 4) % 12 ] = 1.0;
            chord[ (i + 8) % 12 ] = 1.0;

            chords.push( chord );
        }

        // Suspended 2th
        for i in 0..12 {
            let mut chord = vec![ 0.0; 12 ];
            chord[ i ] = 1.0;
            chord[ (i + 2) % 12 ] = 1.0;
            chord[ (i + 7) % 12 ] = 1.0;

            chords.push( chord );
        }

        // Suspended 4th
        for i in 0..12 {
            let mut chord = vec![ 0.0; 12 ];
            chord[ i ] = 1.0;
            chord[ (i + 5) % 12 ] = 1.0;
            chord[ (i + 7) % 12 ] = 1.0;

            chords.push( chord );
        }

        chords
    };
}

pub fn classify_chord( x: &[ Scalar ] ) -> Chord {
    const β: Scalar = 1.06;

    let mut scores = [0.0; 72];
    for (i, chord) in CHORDS.iter().enumerate() {
        let score = chord.iter()
                        .zip( x )
                        .map( |(c, x)| (1.0 - c) * x.powf( 2.0 ) )
                        .sum::<Scalar>().sqrt() / (9. * β);

        scores[ i ] = score;
    }

    let idx = scores.iter()
                    .enumerate()
                    .fold( (0, 1./0.), |a, v| {
                        if *v.1 < a.1 { (v.0, *v.1) }
                        else { a }
                    }).0;

    Chord( ChordRoot::from_usize( idx % 12 ).unwrap(), ChordType::from_usize( idx / 12 ).unwrap() )
}
