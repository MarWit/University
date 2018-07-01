#![feature(box_syntax)]
#![feature(non_ascii_idents)]

use std::env;
use std::iter;
use std::error::Error;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::result;
use std::collections::HashMap;

type Result<T> = result::Result<T, Box<Error>>;

type Map = Vec<Vec<Surface>>;
type Vector<T> = (T, T);

macro_rules! saturated {
    ($a:expr, $b:expr, $v:expr) => {
        if $v < $a { $a }
        else if $a > $b { $b }
        else { $v }
    }
}

#[derive(Eq, PartialEq, Hash, Clone)]
struct State {
    pos: Vector<usize>,
    vel: Vector<i32>
}

impl State {
    pub fn actions( &self, map: & Map ) -> Box<Iterator<Item=Vector<i32>>> {
        let surface = self.surface( map );

        match surface {
            Surface::Road | Surface::Oil => box (-1..=1).flat_map( |i| iter::repeat( i ).zip( -1..=1 ) ),
            _ => box iter::empty()
        }
    }

    fn states<'a>( &'a self, diff: Vector<i32>, map: & Map ) -> Box<Iterator<Item=(f32, State)> + 'a> {
        let surface = self.surface( map );

        match surface {
            Surface::Road => box iter::once((1.0, self.apply( diff ))),
            Surface::Oil => box (-1..=1).flat_map( |i| iter::repeat( i ).zip( -1..=1 ) ).map( move |(dx, dy)| (1.0/9.0, self.apply( (diff.0 + dx, diff.1 + dy) )) ),
            _ => box iter::empty()
        }
    }

    fn apply( &self, diff: Vector<i32> ) -> State {
        let vel = (saturated!(-3, 3, self.vel.0 + diff.0), saturated!(-3, 3, self.vel.1 + diff.1));
        let pos = ((self.pos.0 as i32 + vel.0) as usize, (self.pos.1 as i32 + vel.1) as usize);

        State {
            vel,
            pos
        }
    }

    fn surface( &self, map: &Map ) -> Surface {
        map.get( self.pos.1 )
            .and_then( |e| e.get( self.pos.0 ) )
            .cloned()
            .unwrap_or( Surface::Grass )
    }
}

#[derive(Clone, PartialEq, Debug)]
enum Surface {
    Road,
    Oil,
    Finish,
    Grass
}

impl Surface {
    pub fn reward( &self ) -> f32 {
        match self {
            Surface::Road | Surface::Oil => -0.1,
            Surface::Finish              => 100.0,
            Surface::Grass               => -100.0
        }
    }
}

impl From<char> for Surface {
    fn from( from: char ) -> Surface {
        match from {
            '#' | 's' => Surface::Road,
            'o'       => Surface::Oil,
            'e'       => Surface::Finish,
            _         => Surface::Grass
        }
    }
}

fn load_file<P: AsRef<Path>>( path: P ) -> Result<Map> {
    let file = File::open( path )?;
    let reader = BufReader::new( file );

    let mut output = vec![];

    for line in reader.lines() {
        let line = line?;
        output.push( line.chars().map( |c| c.into() ).collect() );
    }

    Ok( output )
}

fn application() -> Result<()> {
    let filename = env::args()
                    .nth( 1 )
                    .ok_or_else( || "usage: cargo run --release -- <file>".to_owned() )?;

    let map = load_file( filename )?;
    let mut states = HashMap::new();

    for (j, row) in map.iter().enumerate() {
        for (i, v) in row.iter().enumerate() {
            if *v == Surface::Grass { continue; }

            let pos = (i, j);

            for vel in (-3..=3).flat_map( |i| iter::repeat( i ).zip( -3..=3 ) ) {
                states.insert( State { pos, vel }, (0f32, (0, 0)) );
            }
        }
    }

    loop {
        let mut θ = 0f32;

        let cached = states.clone();

        for (k, v) in &mut states {
            let actions = k.actions( &map ).map( |action| {
                let score = k.states( action, &map )
                                .into_iter()
                                .map( |(p, s)| {
                                    p * ( s.surface( &map ).reward( ) + 0.99 * cached.get( &s ).map( |v| v.0 ).unwrap_or( 0.0 ) )
                                } )
                                .sum::<f32>();

                (score, action)
            } );

            let best = actions.max_by( |a, b| (a.0).partial_cmp( & b.0 ).unwrap() );
            if let Some( best ) = best {
                let δ = (best.0 - v.0).abs();
                if δ > θ { θ = δ; }

                *v = best;
            }
        }

        if θ < 0.001 {
            break;
        }

        eprintln!( "θ = {}", θ );
    }

    for (k, v) in states {
        println!( "{} {} {} {} {} {}", k.pos.0, k.pos.1, k.vel.0, k.vel.1, (v.1).0, (v.1).1 );
    }

    Ok( () )
}

fn main() {
    if let Err( e ) = application() {
        println!( "[Error] {}", e );
    }
}
