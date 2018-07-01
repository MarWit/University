extern crate rpds;
extern crate rand;

use std::error::Error;
use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::result;
use std::collections::{HashMap, HashSet, BTreeSet, VecDeque};
use std::slice::Iter;

use rand::Rng;
use rpds::List;

type Result<T> = result::Result<T, Box<Error>>;
type Map = Vec<Vec<char>>;
type Position = (i32, i32);
type State = BTreeSet<Position>;

#[derive(Clone, Copy, PartialEq)]
enum Move {
    Up, Down, Left, Right
}

impl Move {
    pub fn directions() -> Iter<'static, Move> {
        use Move::*;
        const DIRECTIONS: [Move; 4] = [Up, Down, Left, Right];
        DIRECTIONS.into_iter()
    }

    pub fn as_char( &self ) -> char {
        use Move::*;

        match *self {
            Up    => 'U',
            Down  => 'D',
            Left  => 'L',
            Right => 'R'
        }
    }

    pub fn offset( &self ) -> Position {
        use Move::*;

        match *self {
            Up    => (0, -1),
            Down  => (0, 1),
            Left  => (-1, 0),
            Right => (1, 0)
        }
    }
}

fn read_input() -> Result<Map> {
    let file = File::open( "zad_input.txt" )?;
    let reader = BufReader::new( file );
    let mut out = vec![];

    for line in reader.lines() {
        out.push( line?.chars().collect() );
    }

    Ok( out )
}

fn check_wall( position: & Position, map: & Map ) -> bool {
    map[ position.1 as usize ][ position.0 as usize ] == '#'
}

fn make_move( direction: Move, state: & State, map: & Map ) -> State {
    let mut new_state = State::new();
    let (ox, oy) = direction.offset();

    for s in state {
        let new_pos = (s.0 + ox, s.1 + oy);
        if check_wall( & new_pos, map ) {
            new_state.insert( *s );
        } else {
            new_state.insert( new_pos );
        }
    }

    new_state
}

fn reduce_space( mut state: State, map: & Map ) -> (State, Vec<Move>) {
    const SAMPLE_SIZE: usize = 100;

    let mut moves = vec![];

    for _ in 0..SAMPLE_SIZE {
        if state.len() <= 2 {
            break;
        }

        let mut new_states = Move::directions()
                            .map( |&d| (d.clone(), make_move( d, &state, map )) )
                            .collect::<Vec<_>>();

        if rand::thread_rng().gen::<u8>() < 64u8 {
            let idx = rand::thread_rng().gen_range( 0, new_states.len() - 1 );
            let (dir, new_state) = new_states.into_iter().nth( idx ).unwrap();

            moves.push( dir );
            state = new_state;
        } else {
            let (dir, new_state) = new_states
                                    .into_iter()
                                    .min_by_key( |&(_, ref s)| s.len() )
                                    .unwrap();

            moves.push( dir );
            state = new_state;
        }
    }

    (state, moves)
}

fn bfs( state: State, goals: HashSet<Position>, map: Map ) -> Option<VecDeque<Move>> {
    let mut states = HashMap::new();
    let mut queue = VecDeque::new();

    states.insert( state.clone(), None::<(State, Move)> );
    queue.push_front( state );

    while let Some( state ) = queue.pop_back() {
        if goals.is_superset( & state.iter().cloned().collect::<HashSet<_>>() ) {
            let mut query = states.get( & state ).unwrap();
            let mut moves = VecDeque::new();

            while let Some( value ) = query.as_ref() {
                moves.push_front( value.1 );
                query = states.get( & value.0 ).unwrap();
            }

            return Some( moves );
        }

        for dir in Move::directions() {
            let new_state = make_move( *dir, & state, & map );
            if states.contains_key( & new_state ) { continue; }

            queue.push_front( new_state.clone() );
            states.insert( new_state, Some( (state.clone(), *dir) ) );
        }
    }

    None
}

fn application() -> Result<()> {
    let mut map = read_input()?;

    let mut initial_state = State::new();
    let mut goals = HashSet::new();

    for (y, line) in map.iter_mut().enumerate() {
        for (x, c) in line.iter_mut().enumerate() {
            match *c {
                'G' => { goals.insert( (x as i32, y as i32) ); },
                'S' => { initial_state.insert( (x as i32, y as i32) ); },
                'B' => {
                    goals.insert( (x as i32, y as i32) );
                    initial_state.insert( (x as i32, y as i32) );
                },
                _ => continue
            }

            *c = ' ';
        }
    }

    const SAMPLES: usize = 1000;
    let (reduced_state, moves) = (0..SAMPLES).map( |_| reduce_space( initial_state.clone(), & map ) )
                                                 .min_by_key( |&(ref s, _)| s.len() )
                                                 .unwrap();

    println!( "Space reduced!" );

    let moves_tail = bfs( reduced_state, goals, map );
    if let Some( moves_tail ) = moves_tail {
        let moves = moves.into_iter()
                         .chain( moves_tail.into_iter() )
                         .map( |m| m.as_char() )
                         .collect::<String>();

        let mut file = File::create( "zad_output.txt" )?;
        file.write( & moves.into_bytes() )?;
    }

    Ok( () )
}

fn main() {
    if let Err( e ) = application() {
        println!( "[Error] {}", e );
    }
}
