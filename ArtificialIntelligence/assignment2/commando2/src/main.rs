extern crate rpds;
extern crate rand;

use std::error::Error;
use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::result;
use std::collections::{HashMap, HashSet, BTreeSet, BinaryHeap, VecDeque};
use std::slice::Iter;
use std::cmp::Ordering;

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

#[derive(Eq, PartialEq)]
struct Item<I: Eq> {
    item:       I,
    priority:   usize
}

impl<I: Eq> Item<I> {
    fn new( item: I, priority: usize ) -> Self {
        Item {
            item,
            priority
        }
    }
}

impl<I: Eq> Ord for Item<I> {
    fn cmp( &self, other: & Self ) -> Ordering {
        other.priority.cmp( & self.priority )
    }
}

impl<I: Eq> PartialOrd for Item<I> {
    fn partial_cmp( &self, other: & Self ) -> Option<Ordering> {
        Some( self.cmp( other ) )
    }
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

fn calculate_distances( mut state: State, map: & Map ) -> HashMap<Position, usize> {
    let mut dists = HashMap::new();

    for distance in 1.. {
        let mut done = true;
        let states = Move::directions()
                            .flat_map( |&d| make_move( d, & state, map ) )
                            .collect::<State>();

        for state in states.iter() {
            if ! dists.contains_key( state ) {
                dists.insert( state.clone(), distance );
                done = false;
            }
        }

        if done { break; }

        state = states;
    }

    dists
}

fn astar_heuristic( state: & State, distances: & HashMap<Position, usize> ) -> usize {
    (*state.iter().map( |s| distances.get( s ).unwrap() ).max().unwrap() as f32 * 0.9) as usize// + (state.len() as f64 * 0.08) as usize
}

fn astar( state: State, goals: HashSet<Position>, map: Map ) -> Option<VecDeque<Move>> {
    let mut states  = HashMap::new();
    let mut queue   = BinaryHeap::new();
    let distances = calculate_distances( goals.iter().cloned().collect(), & map );

    states.insert( state.clone(), None::<(State, Move)> );
    queue.push( Item::new( (state, 0), 0 ) );

    while let Some( state ) = queue.pop() {
        let (state, dist) = state.item;

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

            let heuristic = dist + 1 + astar_heuristic( & new_state, & distances );

            queue.push( Item::new( (new_state.clone(), dist + 1), heuristic ) );
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

    let moves = astar( initial_state, goals, map );
    if let Some( moves ) = moves {
        let moves = moves.into_iter()
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
