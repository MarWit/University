use std::collections::{VecDeque, HashSet, BinaryHeap};
use std::collections::hash_map::{HashMap, Entry};
use std::cmp::Ordering;
use std::fs;
use std::io::{self, Write, BufRead, BufReader};

type Map = Vec<Vec<char>>;
type Position = (i32, i32);
type Areas = HashSet<Position>;

#[derive(PartialEq, Clone)]
enum Move {
    Up, Down, Left, Right
}

impl Move {
    fn as_char( &self ) -> char {
        use Move::*;

        match *self {
            Left  => 'L',
            Right => 'R',
            Up    => 'U',
            Down  => 'D'
        }
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
struct State {
    guy:        Position,
    diamonds:   Vec<Position>
}

impl State {
    fn with_new_guy( &self, guy: Position ) -> State {
        State {
            guy,
            diamonds: self.diamonds.clone()
        }
    }
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

fn check_wall( pos: & Position, map: & Map ) -> bool {
    let (width, height) = (map[ 0 ].len() as i32, map.len() as i32);

    if pos.0 < 0 || pos.1 < 0 || pos.0 >= width || pos.1 >= height {
        return true;
    }

    if map[ pos.1 as usize ][ pos.0 as usize ] != '.' {
        return true;
    }

    false
}

fn generate_moves( base: & State, map: & Map ) -> Vec<(State, Move)> {
    use Move::*;

    const POSSIBLE_MOVES : &'static [(Position, Move)] = &[
        ((-1, 0), Left),    ((1, 0), Right),
        ((0, -1), Up),      ((0, 1), Down)
    ];

    let mut moves = vec![];

    for mov in POSSIBLE_MOVES {
        let new_pos = (base.guy.0 + (mov.0).0, base.guy.1 + (mov.0).1);

        if check_wall( & new_pos, map ) {
            continue;
        }

        let mut new_state = base.clone();
        new_state.guy = new_pos;

        let diamond = base.diamonds.iter().position( |&d| d == new_pos );
        if let Some( idx ) = diamond {
            let diamond_pos = (new_pos.0 + (mov.0).0, new_pos.1 + (mov.0).1);
            if check_wall( & diamond_pos, map ) {
                continue;
            }

            if base.diamonds.iter().position( |&d| d == diamond_pos ).is_some() {
                continue;
            }

            new_state.diamonds[ idx ] = diamond_pos;
        }

        moves.push( (new_state, mov.1.clone()) );
    }

    moves
}

fn manhattan_distance( a: & Position, b: & Position ) -> usize {
    ((a.0 - b.0).abs() + (a.1 - b.1).abs()) as usize
}

fn heuristic( next: & State, mut areas: Areas ) -> usize {
    let mut value = 0;

    for diamond in next.diamonds.iter() {
        let mut best = (i32::max_value(), i32::max_value());

        for area in areas.iter() {
            if manhattan_distance( & area, & diamond ) < manhattan_distance( & best, & diamond ) {
                best = *area;
            }
        }

        value += manhattan_distance( & best, & diamond );
        areas.remove( & best );
    }

    value
}

fn bfs( init: &State, map: &Map, areas: &Areas ) -> Vec<(State, Vec<Move>)> {
    let mut queue = VecDeque::new();
    let mut paths = HashMap::new();
    let mut visited = HashSet::new();
    let mut fullpaths = vec![];

    queue.push_back( init.guy );
    paths.insert( init.guy, vec![] );

    while let Some( position ) = queue.pop_front() {
        if visited.contains( &position ) { continue; }

        visited.insert( position );
        let state = init.with_new_guy( position );

        for (new_state, direction) in generate_moves( & state, map ) {
            let mut old_path = paths.get( & position ).unwrap().clone();
            old_path.push( direction );

            if new_state.diamonds.iter().any( |v| state.diamonds.iter().any( |vv| vv == v ) ) {
                fullpaths.push( (new_state, old_path) );
            } else {
                if ! paths.contains_key( & new_state.guy ) {
                    paths.insert( new_state.guy, old_path );
                }

                queue.push_back( new_state.guy );
            }
        }
    }

    fullpaths
}

fn bestfirst( base: State, map: Map, areas: Areas ) -> Option<String> {
    let mut queue = BinaryHeap::new();
    let mut states = HashMap::new();

    queue.push( Item::new( base.clone(), 0 ) );
    states.insert( base.clone(), None::<(State, Vec<Move>)> );

    while let Some( state ) = queue.pop() {
        let state = state.item;

        if areas.is_superset( & state.diamonds.iter().cloned().collect() ) {
            let mut letters = VecDeque::new();

            let mut query = states.get( & state ).expect( "that should not happen." );

            while let Some( saved ) = query.as_ref() {
                letters.push_front( (saved.1).clone() );
                query = states.get( & saved.0 ).unwrap();
            }

            return Some( letters.iter().flat_map( |v| v ).map( |m| m.as_char() ).collect() )
        }

        let slist = bfs( & state, & map, & areas );

        for (new_state, steps) in slist {
            if states.contains_key( & new_state ) { continue; }

            let priority = heuristic( & new_state, areas.clone() );

            queue.push( Item::new( new_state.clone(), priority ) );
            states.insert( new_state, Some( (state.clone(), steps) ) );
        }
    }

    None
}

fn read_map( file: &mut fs::File ) -> io::Result<Map> {
    let reader = BufReader::new( file );
    let mut map = vec![];

    for line in reader.lines() {
        map.push( line?.chars().collect() );
    }

    Ok( map )
}

fn application() -> io::Result<()> {
    const INPUT_FILE : &'static str = "zad_input.txt";
    const OUTPUT_FILE : &'static str = "zad_output.txt";

    let mut file = fs::File::open( INPUT_FILE )?;
    let mut map = read_map( &mut file )?;

    let mut areas = HashSet::new();
    let mut state = State {
        guy:        (-1, -1),
        diamonds:   vec![]
    };

    for (y, line) in map.iter_mut().enumerate() {
        for (x, c) in line.iter_mut().enumerate() {
            match *c {
                'K' => state.guy = (x as i32, y as i32),
                'B' => state.diamonds.push( (x as i32, y as i32) ),
                'G' => { areas.insert( (x as i32, y as i32) ); },
                '*' => {
                    state.diamonds.push( (x as i32, y as i32) );
                    areas.insert( (x as i32, y as i32) );
                },
                '+' => {
                    state.guy = (x as i32, y as i32);
                    areas.insert( (x as i32, y as i32) );
                },
                _   => continue
            };

            *c = '.';
        }
    }

    let result = bestfirst( state, map, areas );
    if let Some( moves ) = result {
        let mut out_file = fs::File::create( OUTPUT_FILE )?;
        out_file.write( & moves.into_bytes() )?;
    }

    Ok( () )
}

fn main() {
    if let Err( e ) = application() {
        println!( "[Error] {}", e );
    }
}
