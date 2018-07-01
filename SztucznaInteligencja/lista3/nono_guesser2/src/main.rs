extern crate rpds;

use std::error::Error;
use std::fs::File;
use std::io::{Write, BufRead, BufReader};
use std::result;

use rpds::List;

type Result<T> = result::Result<T, Box<Error>>;

#[derive(Clone, Copy, PartialEq, Debug)]
enum Block {
    FILL,
    EMPTY,
    UNKNOWN
}

type Line = Vec<Block>;
type LineP = List<Block>;
type State = (Option<Block>, usize, usize);

struct LineIterator<'a> {
    initial: &'a Line,
    queue: Vec<(LineP, usize, State)>,
    line:  &'a [usize]
}

struct LineIteratorOld<'a> {
    queue: Vec<(Line, usize, State)>,
    line:  &'a [usize]
}

impl<'a> LineIterator<'a> {
    fn new( line: &'a [usize], known: &'a Line ) -> Self {
        LineIterator {
            initial: known,
            queue: vec![ (List::default(), 0, (None, 0, 0)) ],
            line:  line
        }
    }

    fn union( &mut self ) -> Option<(usize, Line)> {
        use Block::*;

        let mut item = match self.next() {
            Some( i ) => i.into_iter().cloned().collect::<Vec<_>>(),
            None => return None
        };

        let mut count = 1;

        for it in self {
            count += 1;

            for (i, &e) in it.iter().enumerate() {
                if item[ i ] != e { item[ i ] = UNKNOWN; }
            }
        }

        Some( (count, item.into_iter().rev().collect::<Vec<_>>()) )
    }
}

impl<'a> LineIteratorOld<'a> {
    fn new( line: &'a [usize], known: Line ) -> Self {
        LineIteratorOld {
            queue: vec![ (known, 0, (None, 0, 0)) ],
            line:  line
        }
    }

    fn union( &mut self ) -> Option<(usize, Line)> {
        use Block::*;

        let mut item = match self.next() {
            Some( i ) => i,
            None => return None
        };

        let mut count = 1;

        for it in self {
            count += 1;

            for (i, &e) in it.iter().enumerate() {
                if item[ i ] != e { item[ i ] = UNKNOWN; }
            }
        }

        Some( (count, item) )
    }
}

impl<'a> Iterator for LineIteratorOld<'a> {
    type Item = Line;

    fn next( &mut self ) -> Option<Self::Item> {
        use Block::*;

        'main: while let Some( item ) = self.queue.pop() {
            let (mut partial, off, state) = item;
            let (mut block, mut cnt, mut n) = state;

            for i in off..partial.len() {
                if partial[ i ] == UNKNOWN {
                    let mut new_partial = partial.clone();
                    new_partial[ i ] = EMPTY;
                    self.queue.push( (new_partial, i, (block, cnt, n)) );

                    partial[ i ] = FILL;
                }

                if let Some( ref mut last ) = block {
                    if *last != partial[ i ] {
                        if *last == FILL {
                            if n < self.line.len() && cnt == self.line[ n ] {
                                n += 1;
                            } else { continue 'main; }
                        }

                        *last = partial[ i ];
                        cnt = 1;
                    } else { cnt += 1; }
                } else {
                    block = Some( partial[ i ] );
                    cnt = 1;
                }
            }

            if n >= self.line.len() && block == Some( FILL ) {
                continue;
            }

            if block == Some( FILL ) && self.line[ n ] == cnt {
                n += 1;
            }

            if n == self.line.len() {
                return Some( partial );
            }
        }

        None
    }
}

impl<'a> Iterator for LineIterator<'a> {
    type Item = LineP;

    fn next( &mut self ) -> Option<Self::Item> {
        use Block::*;

        'main: while let Some( item ) = self.queue.pop() {
            let (mut partial, off, state) = item;
            let (mut block, mut cnt, mut n) = state;

            for i in off..self.initial.len() {
                if i == partial.len() {
                    if self.initial[ i ] == UNKNOWN {
                        let mut new_partial = partial.push_front( EMPTY );
                        self.queue.push( (new_partial, i, (block, cnt, n)) );

                        partial = partial.push_front( FILL );
                    } else {
                        partial = partial.push_front( self.initial[ i ] );
                    }
                }

                let current = *partial.first().expect( "WTF" );

                if let Some( ref mut last ) = block {
                    if *last != current {
                        if *last == FILL {
                            if n < self.line.len() && cnt == self.line[ n ] {
                                n += 1;
                            } else { continue 'main; }
                        }

                        *last = current;
                        cnt = 1;
                    } else { cnt += 1; }
                } else {
                    block = Some( current );
                    cnt = 1;
                }
            }

            if n >= self.line.len() && block == Some( FILL ) {
                continue;
            }

            if block == Some( FILL ) && self.line[ n ] == cnt {
                n += 1;
            }

            if n == self.line.len() {
                return Some( partial );
            }
        }

        None
    }
}

fn read_input( ) -> Result<(Vec<Vec<usize>>, Vec<Vec<usize>>)> {
    let file = File::open( "zad_input.txt" )?;
    let reader = BufReader::new( file );

    let mut lines = reader.lines().filter_map( |l| l.ok() );
    let size = lines.next()
                    .ok_or_else( || "Invalid input!" )?
                    .split_whitespace()
                    .filter_map( |v| v.parse::<usize>().ok() )
                    .collect::<Vec<_>>();

    let (mut vert_in, mut horiz_in) = (vec![], vec![]);

    for _ in 0..size[ 0 ] {
        let line = lines.next().ok_or_else( || "Unexcepted end of file!" )?;
        let values = line.split_whitespace()
                         .filter_map( |v| v.parse::<usize>().ok() )
                         .collect::<Vec<_>>();
        horiz_in.push( values );
    }

    for _ in 0..size[ 1 ] {
        let line = lines.next().ok_or_else( || "Unexcepted end of file!" )?;
        let values = line.split_whitespace()
                         .filter_map( |v| v.parse::<usize>().ok() )
                         .collect::<Vec<_>>();
        vert_in.push( values );
    }

    Ok( (vert_in, horiz_in) )
}

fn write_output( out: & [Vec<Block>] ) -> Result<()> {
    let mut file = File::create( "zad_output.txt" )?;

    for line in out.iter() {
        let text = line.iter()
                       .map( |&b| if b == Block::FILL { '#' } else { '.' } )
                       .collect::<String>();
        writeln!( file, "{}", text )?;
    }

    Ok( () )
}

fn resolve_by_guessing(
    vert_in: & [Vec<usize>],
    horiz_in: & [Vec<usize>],
    vertical: &mut Vec<Vec<Block>>,
    horizontal: &mut Vec<Vec<Block>>,
    vertical_done: &mut Vec<bool>,
    horizontal_done: &mut Vec<bool>
) -> bool {
    let mut changed = true;

    while changed {
        changed = false;

        for (i, horiz) in horiz_in.iter().enumerate() {
            let new = LineIteratorOld::new( horiz, horizontal[ i ].clone() ).union();
            if let Some( (cnt, new) ) = new {
                horizontal_done[ i ] = cnt == 1;
                if new != horizontal[ i ] {
                    horizontal[ i ] = new;
                    for (j, other) in vertical.iter_mut().enumerate() {
                        other[ i ] = horizontal[ i ][ j ];
                    }

                    changed = true;
                }
            } else { return false; }
        }

        for (i, vert) in vert_in.iter().enumerate() {
            let new = LineIteratorOld::new( vert, vertical[ i ].clone() ).union();
            if let Some( (cnt, new) ) = new {
                vertical_done[ i ] = cnt == 1;
                if new != vertical[ i ] {
                    vertical[ i ] = new;
                    for (j, other) in horizontal.iter_mut().enumerate() {
                        other[ i ] = vertical[ i ][ j ];
                    }

                    changed = true;
                }
            } else { return false; }
        }
    }

    true
}

fn application() -> Result<()> {
    use Block::*;

    let (vert_in, horiz_in) = read_input()?;

    let mut vertical    = vec![ vec![ UNKNOWN; horiz_in.len() ]; vert_in.len() ];
    let mut horizontal  = vec![ vec![ UNKNOWN; vert_in.len() ]; horiz_in.len() ];

    let mut vertical_done   = vec![ false; vert_in.len() ];
    let mut horizontal_done = vec![ false; horiz_in.len() ];

    let mut cache : Vec<Vec<Line>> = vec![];

    loop {
        let ok = resolve_by_guessing(
            & vert_in, & horiz_in,
            &mut vertical, &mut horizontal,
            &mut vertical_done, &mut horizontal_done
        );

        if ! ok {
            let state = cache.pop().unwrap();
            vertical = state;
            for (i, line) in vertical.iter().enumerate() {
                for (j, &v) in line.iter().enumerate() {
                    horizontal[ j ][ i ] = v;
                }
            }

            continue;
        }

        let first_bad = horizontal_done.iter().enumerate().filter( |&(_, &v)| !v ).next();
        if let Some( (j, _) ) = first_bad {
            let (i, _) = horizontal[ j ].iter().enumerate().filter( |&(_, &v)| v == UNKNOWN ).next().unwrap();

            let mut new_vertical = vertical.clone();
            new_vertical[ i ][ j ] = FILL;
            cache.push( new_vertical );

            vertical[ i ][ j ] = EMPTY;
            horizontal[ j ][ i ] = EMPTY;
        } else { break; }
    }

    write_output( & horizontal )?;

    Ok( () )
}

fn main() {
    if let Err( e ) = application() {
        println!( "[Error] {}", e );
    }
}
