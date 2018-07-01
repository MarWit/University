use std::error::Error;
use std::fs::File;
use std::io::{Write, BufRead, BufReader};
use std::result;

type Result<T> = result::Result<T, Box<Error>>;

#[derive(Clone, Copy, PartialEq)]
enum Block {
    FILL,
    EMPTY,
    UNKNOWN
}

type Line = Vec<Block>;
type State = (Option<Block>, usize, usize);

struct LineIterator<'a> {
    queue: Vec<(Line, usize, State)>,
    line:  &'a [usize]
}

impl<'a> LineIterator<'a> {
    fn new( line: &'a [usize], known: Line ) -> Self {
        LineIterator {
            queue: vec![ (known, 0, (None, 0, 0)) ],
            line:  line
        }
    }

    fn union( &mut self ) -> Option<Line> {
        use Block::*;

        let mut item = match self.next() {
            Some( i ) => i,
            None => return None
        };

        for it in self {
            for (i, &e) in it.iter().enumerate() {
                if item[ i ] != e { item[ i ] = UNKNOWN; }
            }
        }

        Some( item )
    }
}

impl<'a> Iterator for LineIterator<'a> {
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

fn write_output( out: Vec<Vec<Block>> ) -> Result<()> {
    let mut file = File::create( "zad_output.txt" )?;

    for line in out.iter() {
        let text = line.iter()
                       .map( |&b| if b == Block::FILL { '#' } else { '.' } )
                       .collect::<String>();
        write!( file, "{}\n", text )?;
    }

    Ok( () )
}

fn application() -> Result<()> {
    use Block::*;

    let (vert_in, horiz_in) = read_input()?;
    let mut changed = true;

    let mut vertical = vec![ vec![ UNKNOWN; horiz_in.len() ]; vert_in.len() ];
    let mut horizontal = vec![ vec![ UNKNOWN; vert_in.len() ]; horiz_in.len() ];

    while changed {
        changed = false;

        for (i, vert) in vert_in.iter().enumerate() {
            let new = LineIterator::new( vert, vertical[ i ].clone() ).union();
            if let Some( new ) = new {
                if new != vertical[ i ] {
                    vertical[ i ] = new;
                    for (j, other) in horizontal.iter_mut().enumerate() {
                        other[ i ] = vertical[ i ][ j ];
                    }

                    changed = true;
                }
            }
        }

        for (i, horiz) in horiz_in.iter().enumerate() {
            let new = LineIterator::new( horiz, horizontal[ i ].clone() ).union();
            if let Some( new ) = new {
                if new != horizontal[ i ] {
                    horizontal[ i ] = new;
                    for (j, other) in vertical.iter_mut().enumerate() {
                        other[ i ] = horizontal[ i ][ j ];
                    }

                    changed = true;
                }
            }

        }
    }

    println!( "Unknown: {}", horizontal.iter().flat_map( |l| l.iter() ).filter( |&&v| v == UNKNOWN ).count() );

    write_output( horizontal )?;

    Ok( () )
}

fn main() {
    if let Err( e ) = application() {
        println!( "[Error] {}", e );
    }
}
