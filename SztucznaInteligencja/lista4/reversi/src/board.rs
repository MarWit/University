#![warn(clippy)]

use std::default::Default;
use std::slice::Iter;
use std::fmt;

use piston_window::*;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Color {
    NONE,
    BLACK,
    WHITE
}

#[derive(Copy, Clone)]
pub enum Direction {
    U, D, L, R,
    UL, UR, DL, DR
}

impl Color {
    pub fn invert( &self ) -> Self {
        use self::Color::*;

        match self {
            BLACK => WHITE,
            WHITE => BLACK,
            NONE  => NONE
        }
    }
}

impl fmt::Display for Color {
    fn fmt( &self, f: &mut fmt::Formatter ) -> fmt::Result {
        use self::Color::*;

        match self {
            BLACK => write!( f, "Black" ),
            WHITE => write!( f, "White" ),
            NONE  => write!( f, "None" )
        }
    }
}

impl Direction {
    pub fn moves() -> Iter<'static, Self> {
        use self::Direction::*;
        const MOVES: [Direction; 8] = [U, D, L, R, UL, UR, DL, DR];

        MOVES.into_iter()
    }

    pub fn offset( &self ) -> (i32, i32) {
        use self::Direction::*;

        match self {
            U  => (0,  -1),
            D  => (0,   1),
            L  => (-1,  0),
            R  => (1,   0),
            UL => (-1, -1),
            UR => (1,  -1),
            DL => (-1,  1),
            DR => (1,   1)
        }
    }
}

#[derive(Copy, Clone)]
pub struct Board {
    occupied: usize,
    color: usize,
    score: Option<(usize, usize)>
}

impl Default for Board {
    fn default() -> Self {
        use self::Color::*;

        let mut board = Board {
            occupied: 0,
            color: 0,
            score: None
        };

        board.set( 3, 3, BLACK );
        board.set( 4, 4, BLACK );
        board.set( 3, 4, WHITE );
        board.set( 4, 3, WHITE );

        board
    }
}

impl Board {
    pub fn get( &self, x: usize, y: usize ) -> Option<self::Color> {
        use self::Color::*;

        if x >= 8 || y >= 8 {
            return None;
        }

        let mask = 1 << (x + 8 * y);
        if (self.occupied & mask) != 0 {
            if (self.color & mask) != 0 {
                Some( BLACK )
            } else {
                Some( WHITE )
            }
        } else {
            Some( NONE )
        }
    }

    pub fn set( &mut self, x: usize, y: usize, color: self::Color ) {
        use self::Color::*;

        if x >= 8 || y >= 8 {
            return;
        }

        let mask = 1 << (x + 8 * y);

        match color {
            NONE => {
                self.occupied &= !mask;
                self.color &= !mask;
            },
            BLACK => {
                self.occupied |= mask;
                self.color |= mask;

            },
            WHITE => {
                self.occupied |= mask;
                self.color &= !mask;
            }
        }
    }

    pub fn draw<G: Graphics>( &self, c: & Context, g: &mut G ) {
        use self::Color::*;

        const BORDER_COLOR: [f32; 4]    = [0.0, 0.0, 0.0, 1.0];
        const BORDER_WIDTH: f64         = 3.0;
        const BORDER_GAP: f64           = 25.0;

        if let Some( viewport ) = c.viewport {
            let (width, height) = (viewport.window_size[ 0 ] as f64, viewport.window_size[ 1 ] as f64);

            rectangle(
                [0.3, 0.3, 0.3, 1.0],
                [ BORDER_GAP, BORDER_GAP, width - 2.0 * BORDER_GAP, height - 2.0 * BORDER_GAP ],
                c.transform,
                g
            );

            let x_spacing = ( width - 2.0 * BORDER_GAP ) / 8.0 - BORDER_WIDTH;
            let y_spacing = ( height - 2.0 * BORDER_GAP ) / 8.0 - BORDER_WIDTH;

            for i in 0..9 {
                let x = BORDER_GAP + (x_spacing + BORDER_WIDTH) * i as f64;
                let y = BORDER_GAP + (y_spacing + BORDER_WIDTH) * i as f64;

                line(
                    BORDER_COLOR, BORDER_WIDTH,
                    [ BORDER_GAP - BORDER_WIDTH, y, width - BORDER_GAP + BORDER_WIDTH, y ],
                    c.transform, g
                );

                line(
                    BORDER_COLOR, BORDER_WIDTH,
                    [ x, BORDER_GAP - BORDER_WIDTH, x, height - BORDER_GAP + BORDER_WIDTH ],
                    c.transform, g
                );
            }

            for y in 0..8 {
                let y_pos = BORDER_WIDTH + BORDER_GAP + (y_spacing + BORDER_WIDTH) * y as f64;

                for x in 0..8 {
                    let x_pos = BORDER_WIDTH + BORDER_GAP + (x_spacing + BORDER_WIDTH) * x as f64;

                    match self.get( x, y ).unwrap() {
                        NONE => {},
                        BLACK => ellipse(
                            [0.0, 0.0, 0.0, 1.0],
                            [ x_pos + 2.0, y_pos + 2.0, x_spacing - 4.0 - BORDER_WIDTH, y_spacing - 4.0 - BORDER_WIDTH ],
                            c.transform, g
                        ),
                        WHITE => ellipse(
                            [1.0, 1.0, 1.0, 1.0],
                            [ x_pos + 2.0, y_pos + 2.0, x_spacing - 4.0 - BORDER_WIDTH, y_spacing - 4.0 - BORDER_WIDTH ],
                            c.transform, g
                        ),
                    }
                }
            }
        }
    }

    pub fn draw_text( &self ) {
        use self::Color::*;

        println!( "  A B C D E F G H" );

        for i in 0..8 {
            print!( "{} ", i );

            for j in 0..8 {
                match self.get( j, i ).unwrap() {
                    NONE  => print!( ". " ),
                    BLACK => print!( "* " ),
                    WHITE => print!( "O " )
                }
            }

            println!( "{}", i );
        }

        println!( "  A B C D E F G H\n" );
    }

    fn can_beat( &self, x: usize, y: usize, dir: self::Direction, color: self::Color ) -> bool {
        if self.get( x, y ) != Some( self::Color::NONE ) {
            return false;
        }

        let (dx, dy) = dir.offset();

        let mut x = x as i32 + dx;
        let mut y = y as i32 + dy;
        let mut cnt = 0;

        while let Some( field ) = self.get( x as usize, y as usize ) {
            if field == Color::NONE { return false; }
            if field == color { break; }
            if field == color.invert() {
                cnt += 1;
            }

            x += dx; y += dy;
        }

        cnt > 0 && self.get( x as usize, y as usize ) == Some( color )
    }

    pub fn valid_moves( &self, color: self::Color ) -> Vec<(usize, usize)> {
        let mut moves = vec![];

        for y in 0..8 {
            for x in 0..8 {
                if Direction::moves().map( |&d| self.can_beat( x, y, d, color ) ).any( |m| m ) {
                    moves.push( (x, y) );
                }
            }
        }

        moves
    }

    pub fn do_move( &mut self, x: usize, y: usize, color: self::Color ) {
        use self::Color::*;

        for dir in Direction::moves() {
            let (dx, dy) = dir.offset();
            let (mut x, mut y) = (x as i32 + dx, y as i32 + dy);
            let mut to_flip = vec![];

            while let Some( field ) = self.get( x as usize, y as usize ) {
                if field == color { break; }
                if field != color && field != NONE {
                    to_flip.push( (x as usize, y as usize) );
                }

                x += dx; y += dy;
            }

            if self.get( x as usize, y as usize ) != Some( color ) {
                continue;
            }

            for (ox, oy) in to_flip {
                self.set( ox, oy, color );
            }
        }

        self.set( x, y, color );
    }

    pub fn get_score( &mut self, color: Color ) -> usize {
        use self::Color::*;

        if self.score.is_none() {
            let (mut black, mut white) = (0, 0);

            for y in 0..8 {
                for x in 0..8 {
                    match self.get( x, y ).unwrap() {
                        WHITE   => white += 1,
                        BLACK   => black += 1,
                        _       => {}
                    }
                }
            }

            self.score = Some( (black, white) );
        }

        let score = self.score.unwrap();
        match color {
            BLACK => score.0,
            WHITE => score.1,
            NONE  => 64 - (score.0 + score.1)
        }
    }
}
