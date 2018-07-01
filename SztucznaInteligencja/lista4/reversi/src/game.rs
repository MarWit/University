use board::{Board, Color};
use player::Player;

use piston_window::*;

pub struct Game<B, W>
    where B: Player,
          W: Player
{
    board: Board,
    black: B,
    white: W,
    now: Color,
    passed: usize
}

impl<B, W> Game<B, W>
    where B: Player,
          W: Player
{
    pub fn new( black: B, white: W ) -> Self {
        Game {
            board: Board::default(),
            black,
            white,
            now: Color::BLACK,
            passed: 0
        }
    }

    pub fn draw<G: Graphics>( &self, c: & Context, g: &mut G ) {
        self.board.draw( c, g );
    }

    pub fn draw_text( &self ) {
        println!( "Now: {}", self.now );
        self.board.draw_text();
    }

    pub fn game_ended( &self ) -> bool {
        self.passed >= 2
    }

    pub fn reset( &mut self ) {
        // self.black.reset();
        // self.white.reset();

        self.board = Board::default();
        self.now = Color::BLACK;
        self.passed = 0;
    }

    pub fn next_move( &mut self ) -> Option<(usize, usize)> {
        if self.passed >= 2 { return None; }

        let new_move = if self.now == Color::BLACK {
            self.black.do_move( &self.board, Color::BLACK )
        } else {
            self.white.do_move( &self.board, Color::WHITE )
        };

        self.passed += 1;

        if let Some( (x, y) ) = new_move {
            self.board.do_move( x, y, self.now );
            self.passed = 0;
        }

        self.now = self.now.invert();
        new_move
    }

    pub fn set_current_player( &mut self, color: Color ) {
        self.now = color;
    }

    pub fn set_board( &mut self, board: Board ) {
        self.board = board;
    }

    pub fn score( &mut self ) -> (usize, usize) {
        let a_score = self.board.get_score( self::Color::BLACK );
        let b_score = self.board.get_score( self::Color::WHITE );

        (a_score, b_score)
    }
}
