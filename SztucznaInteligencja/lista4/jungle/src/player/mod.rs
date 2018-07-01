pub mod random;
pub mod montecarlo;
pub mod runner;

use board::{Board, Move};

pub trait Player {
    fn do_move( &mut self, board: & Board, player_no: usize ) -> Option<Move>;
}
