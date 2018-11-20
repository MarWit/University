use player::Player;
use board::{Board, Color};

use rand::{self, Rng};

pub struct RandomPlayer;

impl Player for RandomPlayer {
    fn do_move( &mut self, board: & Board, color: Color ) -> Option<(usize, usize)> {
        let moves = board.valid_moves( color );
        rand::thread_rng().choose( &moves ).cloned()
    }
}
