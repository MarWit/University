use player::Player;
use board::{Board, Move};

use rand::{self, Rng};

pub struct RandomPlayer;

impl Player for RandomPlayer {
    fn do_move( &mut self, board: & Board, player_no: usize ) -> Option<Move> {
        let moves = board.valid_moves( player_no );
        rand::thread_rng().choose( &moves ).cloned()
    }
}
