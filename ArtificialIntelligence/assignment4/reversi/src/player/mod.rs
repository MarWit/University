pub mod random;
pub mod montecarlo;
pub mod alphabeta;
pub mod mcts;
pub mod bayes;
pub mod negascout;
#[cfg(feature = "lua")] pub mod lua;

use board::{Board, Color};

pub trait Player {
    fn do_move( &mut self, board: &Board, color: Color ) -> Option<(usize, usize)>;
}
