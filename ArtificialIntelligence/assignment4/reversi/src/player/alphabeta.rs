use std::f32;

use board::{Board, Color};
use player::Player;

pub struct AlphaBetaPlayer;

impl AlphaBetaPlayer {
    const WEIGHTS: [f32; 64] = [
        1.2, 0.7, 1.1, 1.1, 1.1, 1.1, 0.7, 1.2,
        0.7, 0.7, 1.0, 1.0, 1.0, 1.0, 0.7, 0.7,
        1.1, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.1,
        1.1, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.1,
        1.1, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.1,
        1.1, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.1,
        0.7, 0.7, 1.0, 1.0, 1.0, 1.0, 0.7, 0.7,
        1.2, 0.7, 1.1, 1.1, 1.1, 1.1, 0.7, 1.2,
    ];

    const MAX_DEPTH: usize = 3;

    fn max_value( mut board: Board, mut alpha: f32, beta: f32, depth: usize ) -> f32 {
        if depth >= Self::MAX_DEPTH { return board.get_score( Color::BLACK ) as f32; }

        let mut value = -f32::INFINITY;
        for next_move in board.valid_moves( Color::BLACK ) {
            let mut new_board = board.clone();
            new_board.do_move( next_move.0, next_move.1, Color::BLACK );

            value = match (value, Self::min_value( new_board, alpha, beta, depth + 1 )) {
                (a, b) if a > b => a,
                (_, b) => b
            };

            if value >= beta {
                return value;
            }

            alpha = if alpha > value { alpha }
                    else { value };
        }

        value
    }

    fn min_value( mut board: Board, alpha: f32, mut beta: f32, depth: usize ) -> f32 {
        if depth >= Self::MAX_DEPTH { return board.get_score( Color::WHITE ) as f32; }

        let mut value = f32::INFINITY;
        for next_move in board.valid_moves( Color::WHITE ) {
            let mut new_board = board.clone();
            new_board.do_move( next_move.0, next_move.1, Color::WHITE );

            value = match (value, Self::max_value( new_board, alpha, beta, depth + 1 )) {
                (a, b) if a < b => a,
                (_, b) => b
            };

            if value <= alpha {
                return value;
            }

            beta = if beta < value { beta }
                   else { value };
        }

        value
    }
}

impl Player for AlphaBetaPlayer {
    fn do_move( &mut self, board: &Board, color: Color ) -> Option<(usize, usize)> {
        board.valid_moves( color )
             .into_iter()
             .map( |m| {
                let mut new_board = board.clone();
                new_board.do_move( m.0, m.1, color );

                (m, Self::min_value( new_board, -f32::INFINITY, f32::INFINITY, 0 ))// * Self::WEIGHTS[ m.1 * 8 + m.0 ])
             } )
             .fold( None::<((usize, usize), f32)>, |a, b| {
                 if let Some( a ) = a {
                    if a.1 > b.1 { Some( a ) }
                    else { Some( b ) }
                 } else { Some( b ) }
             } )
             .map( |e| e.0 )
    }
}
