use std::f32;

use board::{Board, Color};
use player::Player;

pub struct NegascoutPlayer;

impl NegascoutPlayer {
    const MAX_DEPTH: usize = 7;

    fn custom_score( board: &Board, player: Color ) -> f32 {
        const WEIGHTS: [[f32; 8]; 8] = [
            [ 50. , -1.  , 5. , 2. , 2. , 5. , -1.  , 50. ] ,
            [ -1. , -10. , 1. , 1. , 1. , 1. , -10. , -1. ] ,
            [ 5.  , 1.   , 1. , 1. , 1. , 1. , 1.   , 5. ]  ,
            [ 2.  , 1.   , 1. , 0. , 0. , 1. , 1.   , 2. ]  ,
            [ 2.  , 1.   , 1. , 0. , 0. , 1. , 1.   , 2. ]  ,
            [ 5.  , 1.   , 1. , 1. , 1. , 1. , 1.   , 5. ]  ,
            [ -1. , -10. , 1. , 1. , 1. , 1. , -10. , -1. ] ,
            [ 50. , -1.  , 5. , 2. , 2. , 5. , -1.  , 50. ]
        ];

        let mut score = 0.0;
        for j in 0..8 {
            for i in 0..8 {
                if board.get( i, j ).unwrap() == player {
                    score += WEIGHTS[ j ][ i ];
                }
            }
        }

        score
    }

    pub fn pvs( board: Board, depth: usize, mut α: f32, β: f32, now: f32, player: Color ) -> f32 {
        if depth >= Self::MAX_DEPTH { return now * Self::custom_score( &board, player ); }

        let mut value;
        let mut first = true;

        let player_now = if now > 0.0 { player } else { player.invert() };
        let mut boards = board.valid_moves( player_now )
                            .into_iter()
                            .map( |m| {
                                let mut board = board.clone();
                                board.do_move( m.0, m.1, player_now );
                                (board, board.get_score( player ))
                            } )
                            .collect::<Vec<_>>();

        boards.sort_by( |&a, &b| (b.1).cmp( &a.1 ) );

        for (new_board, _) in boards {
            if first {
                first = false;
                value = -Self::pvs( new_board, depth + 1, -β, -α, -now, player );
            } else {
                value = -Self::pvs( new_board, depth + 1, -α-1., -α, -now, player );

                if α < value && β > value {
                    value = -Self::pvs( new_board, depth + 1, -β, -value, -now, player );
                }
            }

            α = if α > value { α } else { value };
            if α >= β {
                break;
            }
        }

        α
    }
}

impl Player for NegascoutPlayer {
    fn do_move( &mut self, board: &Board, color: Color ) -> Option<(usize, usize)> {
        board.valid_moves( color )
             .into_iter()
             .map( |m| {
                let mut new_board = board.clone();
                new_board.do_move( m.0, m.1, color );

                (m, Self::pvs( new_board, 0, -f32::INFINITY, f32::INFINITY, 1.0, color ))
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
