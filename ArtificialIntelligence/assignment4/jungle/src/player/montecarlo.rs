use std::collections::HashMap;

use player::Player;
use player::random::RandomPlayer;
use game::Game;
use board::{Board, Move};

pub struct MontecarloPlayer;

impl MontecarloPlayer {
    fn play_random_game( board: &Board, player_no: usize, first_move: Move ) -> Option<(usize, usize)> {
        let mut new_board = board.clone();
        new_board.do_move( first_move, player_no );

        let mut game = Game::new( RandomPlayer {}, RandomPlayer {}, player_no );
        game.set_board( new_board );
        game.set_current_player( 1 - player_no );

        let mut counter = 0;
        while ! game.game_ended() {
            counter += 1;
            game.next_move();
        }

        Some( (counter, game.winner()) )
    }
}


impl Player for MontecarloPlayer {
    fn do_move( &mut self, board: & Board, player_no: usize ) -> Option<Move> {
        const N: usize = 20_000;

        let mut counter = 0;
        let mut cache = HashMap::new();

        let mut iter = 0;
        let moves = board.valid_moves( player_no );

        if moves.len() == 0 {
            return None;
        }

        while counter < N {
            if let Some( (steps, winner) ) = Self::play_random_game( board, player_no, moves[ iter ].clone() ) {
                counter += steps;
                *cache.entry( moves[ iter ].clone() )
                      .or_insert( 0 ) += if winner == player_no { 1 } else { -1 };
            }

            iter = (iter + 1) % moves.len();
        }

        cache.iter().max_by( |a, b| a.1.cmp( b.1 ) ).map( |e| e.0 ).cloned()
    }
}
