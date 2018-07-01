use std::collections::BTreeMap;

use player::Player;
use player::random::RandomPlayer;
use board::{Board, Color};
use game::Game;

pub struct MontecarloPlayer;

impl MontecarloPlayer {
    fn play_random_game( board: & Board, color: Color ) -> Option<(i32, (usize, usize))> {
        let mut game = Game::new( RandomPlayer {}, RandomPlayer {} );

        game.set_board( board.clone() );
        game.set_current_player( color );

        let first_move = match game.next_move() {
            Some( e ) => e,
            None => return None

        };

        let mut pass = 0;
        while pass < 2 {
            if game.next_move().is_some() {
                pass = 0;
            } else {
                pass += 1;
            }
        }

        let (score_a, score_b) = game.score();
        let score = if score_a > score_b { 10 }
                    else if score_b > score_a { -10 }
                    else { -5 };

        Some( (score, first_move) )
    }
}

impl Player for MontecarloPlayer {
    fn do_move( &mut self, board: & Board, color: Color ) -> Option<(usize, usize)> {
        const SAMPLES: usize = 100;

        let mut cache = BTreeMap::new();

        for _ in 0..SAMPLES {
            if let Some( (score, first_move) ) = Self::play_random_game( board, color ) {
                let e = cache.entry( first_move ).or_insert( (0, 0) );
                e.0 += score; e.1 += 1;
            }
        }

        cache.iter()
             .map( |(&k, v)| (k, v.0 as f32 / v.1 as f32) )
             .fold( None::<((usize, usize), f32)>, |a, b| {
                 if let Some( a ) = a {
                    if a.1 > b.1 { Some( a ) }
                    else { Some( b ) }
                 } else { Some( b ) }
             } )
             .map( |e| e.0 )
    }
}
