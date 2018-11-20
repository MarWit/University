extern crate rand;

use std::error::Error;
use std::result;
use rand::Rng;

type Result<T> = result::Result<T, Box<Error>>;

mod game;
mod board;
mod player;

fn play_one_game() -> usize {
    let mut game = game::Game::new( player::runner::RunnerPlayer {}, player::montecarlo::MontecarloPlayer {}, rand::thread_rng().gen_range( 0, 1 ) );

    while ! game.game_ended() {
        game.next_move();
    }

    game.winner()
}

fn application() -> Result<()> {
    // let mut game = game::Game::new( player::runner::RunnerPlayer {}, player::montecarlo::MontecarloPlayer {} );
    // game.draw();
    //
    // while ! game.game_ended() {
    //     game.next_move();
    //     game.draw();
    // }
    //
    // println!( "Player #{} won.", game.winner() );

    const N: usize = 10;
    let (mut a, mut b) = (0, 0);
    
    for i in 0..N {
        println!( "{}", i );
    
        if play_one_game() == 0 { a += 1; }
        else { b += 1; }
    }
    
    println!( "A won {} games", a );
    println!( "B won {} games", b );

    Ok( () )
}

fn main() {
    if let Err( e ) = application() {
        println!( "[Error] {}", e );
    }
}
