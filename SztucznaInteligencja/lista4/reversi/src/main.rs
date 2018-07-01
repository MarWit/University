#![warn(clippy)]
#![feature(non_ascii_idents)]

extern crate piston_window;
extern crate rand;
extern crate rayon;
extern crate indextree;
extern crate rusty_machine;
#[cfg(feature = "lua")] extern crate rlua;

mod board;
mod player;
mod game;

use std::env;
use std::error::Error;
use std::result;

use piston_window::*;
use rayon::prelude::*;

type Result<T> = result::Result<T, Box<Error>>;

fn interactive() -> Result<()> {
    let mut window : PistonWindow = PistonWindow::new(
        OpenGL::V3_2,
        0,
        WindowSettings::new( "Reversi", [600, 600] )
            .srgb( false )
            .exit_on_esc( true )
            .build()?
    );

    // let player_a = player::random::RandomPlayer {};
    // let player_a = player::montecarlo::MontecarloPlayer {};
    // let player_a = player::lua::LuaPlayer::new( "random.lua" )?;
    // let player_a = player::alphabeta::AlphaBetaPlayer {};
    let player_a = player::negascout::NegascoutPlayer {};
    let player_b = player::random::RandomPlayer {};

    let mut game = game::Game::new( player_a, player_b );

    while let Some( event ) = window.next() {
        window.draw_2d( & event, |c, g| {
            clear( [1.0, 1.0, 1.0, 1.0], g );
            game.draw( &c, g );
        } );

        if event.press_args().is_some() {
            if game.game_ended() {
                game.reset();
            } else {
                game.next_move();
            }

            game.draw_text();
        }
    }

    Ok( () )
}

fn tester() -> Result<()> {
    const TESTS_NUM: usize = 100;
    println!( "Tests num: {}", TESTS_NUM );

    let mut a_lost = (0..TESTS_NUM / 2).into_par_iter()
                          .map( |i| {
                                // let player_a = player::mcts::MCTSPlayer {};
                                let player_a = player::negascout::NegascoutPlayer {};
                                // let player_a = player::bayes::BayesPlayer::new();
                                let player_b = player::random::RandomPlayer {};

                                let mut game = game::Game::new( player_a, player_b );
                                let mut pass = 0;

                                while pass < 2 {
                                    if game.next_move().is_some() {
                                        pass = 0;
                                    } else {
                                        pass += 1;
                                    }
                                }

                                let (a_score, b_score) = game.score();
                                if a_score < b_score { 1 } else { 0 }
                          } )
                          .sum::<usize>();

    println!( "Black lost {} times", a_lost );

    a_lost += (0..TESTS_NUM / 2).into_par_iter()
                          .map( |i| {
                                let player_a = player::random::RandomPlayer {};
                                // let player_b = player::mcts::MCTSPlayer {};
                                let player_b = player::negascout::NegascoutPlayer {};

                                let mut game = game::Game::new( player_a, player_b );
                                let mut pass = 0;

                                while pass < 2 {
                                    if game.next_move().is_some() {
                                        pass = 0;
                                    } else {
                                        pass += 1;
                                    }
                                }

                                let (a_score, b_score) = game.score();
                                if a_score > b_score { 1 } else { 0 }
                          } )
                          .sum::<usize>();

    println!( "Black lost {} times", a_lost );

    Ok( () )
}

fn application() -> Result<()> {
    let mut args = env::args();
    let mut is_interactive = false;

    if let Some( word ) = args.nth( 1 ) {
        if word == "--interactive" {
            is_interactive = true;
        }
    }

    if is_interactive {
        interactive()
    } else {
        tester()
    }
}

fn main() {
    if let Err( e ) = application() {
        println!( "[Error] {}", e );
    }
}
