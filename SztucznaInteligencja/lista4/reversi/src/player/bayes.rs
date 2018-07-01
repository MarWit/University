use std::f32;
use std::path::Path;
use std::fs::File;
use std::io::{BufRead, BufReader};

use board::{Board, Color};
use player::Player;

use rusty_machine::learning::naive_bayes::{self, NaiveBayes};
use rusty_machine::linalg::{Matrix, BaseMatrix};
use rusty_machine::learning::SupModel;

type Single = Vec<f64>;
type TrainData = (Vec<Single>, Vec<Single>);

pub struct BayesPlayer {
    model: NaiveBayes<naive_bayes::Gaussian>
}

fn load_data<P: AsRef<Path>>( path: P ) -> TrainData {
    let file = File::open( path ).expect( "file doesnt exists" );
    let reader = BufReader::new( file );

    let mut questions = vec![];
    let mut answers = vec![];

    for line in reader.lines() {
        let line = line.unwrap();
        let mut splitted = line.split( " " );

        let who = splitted
                    .next()
                    .map( |l| l.parse::<i32>().unwrap() )
                    .map( |v| if v == 1 { vec![ 1f64, 0f64 ] } else { vec![ 0f64, 1f64 ] } )
                    .unwrap();

        let state = {
            let mut board = vec![];
            let state = splitted.next().unwrap();

            for c in state.chars() {
                if c == '1' { board.push( 1f64 ); }
                else { board.push( 0f64 ); }

                if c == '0' { board.push( 1f64 ); }
                else { board.push( 0f64 ); }

                if c == '_' { board.push( 1f64 ); }
                else { board.push( 0f64 ); }
            }

            board
        };

        questions.push( state );
        answers.push( who );
    }

    (questions, answers)
}

impl BayesPlayer {
    pub fn new() -> Self {
        const FILE: &'static str = "/home/marwit/Downloads/reversi_learning_data/bigger.dat";

        let (input, output) = load_data( FILE );
        let input_matrix : Matrix<f64> = input.iter().map( |v| &v[ .. ] ).collect();
        let output_matrix : Matrix<f64> = output.iter().map( |v| &v[ .. ] ).collect();

        let mut model = NaiveBayes::<naive_bayes::Gaussian>::new();
        model.train( &input_matrix, &output_matrix ).unwrap();

        BayesPlayer {
            model
        }
    }
}

impl Player for BayesPlayer {
    fn do_move( &mut self, board: &Board, color: Color ) -> Option<(usize, usize)> {
        board.valid_moves( color )
             .into_iter()
             .map( |m| {
                let mut new_board = board.clone();
                new_board.do_move( m.0, m.1, color );

                let input = {
                    let mut out = vec![];

                    for y in (0..8) {
                        for x in (0..8) {
                            let col = board.get( x, y ).unwrap();

                            if col == Color::BLACK { out.push( 1f64 ); }
                            else { out.push( 0f64 ); }

                            if col == Color::WHITE { out.push( 1f64 ); }
                            else { out.push( 0f64 ); }

                            if col == Color::NONE { out.push( 1f64 ); }
                            else { out.push( 0f64 ); }
                        }
                    }

                    out
                };

                let input_matrix = Matrix::new(1, 64 * 3, input);
                let predicted = self.model.predict( &input_matrix ).unwrap();
                let slice = predicted.data();

                (m, 50. * slice[ 0 ] - 50. * slice[ 1 ])
             } )
             .fold( None::<((usize, usize), f64)>, |a, b| {
                 if let Some( a ) = a {
                    if a.1 > b.1 { Some( a ) }
                    else { Some( b ) }
                 } else { Some( b ) }
             } )
             .map( |e| e.0 )
    }
}
