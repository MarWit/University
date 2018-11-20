use board::{Board, Move, Position};
use player::Player;

pub struct RunnerPlayer;
impl Player for RunnerPlayer {
    fn do_move( &mut self, board: &Board, player_no: usize ) -> Option<Move> {
        fn distance( a: & Position, b: & Position ) -> i32 {
            ((b.0 - a.0) + (b.1 - a.1)).abs()
        }

        let enemy_base = board.get_goal_position( 1 - player_no );
        let moves = board.valid_moves( player_no );

        moves.into_iter()
             .min_by( |a, b| distance( & a.1, & enemy_base ).cmp( & distance( & b.1, & enemy_base ) ) )
    }
}


