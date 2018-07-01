use board::*;
use player::*;

pub struct Game<A, B>
    where A: Player,
          B: Player
{
    board: Board,
    player_a: A,
    player_b: B,
    stalled: usize,
    now: usize,
    starting: usize
}

impl<A,B> Game<A, B>
    where A: Player,
          B: Player
{
    pub fn new( player_a: A, player_b: B, starting: usize ) -> Self {
        Game {
            board: Board::default(),
            player_a,
            player_b,
            stalled: 0,
            now: starting,
            starting
        }
    }

    pub fn draw( &self ) {
        self.board.draw();
    }

    pub fn set_board( &mut self, board: Board ) {
        self.board = board;
    }

    pub fn set_current_player( &mut self, now: usize ) {
        self.now = now;
    }

    pub fn get_starting_player( & self ) -> usize {
        self.starting
    }

    pub fn game_ended( &self ) -> bool {
        self.stalled >= 50 || self.board.game_ended()
    }

    pub fn winner( &self ) -> usize {
        self.board.winner().unwrap_or( 1 - self.starting )
    }

    pub fn next_move( &mut self ) -> Option<Move> {
        let new_move = if self.now == 0 {
            self.player_a.do_move( & self.board, self.now )
        } else {
            self.player_b.do_move( & self.board, self.now )
        };

        self.stalled += 1;

        if let Some( new_move ) = new_move.clone() {
            if self.board.do_move( new_move, self.now ) {
                self.stalled = 0;
            }

        }

        self.now ^= 1;

        new_move
    }
}
