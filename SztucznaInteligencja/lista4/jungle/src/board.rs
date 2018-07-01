use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::slice::Iter;
use std::default::Default;

pub type Position = (i32, i32);
#[derive(Clone, Hash, Eq, PartialEq)]
pub struct Move(pub Position, pub Position);

#[derive(Clone, Eq, PartialEq)]
pub enum Direction {
    UP, DOWN, LEFT, RIGHT
}

impl Direction {
    pub fn moves() -> Iter<'static, Self> {
        use self::Direction::*;
        const DIRECTIONS: [Direction; 4] = [ UP, DOWN, LEFT, RIGHT ];

        DIRECTIONS.into_iter()
    }

    pub fn offset( &self ) -> Position {
        use self::Direction::*;

        match self {
            UP    => (0, -1),
            DOWN  => (0, 1),
            LEFT  => (-1, 0),
            RIGHT => (1, 0)
        }
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Debug)]
pub enum Animal {
    Rat,     Cat,   Dog,  Wolf,
    Panther, Tiger, Lion, Elephant
}

#[derive(PartialEq)]
pub enum Surface {
    Ground, Water, Trap(usize), Base(usize)
}

impl Animal {
    pub fn can_cross_water( &self ) -> bool {
        use self::Animal::*;

        match self {
            Rat | Tiger | Lion => true,
            _ => false
        }
    }

    pub fn can_beat( &self, another: & Self ) -> bool {
        use self::Animal::*;

        if *self == Elephant && *another == Rat {
            false
        } else if *self == Rat && *another == Elephant {
            true
        } else {
            *self >= *another
        }
    }

    pub fn as_char( &self ) -> char {
        use self::Animal::*;

        match self {
            Rat      => 'r',
            Cat      => 'c',
            Dog      => 'd',
            Wolf     => 'w',
            Panther  => 'j',
            Tiger    => 't',
            Lion     => 'l',
            Elephant => 'e'
        }
    }
}

#[derive(Clone)]
pub struct Board {
    player_a: HashMap<Position, Animal>,
    player_b: HashMap<Position, Animal>
}

impl Default for Board {
    fn default() -> Self {
        use self::Animal::*;

        let mut player_a = HashMap::new();
        let mut player_b = HashMap::new();

        player_a.insert( (0, 0), Lion );
        player_a.insert( (6, 0), Tiger );
        player_a.insert( (1, 1), Dog );
        player_a.insert( (5, 1), Cat );
        player_a.insert( (0, 2), Rat );
        player_a.insert( (2, 2), Panther );
        player_a.insert( (4, 2), Wolf );
        player_a.insert( (6, 2), Elephant );

        player_b.insert( (0, 8), Tiger );
        player_b.insert( (6, 8), Lion );
        player_b.insert( (1, 7), Cat );
        player_b.insert( (5, 7), Dog );
        player_b.insert( (0, 6), Elephant );
        player_b.insert( (2, 6), Wolf );
        player_b.insert( (4, 6), Panther );
        player_b.insert( (6, 6), Rat );


        Board {
            player_a,
            player_b
        }
    }
}

impl Board {
    const MAP: [char; 7 * 9] = [
        '.', '.', '#', '*', '#', '.', '.',
        '.', '.', '.', '#', '.', '.', '.',
        '.', '.', '.', '.', '.', '.', '.',
        '.', '~', '~', '.', '~', '~', '.',
        '.', '~', '~', '.', '~', '~', '.',
        '.', '~', '~', '.', '~', '~', '.',
        '.', '.', '.', '.', '.', '.', '.',
        '.', '.', '.', '#', '.', '.', '.',
        '.', '.', '#', '*', '#', '.', '.'
    ];

    fn get_surface( pos: & Position ) -> Surface {
        use self::Surface::*;

        match Self::MAP[ (pos.1 * 7 + pos.0) as usize] {
            '#' => Trap( if pos.1 >= 4 { 1 } else { 0 } ),
            '~' => Water,
            '*' => Base( if pos.1 > 0 { 1 } else { 0 } ),
            _   => Ground
        }
    }

    pub fn get_goal_position( &self, player_no: usize ) -> Position {
        if player_no == 0 { (3, 0) } else { (3, 8) }
    }

    pub fn draw( &self ) {
        for y in 0..9 {
            for x in 0..7 {
                let mut c = self.player_a
                                .get( &(x as i32, y as i32) )
                                .map( |a| a.as_char().to_uppercase().next().unwrap() );
                c = c.or( self.player_b
                              .get( &(x as i32, y as i32) )
                              .map( |a| a.as_char().to_lowercase().next().unwrap() ) );

                print!( "{}", c.unwrap_or( Self::MAP[ y * 7 + x ] ) );
            }

            println!( "" );
        }

        println!( "" );
    }

    fn check_move( &self, animal: & Animal, old_pos: & Position, new_pos: &mut Position, player_no: usize ) -> bool {
        use self::Surface::*;

        if new_pos.0 < 0 || new_pos.0 >= 7 ||
           new_pos.1 < 0 || new_pos.1 >= 9 { return false; }

        if Self::get_surface( new_pos ) == Water && ! animal.can_cross_water() {
            return false;
        }

        if Self::get_surface( new_pos ) == Base(player_no) {
            return false;
        }

        let (player, opponent) = {
            if player_no == 0 { (&self.player_a, &self.player_b) }
            else { (&self.player_b, &self.player_a) }
        };

        if player.contains_key( new_pos ) {
            return false;
        }

        if let Some( another_animal ) = opponent.get( new_pos ) {
            if Self::get_surface( new_pos ) != Trap( player_no ) &&
               ! animal.can_beat( another_animal )
            {
                return false;
            }

            if Self::get_surface( old_pos ) == Water &&
               Self::get_surface( new_pos ) != Water
            {
                return false;
            }
        }

        if Self::get_surface( old_pos ) != Water &&
           Self::get_surface( old_pos ) == Water &&
           *animal != Animal::Rat
        {
            let offset = (new_pos.0 - old_pos.0, new_pos.1 - old_pos.1);
            while Self::get_surface( new_pos ) == Water {
                if opponent.get( new_pos ).is_some() {
                    return false;
                }

                new_pos.0 += offset.0;
                new_pos.1 += offset.1;
            }

            if let Some( other_animal ) = opponent.get( new_pos ) {
                if ! animal.can_beat( other_animal ) {
                    return false;
                }
            }
        }

        true
    }

    pub fn valid_moves( &self, player_no: usize ) -> Vec<Move> {
        let player = if player_no == 0 { &self.player_a } else { &self.player_b };

        let mut moves = vec![];

        for (pos, pawn) in player.iter() {
            let (x, y) = pos;

            for dir in Direction::moves() {

                let (dx, dy) = dir.offset();
                let mut new_pos = (x + dx, y + dy);

                if self.check_move( pawn, pos, &mut new_pos, player_no ) {
                    moves.push( Move(*pos, new_pos) );
                }
            }
        }

        moves
    }

    pub fn winner( &self ) -> Option<usize> {
        if self.player_a.len() == 0 { return Some( 1 ); }
        if self.player_b.len() == 0 { return Some( 0 ); }

        if self.player_a.contains_key( & self.get_goal_position( 1 ) ) {
            return Some( 0 );
        }

        if self.player_b.contains_key( & self.get_goal_position( 0 ) ) {
            return Some( 1 );
        }

        let max_a = self.player_a.iter().max().unwrap();
        let max_b = self.player_b.iter().max().unwrap();

        if max_a > max_b { return Some( 0 ); }
        if max_b > max_a { return Some( 1 ); }

        None
    }

    pub fn game_ended( &self ) -> bool {
        self.player_a.len() == 0 || self.player_b.len() == 0 ||
        self.player_a.contains_key( & self.get_goal_position( 1 ) ) ||
        self.player_b.contains_key( & self.get_goal_position( 0 ) )
    }

    pub fn do_move( &mut self, pmove: Move, player_no: usize ) -> bool {
        let mut beaten = false;

        let (player, opponent) = {
            if player_no == 0 { (&mut self.player_a, &mut self.player_b) }
            else { (&mut self.player_b, &mut self.player_a) }
        };

        if opponent.remove( & pmove.1 ).is_some() {
            beaten = true;
        }

        let animal = player.remove( & pmove.0 ).expect( "WTF" );
        player.insert( pmove.1, animal );

        beaten
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn beating_works() {
        use super::Animal::*;

        let list = vec![
            Rat,     Cat,   Dog,  Wolf,
            Panther, Tiger, Lion, Elephant
        ];

        for i in 0..8 {
            for j in 0..8 {
                if i == j { continue; }
                println!( "{:?} vs. {:?}", list[ i ], list[ j ] );

                if list[ i ] == Rat && list[ j ] == Elephant {
                    assert!( Rat.can_beat( & Elephant ) );
                } else if list[ i ] == Elephant && list[ j ] == Rat {
                    assert!( ! Elephant.can_beat( & Rat ) );
                } else {
                    assert_eq!( list[ i ].can_beat( & list[ j ] ), (i >= j) );
                }
            }
        }
    }
}
