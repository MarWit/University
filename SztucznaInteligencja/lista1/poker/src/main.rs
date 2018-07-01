extern crate rand;

use std::collections::BTreeSet;
use rand::Rng;

/// (Color, Figure)
type Card = (usize, usize);

#[derive(Debug, PartialOrd, PartialEq)]
enum Hand {
    HighCard,
    OnePair,
    TwoPairs,
    ThreeOfKind,
    Straight,
    Flush,
    FullHouse,
    FourOfKind,
    StraightFlush
}

fn generate_hand( start : usize, end : usize ) -> Vec<Card> {
    let mut set = BTreeSet::new();

    while set.len() < 5 {
        let color = rand::thread_rng().gen_range( 0, 4 );
        let figure = rand::thread_rng().gen_range( start, end );

        set.insert( (color, figure) );
    }

    set.into_iter().collect()
}

fn score( hand : &mut Vec<Card> ) -> Hand {
    use self::Hand::*;

    let mut best = HighCard;
    let mut pairs = (2..15).map( |i| hand.iter().filter( |&&(_, v)| i == v ).count() )
                           .filter( |&v| v >= 2 )
                           .collect::<Vec<_>>();

    pairs.sort_by( |a, b| b.cmp( a ) );

    if pairs.len() > 0 {
        if pairs[ 0 ] == 4 {
            best = FourOfKind;
        } else if pairs.len() == 2 && pairs[ 0 ] + pairs[ 1 ] == 5 {
            best = FullHouse;
        } else if pairs[ 0 ] == 3 {
            best = ThreeOfKind;
        } else if pairs.len() == 2 {
            best = TwoPairs;
        } else {
            best = OnePair;
        }
    }

    hand.sort_by( |a, b| a.1.cmp( &b.1 ) );

    let (color, mut same_color) = (hand[ 0 ].0, true);
    let (mut value, mut straight) = (hand[ 0 ].1 - 1, true);

    for card in hand {
        if color != card.0 {
            same_color = false;
        }

        if value + 1 != card.0 {
            straight = false;
        } else {
            value = card.0;
        }
    }

    if straight && same_color {
        best = StraightFlush;
    } else if same_color && Flush > best {
        best = Flush;
    } else if straight && Straight > best {
        best = Straight;
    }

    best
}

pub fn main() {
    const N : usize = 50000;

    let (mut b, mut f) = (0.0, 0.0);

    for _ in 0..N {
        let mut blotkarz = generate_hand( 2, 10 );
        let mut figurant = generate_hand( 11, 14 );

        if score( &mut figurant ) >= score( &mut blotkarz ) {
            f += 1.0;
        } else {
            b += 1.0;
        }
    }

    println!( "Blotkarz: {}", b / (N as f32) );
    println!( "Figurant: {}", f / (N as f32) );
}
