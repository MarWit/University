use std::default::Default;

use rand::{self, Rng};
use indextree::{Arena, NodeId, Node};

use player::Player;
use board::{Board, Color};

#[derive(Debug)]
struct NodeInfo<T> {
    pub value: T,
    pub visited: f64,
    pub score: f64,
    pub childs: Option<Vec<T>>,
    pub player_now: Color,
    pub passes: usize
}

pub struct MCTSPlayer;

impl NodeInfo<(usize, usize)> {
    pub fn ended( &mut self, board: &Board ) -> bool {
        if self.childs.is_none() {
            let moves = board.valid_moves( self.player_now );
            if ! moves.is_empty() {
                self.passes = 0;
            } else {
                self.passes += 1;
            };

            self.childs = Some( moves );
        }

        self.childs.as_ref().unwrap().is_empty()
    }
}

impl Player for MCTSPlayer {
    fn do_move( &mut self, board: &Board, color: Color ) -> Option<(usize, usize)> {
        const SAMPLE_NUM: usize = 10_000;

        let childs = board.valid_moves( color );

        let mut tree = Arena::new();
        let root = {
            let node = NodeInfo {
                value: (42, 42),
                visited: 0.0,
                childs: Some( childs ),
                score: 0.0,
                player_now: color,
                passes: 0
            };

            tree.new_node( node )
        };

        let uct_calc = |n: & Node<NodeInfo<(usize, usize)>>| {
            let node = & n.data;
            (node.score / node.visited) + (2.0 * node.visited.ln() / node.visited).sqrt()
        };

        for _ in 0..SAMPLE_NUM {
            let mut node = root;
            let mut state = board.clone();

            while tree[ node ].data.ended( &state ) && ! tree[ node ].data.passes < 2 {
                node = node.children( & tree ).max_by( |&a, &b| uct_calc( & tree[ a ] ).partial_cmp( & uct_calc( & tree[ b ] ) ).unwrap() ).unwrap();

                let step = tree[ node ].data.value;
                state.do_move( step.0, step.1, tree[ node ].data.player_now.invert() );
            }

            // Calculate
            if ! tree[ node ].data.ended( &state ) {
                let child = {
                    let n = &mut tree[ node ].data;
                    let childs = n.childs.as_mut().unwrap();

                    let len = childs.len();
                    let idx = rand::thread_rng().gen_range( 0, len );

                    NodeInfo {
                        value: childs.swap_remove( idx ),
                        visited: 0.0,
                        childs: None,
                        score: 0.0,
                        player_now: n.player_now.invert(),
                        passes: 0
                    }
                };

                state.do_move( child.value.0, child.value.1, tree[ node ].data.player_now );

                let new_node = tree.new_node( child );
                node.append( new_node, &mut tree );
                node = new_node;
            }

            let mut passes = tree[ node ].data.passes;
            let mut now = tree[ node ].data.player_now;

            // Simulation
            while passes < 2 {
                let moves = state.valid_moves( now );
                let step = rand::thread_rng().choose( &moves );

                if let Some( step ) = step {
                    passes = 0;
                    state.do_move( step.0, step.1, now );
                } else {
                    passes += 1;
                }

                now = now.invert();
            }

            // Backtrack
            let score = {
                let sa = state.get_score( color );
                let sb = state.get_score( color.invert() );

                if sa > sb { 1.0 }
                else { 0.0 }
            };

            while let Some( parent ) = tree[ node ].parent() {
                tree[ parent ].data.visited += 1.0;
                tree[ parent ].data.score += score;

                node = parent;
            }
        }

        root.children( & tree )
            .max_by( |&a, &b| tree[ a ].data.visited.partial_cmp( & tree[ b ].data.visited ).unwrap() )
            .map( |a| tree[ a ].data.value )
    }
}


