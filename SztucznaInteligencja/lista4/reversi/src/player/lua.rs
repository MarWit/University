use std::path::Path;
use std::fs::File;
use std::io::Read;
use std::iter::FromIterator;

use board::{Board, Color};
use player::Player;

use rlua::prelude::*;
use rlua::{Function, UserData, UserDataMethods, Variadic};

impl UserData for Color {
    fn add_methods( methods: &mut UserDataMethods<Self> ) {
        methods.add_method( "invert", |_, this, _:()| {
            Ok( this.invert() )
        });
    }
}

impl UserData for Board {
    fn add_methods( methods: &mut UserDataMethods<Self> ) {
        // methods.add_method( "get", |_, this, v: (usize, usize)| {
        //     this.get( v.0, v.1 ).ok_or( ::rlua::Error::RuntimeError )
        // });
        //
        // methods.add_method( "set ", |_, this, v: (usize, usize, Color)| {
        //     this.set( v.0, v.1, v.2 );
        //     Ok( () )
        // });

        methods.add_method( "valid_moves", |_, this, v: Color| {
            Ok( this.valid_moves( v )
                    .into_iter()
                    .map( |e| vec![ e.0, e.1 ] )
                    .collect::<Vec<_>>()
            )
        });

        methods.add_method_mut( "get_score", |_, this, v: Color| {
            Ok( this.get_score( v ) )
        });

    }
}

pub struct LuaPlayer {
    ctx: Lua
}

impl LuaPlayer {
    pub fn new<P: AsRef<Path>>( path: P ) -> Result<Self, Box<::std::error::Error>> {
        let mut file = File::open( path )?;

        let mut buffer = String::new();
        file.read_to_string( &mut buffer )?;

        let lua = Lua::new();
        lua.exec::<()>( & buffer, None )
           .map_err( |e| format!( "rlua error: {}", e ) )?;

        Ok( LuaPlayer {
            ctx: lua
        } )
    }
}

impl Player for LuaPlayer {
    fn do_move( &mut self, board: & Board, color: Color ) -> Option<(usize, usize)> {
        let globals = self.ctx.globals();
        let func: Function = globals.get( "do_move" )
                                    .map_err( |e| format!( "rlua error: {}", e ) )
                                    .unwrap();

        func.call( (board.clone(), color) ).ok()
    }
}
