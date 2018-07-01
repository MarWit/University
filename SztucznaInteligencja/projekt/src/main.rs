#![feature(iterator_step_by)]
#![feature(non_ascii_idents)]

extern crate alsa;
extern crate rb;
extern crate rustfft;
extern crate sprs;
extern crate ndarray;
extern crate num_complex;
extern crate num_traits;
#[macro_use] extern crate num_derive;
extern crate nalgebra as na;
#[macro_use] extern crate lazy_static;

mod hmm;
mod live;
mod utils;

use std::env;
use std::thread;

use alsa::{ValueOr, Direction};
use alsa::pcm::{PCM, HwParams, Format, Access};
use alsa::poll::{PollDescriptors, poll};

use rb::*;

use utils::{Scalar, Complex, CalcPCP};

type Result<T> = ::std::result::Result<T, Box<::std::error::Error>>;

fn capture( monitor: &str, prod: Producer<i16> ) -> Result<()> {
    const PERIOD_SIZE: usize = 8192;
    const SAMPLING_RATE: usize = 22050 / 2;

    let pcm = PCM::new( monitor, Direction::Capture, true )?;
    let hw = HwParams::any( &pcm )?;

    hw.set_channels( 1 )?;
    hw.set_rate( SAMPLING_RATE as _, ValueOr::Nearest )?;
    hw.set_format( Format::s16() )?;
    hw.set_access( Access::RWInterleaved )?;
    hw.set_period_size( PERIOD_SIZE as _, ValueOr::Nearest )?;
    pcm.hw_params( &hw )?;

    let mut fds = pcm.get()?;
    let io = pcm.io_i16()?;
    let mut buffer = [0i16; PERIOD_SIZE];

    loop {
        poll( &mut fds, -1 ).unwrap();

        if let Ok( cnt ) = io.readi( &mut buffer ) {
            prod.write_blocking( &buffer[ 0..cnt ] );
        }
    }
}

fn application() -> Result<()> {
    const PERIOD_SIZE: usize = 8192;
    const SAMPLING_RATE: Scalar = 22050. / 2.;

    let monitor = env::args().nth( 1 ).unwrap_or( "pulse_monitor".into() );

    let rb = rb::SpscRb::new( PERIOD_SIZE * 8 );
    let (prod, cons) = (rb.producer(), rb.consumer());

    let const_q = utils::ConstantQ::new( 96.0, 5250.0, 12, SAMPLING_RATE );

    thread::spawn( move || {
        capture( &monitor, prod ).unwrap();
    } );

    let mut buffer = [0i16; PERIOD_SIZE - 1024];
    let mut input = [Complex::default(); PERIOD_SIZE];

    loop {
        for i in 0..1024 {
            input[ i ] = input[ PERIOD_SIZE - 1024 + i ];
        }

        cons.read_blocking( &mut buffer ).unwrap();

        for (i, &v) in buffer.into_iter().enumerate() {
            input[ i + 1024 ] = Complex::new( v as Scalar, 0.0 );
        }

        let x = const_q.calc_pcp( input.to_vec() );
        println!( "{:?}", live::classify_chord( &x ) );
    }
}

fn main() {
    if let Err( e ) = application() {
        println!( "[Error] {}", e );
    }
}
