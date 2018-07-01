use rustfft::FFTplanner;
use sprs::{CsMat, TriMat};
use ndarray::prelude::*;

pub type Scalar = f64;
pub type Complex = ::num_complex::Complex<Scalar>;

#[derive(FromPrimitive, Debug)]
pub enum ChordType {
    Major,
    Minor,
    Diminished,
    Augumented,
    Suspended2,
    Suspended4
}

#[derive(FromPrimitive, Debug)]
pub enum ChordRoot {
    C, Cis,
    D, Dis,
    E,
    F, Fis,
    G, Gis,
    A, B,
    H
}

#[derive(Debug)]
pub struct Chord(pub ChordRoot, pub ChordType);

pub struct ConstantQ {
    kernel: CsMat<Complex>,
    bins: usize,
    min_freq: Scalar,
    max_freq: Scalar,
    fft_len: usize
}

impl ConstantQ {
    pub fn new( min_freq: Scalar, max_freq: Scalar, bins: usize, fs: Scalar ) -> Self {
        const THRESHOLD: Scalar = 0.0054;

        let fbins = bins as Scalar;

        let Q = 1.0 / (2f64.powf( 1.0 / fbins ) - 1.0);
        let K = (fbins * (max_freq / min_freq).log2().ceil()) as usize;

        let fft_len = 2f64.powf((Q * fs / min_freq).log( 2.0 ).ceil()) as usize;
        let mut planner = FFTplanner::new( false );
        let fft = planner.plan_fft( fft_len );

        let mut matrix = TriMat::new( (K, fft_len) );

        for k in (0..K).rev() {
            let len = (Q * fs / (min_freq * 2f64.powf(k as Scalar / fbins))).ceil() as usize;
            let mut temp = (0..fft_len).map( |i| {
                if i < len {
                    let w = window::hamming( i as Scalar, len as Scalar );
                    let arg = Complex::new( 0.0, -2.0 * 3.1415926535 * Q * i as Scalar / len as Scalar );

                    w * arg.exp() / len as Scalar
                } else { Complex::default() }
            } ).collect::<Vec<_>>();

            let mut spec = vec![ Complex::default(); fft_len ];
            fft.process( &mut temp, &mut spec );

            for (i, &v) in spec.iter().enumerate() {
                if v.norm_sqr().sqrt() <= THRESHOLD { continue; }

                matrix.add_triplet( k, i, v );
            }
        }

        let mut kernel = matrix.to_csc();

        for v in kernel.data_mut() {
            *v = v.conj() / (fft_len - 1) as Scalar;
        }

        ConstantQ {
            kernel,
            bins,
            min_freq,
            max_freq,
            fft_len
        }
    }

    pub fn calculate( &self, mut input: Vec<Complex> ) -> Array1<Complex> {
        while input.len() < self.fft_len {
            input.push( Complex::default() );
        }

        if input.len() > self.fft_len {
            input.truncate( self.fft_len );
        }

        let mut fft_planner = FFTplanner::new( false );
        let fft = fft_planner.plan_fft( self.fft_len );

        let mut output = Array1::from_iter( ::std::iter::repeat( Complex::default() ).take( self.fft_len ) );
        fft.process( &mut input, output.as_slice_mut().unwrap() );

        &self.kernel * &output
    }
}

pub trait CalcPCP {
    fn calc_pcp( &self, input: Vec<Complex> ) -> Vec<Scalar>;
}

impl CalcPCP for ConstantQ {
    fn calc_pcp( &self, input: Vec<Complex> ) -> Vec<Scalar> {
        let mut bins = vec![0.; 12];
        let cqt = self.calculate( input );
        let octaves = (self.max_freq / self.min_freq).log2().ceil() as usize;

        let mut max = 0.0;

        for b in 0..self.bins {
            let value = (0..octaves).filter_map( |m| cqt.get( b + m * self.bins ) ).map( |v| v.norm_sqr().sqrt() ).sum();
            bins[ (b + 7) % 12 ] = value;

            if value > max { max = value; }
        }

        if max != 0.0 {
            bins.iter_mut().for_each( |v| *v /= max );
        }

        bins
    }
}

pub mod window {
    pub fn hamming( x: super::Scalar, len: super::Scalar ) -> super::Scalar {
        0.53836 - 0.46164 * (2.0 * 3.1415926535 * x / (len - 1.0) ).cos()
    }
}
