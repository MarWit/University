use na::{RowVectorN, MatrixN};
use ndarray::Array2;

use std::default::Default;

use utils::Scalar;

macro_rules! modulus {
    ($a:expr, $m:expr) => {
        (($a) % $m + $m) % $m
    }
}

pub type RowVector12 = RowVectorN<Scalar, ::na::U12>;
pub type RowVector24 = RowVectorN<Scalar, ::na::U24>;
pub type Matrix12 = MatrixN<Scalar, ::na::U12>;
pub type Matrix24 = MatrixN<Scalar, ::na::U24>;

#[derive(Copy, Clone)]
pub struct Gaussian {
    pub μ: RowVector12,
    pub Σ: Matrix12
}

impl Gaussian {
    pub fn new( μ: RowVector12, Σ: Matrix12 ) -> Self {
        Gaussian { μ, Σ }
    }

    pub fn calculate( &self, x: RowVector12 ) -> Scalar {
        let denominator = (2. * 3.14159265358f64).powf( 6.0 ) * self.Σ.determinant().sqrt();
        if denominator.abs() < 1e-7 {
            return 1e-7;
        }

        let arg = -0.5 * (x - self.μ) * self.Σ.try_inverse().unwrap() * (x - self.μ).transpose();
        arg[ (0,0) ].exp() / denominator
    }
}

pub struct HMM {
    pub Π: RowVector24,
    pub A: Matrix24,
    pub B: [Gaussian; 24]
}

impl HMM {
    fn viterbi( &self, X: Vec<RowVector12> ) -> Vec<usize> {
        let mut B = Array2::<Scalar>::zeros( (X.len(), 24) );

        for (j, &x) in X.iter().enumerate() {
            for (i, g) in self.B.iter().enumerate() {
                B[ [j, i] ] = g.calculate( x );
            }
        }

        let mut T1 = Array2::<Scalar>::zeros( (24, X.len()) );
        let mut T2 = Array2::<usize>::zeros( (24, X.len()) );

        for i in 0..24 {
            T1[ [i, 0] ] = self.Π[ i ] * B[ [0, i] ];
        }

        for i in 1..X.len() {
            for j in 0..24 {
                let (k, val) = (0..24).map( |k| (k, T1[ [k, i - 1] ] * self.A[ (k, j) ] * B[ [j, i] ]) )
                                      .fold( (0, -1./0.), |a, v| {
                                          if v.1 > a.1 { v }
                                          else { a }
                                      } );

                T1[ [j, i] ] = val;
                T2[ [j, i] ] = k;
            }
        }

        let mut path = vec![];
        let mut z = (0..24).map( |k| (k, T1[ [k, X.len() - 1] ]) )
                            .fold( (0, -1./0.), |a, v| {
                                if v.1 > a.1 { v }
                                else { a }
                            } ).0;
        path.push( z );

        for i in (1..X.len()).rev() {
            z = T2[ [z, i] ];
            path.push( z );
        }

        path.into_iter().rev().collect()
    }

    // TODO: For completness we need Baum-Welchs're "Forward-Backward alghorithm"
}

impl Default for HMM {
    fn default() -> Self {
        let Π = RowVector24::repeat( 1.0 ) / 24.0;
        let mut A = Matrix24::from_fn( |r, c| {
            let (r, c) = (r as i32, c as i32);

            let distance = modulus!(r - c, 24).min( modulus!(c - r, 24) );
            let k = (12 - distance) as Scalar;
            let ε = 1e-3;

            (k + ε) / (144. + 24. * ε)
        } );

        let mut B = [Gaussian::new(RowVector12::zeros(), Matrix12::zeros()); 24];

        for (i, g) in B.iter_mut().enumerate() {
            let tonic = i % 12;
            let mediant = if i < 12 {
                // Major chord
                (tonic + 4) % 12
            } else {
                // Minor chord
                (tonic + 3) % 12
            };
            let dominant = (tonic + 7) % 12;

            g.μ[ tonic ] = 1.0;
            g.μ[ mediant ] = 1.0;
            g.μ[ dominant ] = 1.0;

            g.Σ.fill_diagonal( 0.2 );

            // All notes in chord are dependant on each other
            g.Σ[ (tonic, tonic) ] = 1.0;
            g.Σ[ (mediant, mediant) ] = 1.0;
            g.Σ[ (dominant, dominant) ] = 1.0;

            // There is less correlation between mediant and tonic than rest
            g.Σ[ (dominant, tonic) ] = 0.8;
            g.Σ[ (tonic, dominant) ] = 0.8;
            g.Σ[ (mediant, dominant) ] = 0.8;
            g.Σ[ (dominant, mediant) ] = 0.8;
            g.Σ[ (tonic, mediant) ] = 0.6;
            g.Σ[ (dominant, tonic) ] = 0.6;

        }

        HMM { Π, A, B }
    }
}
