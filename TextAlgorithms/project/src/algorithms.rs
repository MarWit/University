use std::ops::Index;

use shrinkwraprs::Shrinkwrap;

#[derive(Shrinkwrap)]
struct PaddedSlice<'a, T>(&'a [T]);

#[cfg(test)]
mod tests;

macro_rules! dump_memory {
    ($name:expr) => {
        #[cfg(feature = "mem_bench")]
        {
            use jemalloc_ctl::{epoch, stats};
            epoch::advance().unwrap();
            eprintln!("{}: {}KB", $name, stats::allocated::read().unwrap() / 1024);
        }
    };
}

impl Index<usize> for PaddedSlice<'_, u32> {
    type Output = u32;

    fn index(&self, i: usize) -> &Self::Output {
        if i >= self.len() {
            &0
        } else {
            &self.0[i]
        }
    }
}

impl<'a, T> From<&'a [T]> for PaddedSlice<'a, T> {
    fn from(e: &'a [T]) -> Self {
        Self(e)
    }
}

pub const ALPHABET_SIZE: usize = 64;
pub fn transform_char(c: char) -> u32 {
    (match c {
        '$' => 0,
        'A'..='Z' => (c as u8 - b'A') * 2 + 1,
        'a'..='z' => (c as u8 - b'a') * 2 + 2,
        '0'..='9' => (c as u8 - b'0') + 53,
        _ => 63,
    }) as u32
}

pub fn transformer<S: AsRef<str>>(input: S) -> Vec<u32> {
    input.as_ref().chars().map(transform_char).collect()
}

fn radix_sort(a: &[u32], b: &mut [u32], r: &[u32], n: usize, k: usize) {
    let mut c = vec![0; k + 1];
    let r: PaddedSlice<_> = r.into();

    for i in 0..n {
        c[r[a[i] as usize] as usize] += 1;
    }

    let mut sum = 0;
    for v in &mut c {
        let t = *v;
        *v = sum;
        sum += t;
    }

    for &v in a {
        b[c[r[v as usize] as usize] as usize] = v;
        c[r[v as usize] as usize] += 1;
    }
}

pub fn suffix_array(s: &[u32], k: usize) -> Vec<u32> {
    let s: PaddedSlice<_> = s.into();
    let n = s.len();
    let n0 = (n + 2) / 3;
    let n1 = (n + 1) / 3;
    let n2 = n / 3;
    let n02 = n0 + n2;

    let mut s12 = vec![0; n02];
    let mut sa12 = vec![0; n02];
    let mut s0 = vec![0; n0];
    let mut sa0 = vec![0; n0];

    for (i, j) in (0..n + n0 - n1).filter(|i| (i % 3) != 0).enumerate() {
        s12[i] = j as _;
    }

    radix_sort(&s12, &mut sa12, &s.0[2..], n02, k);
    radix_sort(&sa12, &mut s12, &s.0[1..], n02, k);
    radix_sort(&s12, &mut sa12, &s, n02, k);

    let mut name = 0;
    let (mut c0, mut c1, mut c2) = (-1i64, -1i64, -1i64);

    for i in 0..n02 {
        let sa12i = sa12[i] as usize;
        if s[sa12i] as i64 != c0 || s[sa12i + 1] as i64 != c1 || s[sa12i + 2] as i64 != c2 {
            name += 1;
            c0 = s[sa12i] as _;
            c1 = s[sa12i + 1] as _;
            c2 = s[sa12i + 2] as _;
        }

        if sa12[i] % 3 == 1 {
            s12[sa12i / 3] = name as u32;
        } else {
            s12[sa12i / 3 + n0] = name as u32;
        }
    }

    if name < n02 {
        sa12 = suffix_array(&s12, name);
        for i in 0..n02 {
            s12[sa12[i] as usize] = i as u32 + 1;
        }
    } else {
        for i in 0..n02 {
            sa12[s12[i] as usize - 1] = i as u32;
        }
    }

    for (i, j) in (0..n02).filter(|&i| (sa12[i] as usize) < n0).enumerate() {
        s0[i] = 3 * sa12[j];
    }
    radix_sort(&s0, &mut sa0, &s, n0, k);

    let mut p = 0;
    let mut k = 0;
    let mut t = n0 - n1;

    let s12: PaddedSlice<_> = PaddedSlice(&s12);
    let sa12: PaddedSlice<_> = PaddedSlice(&sa12);
    let mut sa = vec![0; n];

    while k < n {
        let sa12t = sa12[t] as usize;
        let i = if sa12t < n0 {
            sa12t * 3 + 1
        } else {
            (sa12t - n0) * 3 + 2
        };
        let j = sa0[p] as usize;

        if (sa12t < n0 && (s[i], s12[sa12t + n0]) <= (s[j], s12[j / 3]))
            || (sa12t >= n0
                && (s[i], s[i + 1], s12[sa12t - n0 + 1]) <= (s[j], s[j + 1], s12[j / 3 + n0]))
        {
            sa[k] = i as u32;
            t += 1;

            if t == n02 {
                k += 1;
                while p < n0 {
                    sa[k] = sa0[p];
                    p += 1;
                    k += 1;
                }
            }
        } else {
            sa[k] = j as u32;
            p += 1;

            if p == n0 {
                k += 1;
                while t < n02 {
                    sa[k] = if (sa12[t] as usize) < n0 {
                        sa12[t] * 3 + 1
                    } else {
                        (sa12[t] - n0 as u32) * 3 + 2
                    };
                    t += 1;
                    k += 1;
                }
            }
        }

        k += 1;
    }

    sa
}

/// Longest common prefix in 13n bytes (Kasai et al.)
///
/// **Lemma**: For any suffix S[i..n], LCP[RSA[i]] - 1 <= LCP[RSA[i+1]],
/// assuming i < n and neither RSA[i] nor RSA[i + 1] are the first
/// element of the suffix array.
///
/// We will want to calculate next entries in RSA[i] order, so we will be able
/// to exploit fact that next LCP entry we will calculate is
/// lower bounded by previous entry - 1. Therefore we can start matching string
/// from k = LCP[i-1] - 1.
///
/// Consider S = "banana$", then
/// SA = [7 6 4 2 1 5 3], RSA = [5 4 7 3 6 2 1]
/// We can then calculate LCP in the order given by our lemma
/// 4:5 =>  0
/// 3:4 =>  3
/// 6:7 =>  2
/// 2:3 =>  1
/// 5:6 =>  0         Therefore our LCP is
/// 1:2 =>  0         LCP = [-1 0 1 3 0 0 2]
///  :1 => -1
pub fn lcp_kasai(s: &[u32], sa: &[u32]) -> Vec<i32> {
    let n = sa.len();
    let mut rsa = vec![0; n];
    let mut lcp = vec![0; n];

    for i in 0..n {
        rsa[sa[i] as usize] = i;
    }

    let mut h = 0;
    for i in 0..n {
        let k = rsa[i];
        if k == 0 {
            lcp[k] = -1;
        } else {
            let j = sa[k - 1] as usize;
            while i + h < n && j + h < n && s[i + h] == s[j + h] {
                h += 1;
            }
            lcp[k] = h as _;
        }

        if h > 0 {
            h -= 1;
        }
    }

    dump_memory!("inside");

    lcp
}

/// Calculate Rank Next array based on S and SA
///
/// RankNext[i] = RSA[SA[i] + 1]
/// It describes the rank (RSA) of the suffix S[SA[i] + 1..], that is
/// index of suffix $i$ with removed first character.
fn rank_next(s: &[u32], sa: &[u32], sigma: usize) -> (usize, Vec<i32>) {
    let n = s.len();
    let mut rank_next = vec![0; n];
    let mut count = vec![0; sigma + 1];

    for i in 0..n {
        count[s[i] as usize + 1] += 1;
    }

    for i in 1..sigma {
        count[i] += count[i - 1];
    }

    count[s[n - 1] as usize] += 1;

    let mut k = 0;
    for i in 0..n {
        if sa[i] == 0 {
            k = i;
        } else {
            let c = s[sa[i] as usize - 1] as usize; // BWT[i]
            count[c] += 1;
            let j = count[c];
            rank_next[j - 1] = i as _;
        }
    }

    dump_memory!("rank_next");

    (k, rank_next)
}

/// Longest common prefix in 9n bytes
///
/// We are using the fact, that we really only need
/// RSA[0], RSA[1], ..., RSA[n-1] in that exact order. Also, after using
/// given RSA value, we don't need it anymore. Therefore we can just
/// calculate it in the fly using RankNext array.
///
/// **Observation**: RankNext[RSA[j]] = RSA[SA[RSA[j]] + 1] = RSA[j + 1]
///
/// Given that, to calculate all RSA values we only need RSA[0]
/// and RankNext array. Code below is basicly copy of the `lcp_kasai`
/// except we are using RankNext array which we store at the beginning
/// in the lcp array to save space. That allows us to shove 4n bytes.
pub fn lcp9(s: &[u32], sa: &[u32], sigma: usize) -> Vec<i32> {
    let (mut k, mut lcp) = rank_next(s, sa, sigma);
    let n = sa.len();

    let mut h = 0;
    for i in 0..n {
        let next_k = lcp[k];
        if k == 0 {
            lcp[k] = -1;
        } else {
            let j = sa[k - 1] as usize;
            while i + h < n && j + h < n && s[i + h] == s[j + h] {
                h += 1;
            }
            lcp[k] = h as _;
        }

        if h > 0 {
            h -= 1;
        }
        k = next_k as _;
    }

    dump_memory!("inside");

    lcp
}

/// Calculate Rank Next array inplace based on S and BWT
///
/// Same as `rank_next` function except we are now using BWT array
/// instead of calculating it's entries on the fly using SA.
fn rank_next_bwt(s: &[u32], bwt: &[i8], sigma: usize, out: &mut [i32]) -> usize {
    let n = s.len();
    let mut count = vec![0u32; sigma + 1];

    for i in 0..n {
        count[s[i] as usize + 1] += 1;
    }

    for i in 1..sigma {
        count[i] += count[i - 1];
    }

    let mut k = 0;
    for (i, &b) in bwt.iter().enumerate() {
        if b < 0 {
            k = i;
        } else {
            count[b as usize] += 1;
            let j = count[b as usize] as usize;
            out[j - 1] = i as _;
        }
    }

    dump_memory!("rank_next_bwt");

    k
}

/// Longest common prefix in (6 + δ)n bytes
///
/// **Lemma**: Assume k = RSA[i]. If k > 0 and BWT[k-1] = BWT[k]
/// then LCP[i] = LCP[i-1] - 1
///
/// So, as long BWT[k-1] = BWT[k] holds, we dont need any extra data
/// to calculate next LCP entry. Therefore we need to store SA entries only
/// for such k, that BWT[k-1] != BWT[k].
/// Main idea for this algorithm is to store all the computations
/// inside SA array, including calculated LCP array. We also need
/// to calculate BWT so we can use it to calculate RSA and
/// also to generate _smaller SA_ (`ssa` in code) with SA entries
/// that we care about. 
pub fn lcp6(s: &[u32], sa: &mut [i32], sigma: usize) {
    let n = sa.len();
    let bwt = (0..n)
        .into_iter()
        .map(|i| {
            if sa[i] == 0 {
                -1
            } else {
                s[sa[i] as usize - 1] as i8
            }
        })
        .collect::<Vec<_>>();

    let mut z = 0;
    for i in 1..n {
        if bwt[i - 1] != bwt[i] {
            z += 1;
        }
    }

    dump_memory!("inside1");

    let mut ssa = Vec::with_capacity(z);
    let mut k = rank_next_bwt(s, &bwt, sigma, sa);
    let rsa1 = k;

    // Storing indices for required SA entries, in RSA order
    for _ in 1..n {
        if bwt[k - 1] != bwt[k] {
            ssa.push(k - 1);
        }
        k = sa[k] as _;
    }

    k = rsa1;

    // Calculating SA array
    for i in 0..n {
        let next_k = sa[k];
        sa[k] = i as i32;
        k = next_k as _;
    }

    // SA_compact[k] = SA[RSA[k]]
    for v in 0..z {
        ssa[v] = sa[ssa[v]] as _;
    }

    k = rank_next_bwt(s, &bwt, sigma, sa);

    dump_memory!("inside2");

    let mut h = 0;
    let mut v = 0;
    for i in 0..n {
        let next_k = sa[k];
        if k == 0 {
            sa[k] = -1;
        } else if bwt[k - 1] == bwt[k] {
            sa[k] = h as _;
        } else {
            let j = ssa[v];
            v += 1;

            while i + h < n && j + h < n && s[i + h] == s[j + h] {
                h += 1;
            }

            sa[k] = h as _;
        }

        if h > 0 {
            h -= 1;
        }

        k = next_k as _;
    }

    dump_memory!("inside3");
}
