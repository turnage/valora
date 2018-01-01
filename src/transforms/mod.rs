pub mod warp;
pub mod spatial;

pub use self::spatial::*;
pub use self::warp::*;

use rand::Rng;

pub fn iterate<S, F: Fn(S) -> S>(s: S, n: usize, f: F) -> S {
    (0..n).into_iter().fold(s, |s, _| f(s))
}

pub fn iterate_rand<R: Rng, S, F: Fn(S, &mut R) -> S>(s: S, n: usize, r: &mut R, f: F) -> S {
    (0..n).into_iter().fold(s, |s, _| f(s, r))
}
