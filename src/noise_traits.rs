//! Noise sampling.

use crate::{P2, P3};
use noise::NoiseFn;

/// A trait for types which are sources of noise, samplable by type `P`.
pub trait NoiseSrc<P> {
    /// Samples noise.
    fn noise(&self, p: P) -> f32;
}

impl<N> NoiseSrc<P2> for N
where
    N: NoiseFn<[f64; 2]>,
{
    fn noise(&self, p: P2) -> f32 { self.get([p.x as f64, p.y as f64]) as f32 }
}

impl<N> NoiseSrc<P3> for N
where
    N: NoiseFn<[f64; 3]>,
{
    fn noise(&self, p: P3) -> f32 { self.get([p.x as f64, p.y as f64, p.z as f64]) as f32 }
}
