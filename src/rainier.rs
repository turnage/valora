//! Tools for composing generative fine art.

use crate::amicola::*;
use rand::prelude::*;

#[derive(Debug)]
pub struct Context<S> {
    pub width: usize,
    pub height: usize,
    pub seed: u64,
    pub state: S,
}

pub trait Generate<S> {
    type Output;

    fn generate(&self, ctx: &Context<S>, rng: &mut StdRng) -> Self::Output;
}

impl<S, T, F: Fn(&Context<S>, &mut StdRng) -> T> Generate<S> for F {
    type Output = T;

    fn generate(&self, ctx: &Context<S>, rng: &mut StdRng) -> Self::Output {
        (self)(ctx, rng)
    }
}

pub struct Composition {
    surface: Surface,
}

impl Composition {
    pub fn with_dimensions(width: usize, height: usize) -> Self {
        Self {
            surface: Surface::with_dimensions(width, height),
        }
    }
}
