//! Tools for composing generative fine art.

use crate::amicola::*;
use rand::prelude::*;

pub use crate::amicola::{Shader, V2, V4};

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

pub trait Render<S> {
    fn render(&self, ctx: &Context<S>, rng: &mut StdRng, comp: &mut Composition);
}

pub struct Composition {
    surface: Surface,
    current_path: Vec<V2>,
    current_shader: Shader,
}

impl Composition {
    pub fn with_dimensions(width: usize, height: usize) -> Self {
        Self {
            surface: Surface::with_dimensions(width, height),
            current_path: vec![],
            current_shader: Shader::Solid(V4::new(1.0, 1.0, 1.0, 1.0)),
        }
    }

    pub fn move_to(&mut self, dest: V2) {
        self.current_path = vec![dest];
    }

    pub fn line_to(&mut self, dest: V2) {
        self.current_path.push(dest);
    }

    pub fn fill(&mut self) {
        let mut path = vec![];
        std::mem::swap(&mut self.current_path, &mut path);
        raster(
            &mut self.surface,
            Element {
                path,
                shader: self.current_shader.clone(),
                raster_method: RasterMethod::Fill,
            },
        );
    }

    pub fn finalize(self) -> FinalBuffer {
        self.surface.into()
    }
}
