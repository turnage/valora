#![feature(specialization)]
#![feature(use_nested_groups)]
#![feature(unboxed_closures)]
#![feature(fnbox)]
#![feature(nll)]
#![feature(crate_in_paths)]
#![recursion_limit = "128"]

#[macro_use]
extern crate error_chain;
extern crate itertools;
#[macro_use]
extern crate lazy_static;
extern crate lyon;
#[macro_use]
extern crate maplit;
extern crate num;
extern crate petgraph;
extern crate rayon;
extern crate tess2;
#[macro_use]
pub extern crate glium;
pub extern crate image;
pub extern crate palette;
pub extern crate poly;
pub extern crate rand;

pub mod sketch;
pub mod patterns;
pub mod actors;
pub mod errors;
pub mod composition;
pub mod tween;
pub mod generators;
pub mod transforms;

pub mod color;
mod pipes;
mod gpu;
mod mesh;

pub use actors::*;
pub use color::*;
pub use composition::*;
pub use errors::*;
pub use generators::*;
pub use gpu::shaders;
pub use mesh::*;
pub use patterns::*;
pub use poly::*;
pub use sketch::*;
pub use transforms::*;
pub use tween::*;

#[cfg(test)]
mod test {
    #[test]
    fn works() {
        use lyon;
        use lyon::path_builder::math::Point;
        use lyon::tessellation::*;
        use lyon::tessellation::polyetry_builder::{simple_builder, VertexBuffers};
        use rand::{Rng, StdRng};

        let n = 2560;
        let mut rng = StdRng::new().expect("");
        let verts = (0..n)
            .into_iter()
            .map(|_| -> Point { Point::new(rng.gen::<f32>() * 1000.0, rng.gen::<f32>() * 1000.0) });
        let mut buffer: VertexBuffers<FillVertex> = VertexBuffers::new();
        basic_shapes::fill_polyline(
            verts,
            &mut FillTessellator::new(),
            &FillOptions::default().assume_no_intersections(),
            &mut simple_builder(&mut buffer),
        );
    }
}
