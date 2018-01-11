#![feature(specialization)]
#![feature(use_nested_groups)]
#![feature(unboxed_closures)]
#![feature(fnbox)]
#![feature(nll)]
#![feature(crate_in_paths)]
#![recursion_limit="128"]

#[macro_use] extern crate error_chain;
#[macro_use] pub extern crate glium;
extern crate lyon;
pub extern crate image;
pub extern crate rand;
extern crate petgraph;
extern crate itertools;
extern crate num;
pub extern crate palette;
#[macro_use] extern crate maplit;
#[macro_use] extern crate lazy_static;
extern crate tess2;
extern crate rayon;
pub extern crate poly;

pub mod sketch;
pub mod patterns;
pub mod actors;
pub mod errors;
pub mod composition;
pub mod tween;
pub mod generators;

pub mod color;
mod pipes;
mod gpu;
mod mesh;

pub use actors::*;
pub use errors::*;
pub use poly::*;
pub use patterns::*;
pub use sketch::*;
pub use color::*;
pub use mesh::*;
pub use composition::*;
pub use tween::*;
pub use generators::*;
pub use transforms::*;
pub use gpu::shaders;

#[cfg(test)]
mod test {
    #[test]
    fn works() {
        use lyon;
        use rand::{Rng, StdRng};
        use lyon::tessellation::*;
        use lyon::tessellation::polyetry_builder::{VertexBuffers, simple_builder};
        use lyon::path_builder::math::Point;


        let n = 2560;
        let mut rng = StdRng::new().expect("");
        let verts = (0..n).into_iter().map(|_| -> Point {Point::new(rng.gen::<f32>() * 1000.0,rng.gen::<f32>() * 1000.0) });
        let mut buffer: VertexBuffers<FillVertex> = VertexBuffers::new();
        basic_shapes::fill_polyline(
            verts,
            &mut FillTessellator::new(),
            &FillOptions::default().assume_no_intersections(),
            &mut simple_builder(&mut buffer));
    }
}
