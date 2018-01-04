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
extern crate image;
pub extern crate rand;
extern crate petgraph;
extern crate itertools;
extern crate num;
pub extern crate palette;
#[macro_use] extern crate maplit;
#[macro_use] extern crate lazy_static;
extern crate tess2;

pub mod geom;
pub mod sketch;
pub mod patterns;
pub mod actors;
pub mod errors;
pub mod composition;
pub mod tween;
pub mod transforms;
pub mod generators;

mod color;
mod properties;
mod gpu;
mod mesh;

pub use actors::*;
pub use errors::*;
pub use geom::*;
pub use patterns::*;
pub use sketch::*;
pub use color::*;
pub use mesh::*;
pub use composition::*;
pub use tween::*;
pub use generators::*;
pub use transforms::*;

#[cfg(test)]
mod test {
    #[test]
    fn works() {
        use lyon;
        use rand::{Rng, StdRng};
        use lyon::tessellation::*;
        use lyon::tessellation::geometry_builder::{VertexBuffers, simple_builder};
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