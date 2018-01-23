#![feature(specialization)]
#![feature(use_nested_groups)]
#![feature(unboxed_closures)]
#![feature(fnbox)]
#![feature(nll)]
#![feature(crate_in_paths)]
#![feature(macro_reexport)]
#![feature(slice_rotate)]
#![recursion_limit = "128"]

#[macro_use]
extern crate error_chain;
extern crate itertools;
#[macro_use]
extern crate lazy_static;
extern crate lyon;
extern crate num;
extern crate petgraph;
extern crate rayon;
extern crate tess2;
extern crate structopt;
#[macro_use]
extern crate structopt_derive;
#[macro_use]
pub extern crate glium;
#[macro_use]
#[macro_reexport(shader)]
pub extern crate glossy;
pub extern crate image;
pub extern crate noise;
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
pub mod entangle;
pub mod pipes;

mod gpu;
mod mesh;

pub use entangle::*;
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
pub use pipes::*;
pub use gpu::shaders::*;
pub use structopt::StructOpt;
