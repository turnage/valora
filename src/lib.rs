#![feature(specialization)]
#![feature(use_nested_groups)]
#![feature(unboxed_closures)]
#![feature(fnbox)]

#[macro_use]
extern crate error_chain;
#[macro_use]
pub extern crate glium;
extern crate lyon;
extern crate image;
extern crate rand;
extern crate petgraph;
extern crate itertools;
pub extern crate palette;

pub mod geom;
pub mod sketch;
pub mod shaders;
pub mod patterns;
pub mod actors;
pub mod textures;
pub mod errors;

mod properties;
mod raster;
mod pipeline;

pub use actors::*;
pub use errors::*;
pub use geom::*;
pub use patterns::*;
pub use shaders::*;
pub use sketch::*;
pub use textures::*;