#![feature(specialization)]
#![feature(use_nested_groups)]
#![feature(unboxed_closures)]

#[macro_use]
extern crate error_chain;
#[macro_use]
pub extern crate glium;
extern crate lyon;
extern crate image;
extern crate rand;
extern crate petgraph;
pub extern crate palette;

pub mod geom;
pub mod sketch;
pub mod shaders;
pub mod patterns;
pub mod actors;
mod properties;
pub mod raster;
mod pipeline;
pub mod textures;
pub mod errors;