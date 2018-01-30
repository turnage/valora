//! valora is declarative generative art libary.
//!
//! Status: Not ready for you yet.
//!
//! When writing art with valora, instead of specifying what to *do*, we
//! specify what we *want*. Even for animations, a valora composition is a
//! single struct we build once, which contains all the information valora
//! needs to render and maybe animate the composition.
//!
//! Where in Processing for example we would
//!
//!    1. Set the global fill color to red.
//!    2. Draw an ellipse.
//!
//! in valora we describe what we want in structs and valora will figure out
//! how to render it later. This means you can toss around your shapes and
//! colors however you want until you're finally ready to draw your
//! composition.
//!
//! ````
//! # use valora::*;
//! let circle = Ellipse::circle(Point::center(), /*radius=*/0.2);
//! let red = Colora::rgb(1.0, 0.0, 0.0, 1.0);
//! let mesh = Mesh::from(circle).with_color(red);
//! ````
//!
//! A valora composition consists of meshes and shaders. In the end you will
//! give valora an ordered set of them to render. Here is an example:
//!
//! ````
//! #   compose(
//! #       CompositionCfg::from_args(),
//! #       |_, _| -> Result<Composition> {
//! # let white = Colora::rgb(1.0, 1.0, 1.0, 1.0);
//! # let red = Colora::rgb(1.0, 0.0, 0.0, 1.0);
//! let background = Mesh::from(Rect::frame()).with_color(white);
//! let circle = Mesh::from(Ellipse::circle(Point::center(), 0.3)).with_color(red);
//!
//! Ok(Composition::new().add(background).add(circle))
//! #      }
//! #  ).expect("working composition")
//! ````
//!
//! ![result](https://i.imgur.com/U2zH3i0.png)
//!
//! You can find the full code in examples/tutorial1.rs, among other examples.
//!
//! Valora re-exports some incredible crates you'll probably end up needing while
//! writing your art:
//! 
//! * [noise-rs](https://github.com/brendanzab/noise-rs) by a group of generous Rustaceans. 
//! * [palette](https://github.com/Ogeon/palette) for color by Erik Hedvall and @sidred
//! * [image](https://github.com/PistonDevelopers/image) by the Piston team.
//! * [glossy](https://github.com/elizagamedev/rust-glossy) for loading shaders by Eliza.
//!
//! Valora also stands on the shoulders of
//!
//! * [glium](https://github.com/glium/glium) the safe OpenGL wrapper by @tomaka & other rustaceans.
//! * [lyon](https://github.com/nical/lyon) by @nical & other rustaceans for tessellation and bezier interpolations.
//! * [petgraph](https://github.com/bluss/petgraph) by @bluss & other rustaceans for graph implementations.
//! * [rayon](https://github.com/rayon-rs/rayon) by the Rayon team for that sweet, sweet parallelism.

#![feature(specialization)]
#![feature(use_nested_groups)]
#![feature(unboxed_closures)]
#![feature(fnbox)]
#![feature(nll)]
#![feature(crate_in_paths)]
#![feature(macro_reexport)]
#![feature(slice_rotate)]
#![recursion_limit = "128"]
#![feature(repr_align)]
#![feature(attr_literals)]

#[macro_use]
extern crate error_chain;
extern crate itertools;
extern crate lyon;
extern crate num;
extern crate petgraph;
extern crate rayon;
extern crate structopt;
#[macro_use]
extern crate structopt_derive;
extern crate tess2;
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
pub use transforms::*;
pub use tween::*;
pub use pipes::*;
pub use gpu::shaders::*;
pub use structopt::StructOpt;
pub use palette::*;
