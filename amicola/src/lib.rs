//! A rasterizer for fine art.
//!
//! Build paths on a [`Canvas`], then [`Canvas::fill`] or [`Canvas::stroke`]
//! them. Each returns a [`Raster`]: a stream of [`ShadeCommand`]s, produced
//! one pixel row at a time, which shades every pixel of the raster area once.
//! Pixels the area's boundary passes through come out individually with their
//! opacity, and runs of pixels entirely inside the area come out as spans.

mod canvas;
mod edge;
mod raster;
mod sampling;

pub use canvas::Canvas;
pub use geo_types;
pub use kurbo;
pub use raster::{FillRule, Raster, ShadeCommand};
pub use sampling::SampleDepth;
