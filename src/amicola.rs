//! A CPU rasterizer for fine art.

mod geo;
mod raster;

use derive_more::DebugCustom;
use std::sync::Arc;

pub use self::{
    geo::{Error, Polygon, V2, V4},
    raster::{
        gpu_raster::GpuTarget,
        surface::{FinalBuffer, Surface},
    },
};

pub trait RasterTarget {
    fn clear(&mut self);
    fn raster(&mut self, element: Element);
    fn flush(&mut self);
}

/// The method by which the rasterizer will raster the vector path.
#[derive(Debug, Clone, Copy)]
pub enum RasterMethod {
    /// In fill method, the rasterizer will treat all the area inside the path as part of the
    /// rastered area. In this method, paths are assumed to be closed.
    Fill,
    /// In stroke method, the rasterizer will treat the area immediately adjacent the path within
    /// the given thickness as part of the rastered area. In this method, paths are assumed to be
    /// open.
    Stroke(f64),
}

/// The method by which the rasterizer will generate a color for a pixel which is part of the fill
/// or stroke of a vector path.
#[derive(Clone, DebugCustom)]
pub enum Shader {
    /// Shades the path with a solid color.
    Solid(V4),
}

/// A rasterable element in a composition.
#[derive(Debug)]
pub struct Element {
    pub path: Vec<V2>,
    pub raster_method: RasterMethod,
    pub shader: Shader,
}
