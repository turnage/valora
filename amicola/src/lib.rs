//! A CPU rasterizer for fine art.

mod geo;
mod grid_lines;
mod path;
mod regions;
mod sampling;

pub use self::geo::{Polygon, V2, V4};
pub use regions::{RegionList, ShadeCommand};

/// The method by which the rasterizer will raster the vector path.
#[derive(Debug, Clone, Copy)]
pub enum RasterMethod {
    /// In fill method, the rasterizer will treat all the area inside the path as part of the
    /// rastered area. In this method, paths are assumed to be closed.
    Fill,
    /// In stroke method, the rasterizer will treat the area immediately adjacent the path within
    /// the given thickness as part of the rastered area. In this method, paths are assumed to be
    /// open.
    Stroke(f32),
}
