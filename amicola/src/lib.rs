//! A rasterizer for fine art.

mod ext;
mod geo;
mod grid_lines;
mod path;
mod regions;
mod sampling;

pub use self::geo::{Path, PathSegment, V2, V4};
pub use regions::ShadeCommand;
pub use sampling::SampleDepth;

use regions::RegionList;

/// Generates commands to shade the area inside the path. The path is automatically closed by
/// assuming an edge from the last to the first vertex.
pub fn fill_path(path: &Path, sample_depth: SampleDepth) -> impl Iterator<Item = ShadeCommand> {
    RegionList::from(path).shade_commands(sample_depth)
}

/// The method by which the rasterizer will rasterize the vector path.
#[derive(Debug, Clone, Copy)]
pub enum Method {
    /// In fill method, the rasterizer will treat all the area inside the path as part of the
    /// raster area. In this method, paths are automatically closed by assuming an edge from the
    /// last to the first vertex.
    Fill,
    /// In stroke method, the rasterizer will treat the area immediately adjacent the path within
    /// the given thickness as part of the rastered area. In this method, paths are left open
    /// and no edge between the last and first vertex is assumed.
    Stroke(f32),
}
