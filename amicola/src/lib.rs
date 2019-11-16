//! A rasterizer for fine art.

mod bounds;
mod ext;
mod grid_lines;
mod monotonics;
mod path;
mod regions;
mod sampling;
mod stroker;

pub use self::path::{Path, Segment};
pub use regions::ShadeCommand;
pub use sampling::SampleDepth;

use nalgebra::{base::*, Matrix};

pub type V2 = Matrix<f32, U2, U1, ArrayStorage<f32, U2, U1>>;
pub type V4 = Matrix<f32, U4, U1, ArrayStorage<f32, U4, U1>>;

use self::monotonics::RasterSegmentSet;
use regions::RegionList;

/// Generates commands to shade the area inside the path. The path is automatically closed by
/// assuming an edge from the last to the first vertex.
pub fn fill_path(path: &Path, sample_depth: SampleDepth) -> impl Iterator<Item = ShadeCommand> {
    RegionList::from(RasterSegmentSet::build_from_path(path)).shade_commands(sample_depth)
}

pub fn stroke_path(
    path: &Path,
    thickness: f32,
    sample_depth: SampleDepth,
) -> impl Iterator<Item = ShadeCommand> {
    RegionList::from(stroker::stroke_path(path, thickness)).shade_commands(sample_depth)
}

pub fn raster_path(
    path: &Path,
    method: Method,
    sample_depth: SampleDepth,
) -> impl Iterator<Item = ShadeCommand> {
    match method {
        Method::Fill => {
            Box::new(fill_path(path, sample_depth)) as Box<dyn Iterator<Item = ShadeCommand>>
        }
        Method::Stroke(thickness) => Box::new(stroke_path(path, thickness, sample_depth))
            as Box<dyn Iterator<Item = ShadeCommand>>,
    }
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
