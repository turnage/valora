//! A rasterizer for fine art.

mod ext;
mod grid_lines;
mod regions;
mod sampling;
mod stroker;

pub use lyon_path::{
    iterator::{Flattened, FlattenedIterator},
    Builder,
    Path,
    PathEvent,
};
pub use regions::ShadeCommand;
pub use sampling::SampleDepth;
pub type V2 = Point2D<f32, UnknownUnit>;

use euclid::{Point2D, UnknownUnit};
use lyon_geom::LineSegment;

use regions::RegionList;

/// Generates commands to shade the area inside the path. The path is automatically closed by
/// assuming an edge from the last to the first vertex.
pub fn fill_path(
    builder: Builder,
    sample_depth: SampleDepth,
) -> impl Iterator<Item = ShadeCommand> {
    let path = builder.build();
    let samples_per_pixel: u64 = sample_depth.into();
    let builder = Flattened::new(1.0 / samples_per_pixel as f32, path.into_iter());
    RegionList::from(builder.line_segments().collect::<Vec<LineSegment<f32>>>())
        .shade_commands(sample_depth)
}

pub fn stroke_path(
    path: Builder,
    thickness: f32,
    sample_depth: SampleDepth,
) -> impl Iterator<Item = ShadeCommand> {
    std::iter::empty() /*
                       let builder = FlatteningBuilder(path, 1.0 / sample_depth.into() as f32);
                       RegionList::from(
                           stroker::stroke_path(builder.into_iter(), thickness)
                               .filter_map(<Option<RasterSegment>>::from)
                               .collect::<Vec<RasterSegment>>(),
                       )
                       .shade_commands(sample_depth)*/
}

pub fn raster_path(
    path: Builder,
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
