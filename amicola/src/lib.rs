//! A rasterizer for fine art.

mod ext;
mod grid_lines;
mod regions;
mod sampling;

pub use lyon_path::{iterator::Flattened, Builder, Event, Path, PathEvent};
pub use regions::ShadeCommand;
pub use sampling::SampleDepth;
pub type V2 = Point2D<f32, UnknownUnit>;

use euclid::{Point2D, UnknownUnit};
use lyon_geom::LineSegment;

use regions::RegionList;

fn segment(path_event: PathEvent) -> Option<LineSegment<f32>> {
    match path_event {
        Event::Line { from, to } => Some(LineSegment { from, to }),
        _ => None,
    }
}

/// Generates commands to shade the area inside the path. The path is automatically closed by
/// assuming an edge from the last to the first vertex.
pub fn fill_path(
    builder: Builder,
    sample_depth: SampleDepth,
) -> impl Iterator<Item = ShadeCommand> {
    let path = builder.build();
    let samples_per_pixel: u64 = sample_depth.into();
    let builder = Flattened::new(1.0 / samples_per_pixel as f32, path.into_iter());
    RegionList::from(builder.filter_map(segment)).shade_commands(sample_depth)
}
