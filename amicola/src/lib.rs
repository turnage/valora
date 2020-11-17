//! A rasterizer for fine art.

mod ext;
mod grid_lines;
mod regions;
mod sampling;

pub use geo_types::{CoordinateType, Line, LineString, MultiPolygon, Polygon};
pub use lyon_path::{iterator::Flattened, Builder, Event, Path, PathEvent};
pub use regions::ShadeCommand;
pub use sampling::SampleDepth;
pub type V2 = Point2D<f32, UnknownUnit>;

use euclid::{Point2D, UnknownUnit};
use itertools::Itertools;
use lyon_geom::{math::point, LineSegment};
use num_traits::NumCast;

use regions::RegionList;

fn segment(path_event: PathEvent) -> Option<LineSegment<f32>> {
    match path_event {
        Event::Line { from, to } => Some(LineSegment { from, to }),
        _ => None,
    }
}

fn line_string_lines<T: CoordinateType>(
    line_string: LineString<T>,
) -> impl Iterator<Item = LineSegment<f32>> {
    let to_f32 = |coordinate: T| -> f32 { NumCast::from(coordinate).expect("NumCast") };

    line_string
        .into_iter()
        .tuple_windows::<(_, _)>()
        .map(move |(start, end)| LineSegment {
            from: point(to_f32(start.x), to_f32(start.y)),
            to: point(to_f32(end.x), to_f32(end.y)),
        })
}

fn polygon_edges<T: CoordinateType>(
    polygon: Polygon<T>,
) -> impl Iterator<Item = (LineSegment<f32>, i32)> {
    let (exterior, interiors) = polygon.into_inner();

    let positive_wind = 1;
    let exterior = line_string_lines(exterior).map(move |line| (line, positive_wind));

    let negative_wind = -1;
    let interior = interiors
        .into_iter()
        .flat_map(line_string_lines)
        .map(move |line| (line, negative_wind));

    exterior.chain(interior)
}

pub fn raster(
    shape: impl Iterator<Item = Polygon<impl CoordinateType>>,
    sample_depth: SampleDepth,
) -> impl Iterator<Item = ShadeCommand> {
    shape.flat_map(polygon_edges);

    std::iter::empty()
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
    RegionList::from(builder.filter_map(segment).map(|line| (line, 1))).shade_commands(sample_depth)
}
