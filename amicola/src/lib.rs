//! A rasterizer for fine art.

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

fn line_string_lines<T: CoordinateType>(
    line_string: LineString<T>,
) -> impl Iterator<Item = LineSegment<f64>> {
    let to_f64 = |coordinate: T| -> f64 { NumCast::from(coordinate).expect("NumCast") };

    line_string
        .into_iter()
        .tuple_windows::<(_, _)>()
        .map(move |(start, end)| LineSegment {
            from: point(to_f64(start.x), to_f64(start.y)),
            to: point(to_f64(end.x), to_f64(end.y)),
        })
}

pub(crate) fn polygon_edges<T: CoordinateType>(
    polygon: Polygon<T>,
) -> impl Iterator<Item = (LineSegment<f64>, i32)> {
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
    RegionList::from(shape.flat_map(polygon_edges)).shade_commands(sample_depth)
}
