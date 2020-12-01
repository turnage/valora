//! A rasterizer for fine art.

mod boundary_spans;
mod grid_lines;
mod regions;
mod sampling;
mod stroke;
mod wind;

pub use geo_types::{CoordinateType, Line, LineString, MultiPolygon, Polygon};
pub use lyon_path::{iterator::Flattened, Builder, Event, Path, PathEvent};
pub use regions::ShadeCommand;
pub use sampling::SampleDepth;
pub type V2 = Point2D<f32, UnknownUnit>;

use euclid::{Point2D, UnknownUnit};
use itertools::Itertools;
use lyon_geom::{math::point, LineSegment};
use num_traits::{Num, NumCast};
use std::cmp::Ordering;

use regions::RegionList;

pub enum RasterInput {
    Fill(Polygon<f64>),
    Stroke(Stroke),
}

enum EdgeIter<F, S> {
    Fill(F),
    Stroke(S),
}

impl<F, S> Iterator for EdgeIter<F, S>
where
    F: Iterator<Item = LineSegment<f64>>,
    S: Iterator<Item = LineSegment<f64>>,
{
    type Item = LineSegment<f64>;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            EdgeIter::Fill(fill) => fill.next(),
            EdgeIter::Stroke(stroke) => stroke.next(),
        }
    }
}

impl RasterInput {
    fn edges(self) -> impl Iterator<Item = LineSegment<f64>> {
        match self {
            RasterInput::Fill(polygon) => EdgeIter::Fill(polygon_edges(polygon)),
            RasterInput::Stroke(stroke) => EdgeIter::Stroke(stroke::inflate(stroke)),
        }
    }
}

pub struct Stroke {
    pub width: f64,
    pub closed: bool,
    pub path: LineString<f64>,
}

impl From<Polygon<f64>> for RasterInput {
    fn from(polygon: Polygon<f64>) -> RasterInput {
        RasterInput::Fill(polygon)
    }
}

impl From<Stroke> for RasterInput {
    fn from(stroke: Stroke) -> RasterInput {
        RasterInput::Stroke(stroke)
    }
}

fn polygon_edges(polygon: Polygon<f64>) -> impl Iterator<Item = LineSegment<f64>> {
    let (exterior, interior) = polygon.into_inner();

    let positive_wind = 1;
    let exterior = line_string_lines(exterior);

    let negative_wind = -1;
    let interior = interior.into_iter().flat_map(line_string_lines);

    exterior.chain(interior)
}

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub(crate) struct Pixel {
    pub x: isize,
    pub y: isize,
}

impl PartialOrd for Pixel {
    fn partial_cmp(&self, other: &Pixel) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Pixel {
    fn cmp(&self, other: &Pixel) -> Ordering {
        match self.y.cmp(&other.y) {
            Ordering::Equal => self.x.cmp(&other.x),
            other => other,
        }
    }
}

pub(crate) fn line_string_lines(
    line_string: LineString<f64>,
) -> impl Iterator<Item = LineSegment<f64>> {
    line_string
        .into_iter()
        .tuple_windows::<(_, _)>()
        .map(move |(start, end)| LineSegment {
            from: point(start.x, start.y),
            to: point(end.x, end.y),
        })
}

pub fn raster(
    input: impl Into<RasterInput>,
    sample_depth: SampleDepth,
) -> impl Iterator<Item = ShadeCommand> {
    let input = input.into();
    let edges = input.edges();
    let regions = RegionList::from(edges);
    regions.shade_commands(sample_depth)
}
