//! Module for working with paths and path segments.

mod line_segment;
mod quadratic_segment;

use self::{
    line_segment::LineSegment,
    quadratic_segment::{CreateQuadraticResult, QuadraticBezier},
};
use crate::{
    bounds::Bounds,
    path::{self, Path, PathLink},
    V2,
};
use enum_dispatch::enum_dispatch;

pub struct RasterSegmentSet;

impl RasterSegmentSet {
    pub fn build_from_path(path: &Path) -> Vec<Segment> {
        let mut segments = vec![];

        for PathLink {
            position: start,
            segment,
        } in path.links()
        {
            match segment {
                path::Segment::LineTo(end) => {
                    if let Some(line_segment) = LineSegment::new_rasterable(start, end) {
                        segments.push(Segment::from(line_segment));
                    }
                }
                path::Segment::QuadraticTo { ctrl, end } => {
                    match QuadraticBezier::new(start, ctrl, end) {
                        CreateQuadraticResult::IsLineSegment { start, end } => {
                            if let Some(line_segment) = LineSegment::new_rasterable(start, end) {
                                segments.push(Segment::from(line_segment));
                            }
                        }
                        CreateQuadraticResult::Quadratic(qs) => {
                            segments.extend(qs.into_iter().map(Segment::from));
                        }
                    }
                }
                path::Segment::MoveTo(_) => {}
            }
        }

        segments
    }
}

#[enum_dispatch]
#[derive(Debug)]
pub enum Segment {
    LineSegment(LineSegment),
    QuadraticSegment(QuadraticBezier),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Intersection {
    /// Where on the excluded axis (x or y) the intersection occurs.
    pub axis: f32,
    /// Where along the segment [0.0, 1.0] the intersection occurs.
    pub t: f32,
}

/// A trait for monotonic curves.
#[enum_dispatch(Segment)]
pub trait Curve {
    /// Returns the point at `t` on the curve, if it is defined. Curves are defined on
    /// the domain `[0, 1]` only.
    fn sample_t(&self, t: f32) -> Option<V2>;

    /// Returns the `y` and `t` coordinate where the curve intersects a vertical line at `x`.
    fn sample_x(&self, x: f32) -> Option<Intersection>;

    /// Returns the `x` and `t` coordinate where the curve intersects a horizontal line at `y`.
    fn sample_y(&self, y: f32) -> Option<Intersection>;

    /// Returns a bounding box for the curve.
    fn bounds(&self) -> &Bounds;
}
