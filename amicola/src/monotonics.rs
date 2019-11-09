//! Module for working with paths and path segments.

mod line_segment;

use self::line_segment::LineSegment;
use crate::{bounds::Bounds, path::PathSegment, V2};
use enum_dispatch::enum_dispatch;

#[enum_dispatch]
#[derive(Debug)]
pub enum MonotonicSegment {
    LineSegment(LineSegment),
}

impl MonotonicSegment {
    pub fn from_link(link: (PathSegment, PathSegment)) -> Vec<MonotonicSegment> {
        match link {
            (PathSegment::MoveTo(start), PathSegment::LineTo(end))
            | (PathSegment::LineTo(start), PathSegment::LineTo(end)) => {
                LineSegment::new_rasterable(start, end)
                    .map(|ls| vec![Self::from(ls)])
                    .unwrap_or_default()
            }
            (_, PathSegment::MoveTo(_)) => vec![],
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Intersection {
    /// Where on the excluded axis (x or y) the intersection occurs.
    pub axis: f32,
    /// Where along the segment [0.0, 1.0] the intersection occurs.
    pub t: f32,
}

/// A trait for monotonic curves.
#[enum_dispatch(MonotonicSegment)]
pub trait MonotonicCurve {
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
