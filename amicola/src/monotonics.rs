//! Module for working with paths and path segments.

mod cubic_segment;
mod line_segment;
mod quadratic_segment;

use self::{
    cubic_segment::{CreateCubicResult, CubicBezier},
    line_segment::{LineSegment, RasterableLineSegment},
    quadratic_segment::{CreateQuadraticResult, QuadraticBezier},
};
use crate::{bounds::Bounds, PathIterator, V2};
use enum_dispatch::enum_dispatch;
use lyon_path::PathEvent;
use std::convert::TryFrom;

pub struct RasterSegmentSet;

impl RasterSegmentSet {
    pub fn build_from_path(path: impl PathIterator) -> Vec<RasterSegment> {
        let mut segments = vec![];

        for event in path {
            match event {
                PathEvent::Line(line_segment) => {
                    if let Some(line_segment) = RasterableLineSegment::new(
                        V2::new(line_segment.from.x, line_segment.from.y),
                        V2::new(line_segment.to.x, line_segment.to.y),
                    ) {
                        segments.push(RasterSegment::from(line_segment));
                    }
                }
                PathEvent::Quadratic(q) => match QuadraticBezier::new(
                    V2::new(q.from.x, q.from.y),
                    V2::new(q.ctrl.x, q.ctrl.y),
                    V2::new(q.to.x, q.to.y),
                ) {
                    CreateQuadraticResult::IsLineSegment { start, end } => {
                        if let Some(line_segment) = RasterableLineSegment::new(start, end) {
                            segments.push(RasterSegment::from(line_segment));
                        }
                    }
                    CreateQuadraticResult::Quadratic(qs) => {
                        segments.extend(qs.into_iter().map(RasterSegment::from));
                    }
                },
                PathEvent::Cubic(c) => match CubicBezier::new(
                    V2::new(c.from.x, c.from.y),
                    V2::new(c.ctrl1.x, c.ctrl1.y),
                    V2::new(c.ctrl2.x, c.ctrl2.y),
                    V2::new(c.to.x, c.to.y),
                ) {
                    CreateCubicResult::IsLineSegment { start, end } => {
                        if let Some(line_segment) = RasterableLineSegment::new(start, end) {
                            segments.push(RasterSegment::from(line_segment));
                        }
                    }
                    CreateCubicResult::Cubic(cs) => {
                        segments.extend(cs.into_iter().map(RasterSegment::from));
                    }
                },
                _ => {}
            }
        }

        segments
    }
}

#[enum_dispatch]
#[derive(Debug)]
pub enum RasterSegment {
    RasterableLineSegment(RasterableLineSegment),
    QuadraticSegment(QuadraticBezier),
    CubicSegment(CubicBezier),
}

#[enum_dispatch]
#[derive(Debug)]
pub enum Segment {
    LineSegment(LineSegment),
    QuadraticSegment(QuadraticBezier),
    CubicSegment(CubicBezier),
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
#[enum_dispatch(RasterSegment)]
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

    /// Returns the start and end of the curve.
    fn bookends(&self) -> (V2, V2);
}
