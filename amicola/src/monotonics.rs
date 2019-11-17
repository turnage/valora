//! Module for working with paths and path segments.

mod cubic_segment;
mod line_segment;
mod quadratic_segment;

use self::{
    cubic_segment::{CreateCubicResult, CubicBezier},
    line_segment::{LineSegment, RasterableLineSegment},
    quadratic_segment::{CreateQuadraticResult, QuadraticBezier},
};
use crate::{
    bounds::Bounds,
    path::{self, Path, PathLink},
    V2,
};
use enum_dispatch::enum_dispatch;
use std::convert::TryFrom;

pub struct RasterSegmentSet;

impl RasterSegmentSet {
    pub fn build_from_path(path: &Path) -> Vec<RasterSegment> {
        let mut segments = vec![];

        for PathLink {
            position: start,
            segment,
        } in path.links()
        {
            match segment {
                path::Segment::LineTo(end) => {
                    if let Some(line_segment) = RasterableLineSegment::new(start, end) {
                        segments.push(RasterSegment::from(line_segment));
                    }
                }
                path::Segment::QuadraticTo { ctrl, end } => {
                    match QuadraticBezier::new(start, ctrl, end) {
                        CreateQuadraticResult::IsLineSegment { start, end } => {
                            if let Some(line_segment) = RasterableLineSegment::new(start, end) {
                                segments.push(RasterSegment::from(line_segment));
                            }
                        }
                        CreateQuadraticResult::Quadratic(qs) => {
                            segments.extend(qs.into_iter().map(RasterSegment::from));
                        }
                    }
                }
                path::Segment::CubicTo { ctrl0, ctrl1, end } => {
                    match CubicBezier::new(start, ctrl0, ctrl1, end) {
                        CreateCubicResult::IsLineSegment { start, end } => {
                            if let Some(line_segment) = RasterableLineSegment::new(start, end) {
                                segments.push(RasterSegment::from(line_segment));
                            }
                        }
                        CreateCubicResult::Cubic(cs) => {
                            segments.extend(cs.into_iter().map(RasterSegment::from));
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
pub enum RasterSegment {
    RasterableLineSegment(RasterableLineSegment),
    QuadraticSegment(QuadraticBezier),
    CubicSegment(CubicBezier),
}

impl From<Segment> for Option<RasterSegment> {
    fn from(segment: Segment) -> Option<RasterSegment> {
        match segment {
            Segment::LineSegment(line_segment) => RasterableLineSegment::try_from(line_segment)
                .ok()
                .map(RasterSegment::from),
            Segment::QuadraticSegment(q) => Some(RasterSegment::from(q)),
            Segment::CubicSegment(c) => Some(RasterSegment::from(c)),
        }
    }
}

pub struct SegmentSet;

impl SegmentSet {
    pub fn build_from_path(path: &Path) -> Vec<Segment> {
        let mut segments = vec![];

        for PathLink {
            position: start,
            segment,
        } in path.links()
        {
            match segment {
                path::Segment::LineTo(end) => {
                    segments.push(Segment::from(LineSegment::new(start, end)));
                }
                path::Segment::QuadraticTo { ctrl, end } => {
                    match QuadraticBezier::new(start, ctrl, end) {
                        CreateQuadraticResult::IsLineSegment { start, end } => {
                            segments.push(Segment::from(LineSegment::new(start, end)));
                        }
                        CreateQuadraticResult::Quadratic(qs) => {
                            segments.extend(qs.into_iter().map(Segment::from));
                        }
                    }
                }
                path::Segment::CubicTo { ctrl0, ctrl1, end } => {
                    match CubicBezier::new(start, ctrl0, ctrl1, end) {
                        CreateCubicResult::IsLineSegment { start, end } => {
                            segments.push(Segment::from(LineSegment::new(start, end)));
                        }
                        CreateCubicResult::Cubic(cs) => {
                            segments.extend(cs.into_iter().map(Segment::from));
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
