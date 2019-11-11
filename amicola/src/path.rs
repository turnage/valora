//! Geometry primitives.

use crate::V2;
use std::iter::FromIterator;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Segment {
    MoveTo(V2),
    LineTo(V2),
    QuadraticTo { ctrl: V2, end: V2 },
    CubicTo { ctrl0: V2, ctrl1: V2, end: V2 },
}

impl Segment {
    fn end(&self) -> V2 {
        match *self {
            Segment::MoveTo(p) => p,
            Segment::LineTo(p) => p,
            Segment::QuadraticTo { end, .. } => end,
            Segment::CubicTo { end, .. } => end,
        }
    }
}

/// A path is an input to the rasterizer, a series of vertices.
#[derive(Debug, PartialEq, Clone)]
pub struct Path {
    segments: Vec<Segment>,
}

#[derive(Debug)]
pub(crate) struct PathLink {
    pub position: V2,
    pub segment: Segment,
}

impl Path {
    pub fn segments<'a>(&'a self) -> impl Iterator<Item = &'a Segment> + 'a { self.segments.iter() }

    // TODO: Make link iterator ergonomic.
    pub(crate) fn links<'a>(&'a self) -> impl Iterator<Item = PathLink> + 'a {
        let mut start_position = self
            .segments
            .first()
            .map(Segment::end)
            .unwrap_or(V2::new(0., 0.));

        std::iter::once(Segment::MoveTo(start_position))
            .chain(self.segments.iter().skip(1).copied())
            .map(move |segment| PathLink {
                position: {
                    let position = start_position;
                    start_position = segment.end();
                    position
                },
                segment,
            })
    }
}

impl FromIterator<Segment> for Path {
    fn from_iter<T: IntoIterator<Item = Segment>>(iter: T) -> Self {
        Self {
            segments: iter.into_iter().collect(),
        }
    }
}
