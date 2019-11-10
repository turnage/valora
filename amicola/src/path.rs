//! Geometry primitives.

use crate::V2;
use std::iter::FromIterator;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Segment {
    MoveTo(V2),
    LineTo(V2),
}

/// A path is an input to the rasterizer, a series of vertices.
#[derive(Debug, PartialEq, Clone)]
pub struct Path {
    segments: Vec<Segment>,
}

impl Path {
    pub fn links<'a>(&'a self) -> impl Iterator<Item = (Segment, Segment)> + 'a {
        self.segments
            .iter()
            .copied()
            .zip(self.segments.iter().skip(1).copied())
    }
}

impl FromIterator<Segment> for Path {
    fn from_iter<T: IntoIterator<Item = Segment>>(iter: T) -> Self {
        Self {
            segments: iter.into_iter().collect(),
        }
    }
}
