//! Geometry primitives.

use nalgebra::{base::*, Matrix};
use std::iter::FromIterator;

pub type V2 = Matrix<f32, U2, U1, ArrayStorage<f32, U2, U1>>;
pub type V4 = Matrix<f32, U4, U1, ArrayStorage<f32, U4, U1>>;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PathSegment {
    MoveTo(V2),
    LineTo(V2),
}

/// A path is an input to the rasterizer, a series of vertices.
#[derive(Debug, PartialEq, Clone)]
pub struct Path {
    segments: Vec<PathSegment>,
}

impl Path {
    pub fn links<'a>(&'a self) -> impl Iterator<Item = (PathSegment, PathSegment)> + 'a {
        self.segments
            .iter()
            .copied()
            .zip(self.segments.iter().skip(1).copied())
    }
}

impl FromIterator<PathSegment> for Path {
    fn from_iter<T: IntoIterator<Item = PathSegment>>(iter: T) -> Self {
        Self {
            segments: iter.into_iter().collect(),
        }
    }
}
