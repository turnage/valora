//! Polygon.

use crate::{Canvas, ClosedPath, FlatIterPath, Paint, Subdivide, P2};
use arrayvec::ArrayVec;
use itertools::Itertools;
use std::iter::{DoubleEndedIterator, FromIterator};

#[derive(Debug, Clone, PartialEq)]
pub struct Polygon {
    vertices: Vec<P2>,
}

impl Polygon {
    pub fn vertices<'a>(&'a self) -> impl DoubleEndedIterator<Item = P2> + Clone + 'a {
        self.vertices.iter().copied()
    }

    /// Returns an iterator over each vertex in the form (left neighbor, vertex, right neighbor).
    pub fn vertices_with_neighbors<'a>(&'a self) -> impl Iterator<Item = (P2, P2, P2)> + 'a {
        let last_iter = self.vertices().rev().take(1);
        last_iter
            .clone()
            .chain(self.vertices())
            .chain(last_iter)
            .tuple_windows()
    }
}

impl Paint for Polygon {
    fn paint(&self, canvas: &mut Canvas) {
        canvas.paint(ClosedPath::from(FlatIterPath::from(self.vertices())))
    }
}

impl Paint for &Polygon {
    fn paint(&self, canvas: &mut Canvas) { (**self).paint(canvas); }
}

impl Subdivide for Polygon {
    fn subdivide(self) -> Self {
        let from_start = self.vertices();
        let skipped = self.vertices().skip(1).chain(self.vertices().rev().take(1));

        from_start
            .zip(skipped)
            .flat_map(|(a, b)| {
                let midpoint = a + ((b + (a.to_vector() * -1.)) * 0.5).to_vector();
                ArrayVec::from([a, midpoint]).into_iter()
            })
            .collect::<Self>()
    }
}

impl<I> From<I> for Polygon
where
    I: Iterator<Item = P2>,
{
    fn from(src: I) -> Self {
        Self {
            vertices: src.collect(),
        }
    }
}

impl FromIterator<P2> for Polygon {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = P2>,
    {
        Self {
            vertices: iter.into_iter().collect(),
        }
    }
}
