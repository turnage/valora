//! Polygon.

use crate::{Subdivide, P2};
use arrayvec::ArrayVec;
use std::iter::{DoubleEndedIterator, FromIterator};

#[derive(Debug, Clone, PartialEq)]
pub struct Polygon {
    vertices: Vec<P2>,
}

impl Polygon {
    pub fn vertices<'a>(&'a self) -> impl DoubleEndedIterator<Item = P2> + 'a {
        self.vertices.iter().copied()
    }
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
