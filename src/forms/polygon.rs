//! Polygon.

use crate::{
    Angle, Canvas, Center, Ellipse, FlatIterPath, Paint, Rect, Rotate, Scale, Subdivide, Translate,
    P2, V2,
};
use arrayvec::ArrayVec;
use itertools::Itertools;
use std::iter::{DoubleEndedIterator, FromIterator};

#[derive(Debug, Clone, PartialEq)]
pub struct Polygon {
    vertices: Vec<P2>,
}

impl Polygon {
    /// Returns an iterator over the polygon's vertices.
    pub fn vertices<'a>(&'a self) -> impl DoubleEndedIterator<Item = P2> + Clone + 'a {
        self.vertices.iter().copied()
    }

    /// Returns an iterator over unique references to the polgyon's vertices.
    pub fn vertices_mut<'a>(&'a mut self) -> impl DoubleEndedIterator<Item = &'a mut P2> + 'a {
        self.vertices.iter_mut()
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
        canvas.paint(FlatIterPath::new(self.vertices(), /*closed=*/ true))
    }
}

impl Paint for &Polygon {
    fn paint(&self, canvas: &mut Canvas) {
        (**self).paint(canvas);
    }
}

impl Subdivide for Polygon {
    fn subdivide(self) -> Self {
        let from_start = self.vertices();
        let skipped = self.vertices().skip(1).chain(self.vertices().take(1));

        from_start
            .zip(skipped)
            .flat_map(|(a, b)| {
                let midpoint = a + ((b + (a.to_vector() * -1.)) * 0.5).to_vector();
                ArrayVec::from([a, midpoint]).into_iter()
            })
            .collect::<Self>()
    }
}

impl Rotate for Polygon {
    fn rotate(mut self, pivot: P2, theta: Angle) -> Self {
        self.vertices_mut().for_each(|v| {
            *v = v.rotate(pivot, theta);
        });

        self
    }
}

impl Translate for Polygon {
    fn translate(mut self, translation: V2) -> Self {
        self.vertices.iter_mut().for_each(|p| *p += translation);
        self
    }
}

impl Center for Polygon {
    fn center(&self) -> P2 {
        Rect::extent(self.vertices()).center()
    }
}

impl Scale for Polygon {
    fn scale(mut self, factor: f32) -> Self {
        let scale_vertex_from = |anchor: P2, v: P2| -> P2 {
            let distance = (anchor - v).length();
            let phase = Ellipse::circle(anchor, distance).circumphase(&v);
            Ellipse::circle(anchor, distance * factor).circumpoint(phase)
        };

        let center = self.center();
        self.vertices
            .iter_mut()
            .for_each(|p| *p = scale_vertex_from(center, *p));
        self
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
