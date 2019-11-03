//! Geometry primitives.

use nalgebra::{base::*, Matrix};

pub type V2 = Matrix<f32, U2, U1, ArrayStorage<f32, U2, U1>>;
pub type V4 = Matrix<f32, U4, U1, ArrayStorage<f32, U4, U1>>;

/// A path is an input to the rasterizer, a series of vertices.
#[derive(Debug, PartialEq, Clone)]
pub struct Path {
    vertices: Vec<V2>,
}

impl Path {
    /// Returns an iterator over edges between the vertices of the path.
    pub fn edges<'a>(&'a self) -> impl Iterator<Item = (&'a V2, &'a V2)> + 'a {
        self.vertices().iter().zip(self.vertices().iter().skip(1))
    }

    /// Returns an iterator over edges between the vertices of the path, including an inferred
    /// edge between the last and vertex, to close the path.
    pub fn edges_wrapped<'a>(&'a self) -> impl Iterator<Item = (&'a V2, &'a V2)> + 'a {
        let wrap_around = self
            .vertices
            .iter()
            .skip(self.vertices.len() - 1)
            .zip(self.vertices.iter().take(1));
        self.edges().chain(wrap_around)
    }

    pub fn vertices(&self) -> &[V2] { &self.vertices }
}

impl From<Vec<V2>> for Path {
    fn from(vertices: Vec<V2>) -> Self { Path { vertices } }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn polygon_edges() {
        let polygon = Path {
            vertices: vec![V2::new(0.0, 0.0), V2::new(0.0, 1.0), V2::new(1.0, 0.0)],
        };
        assert_eq!(
            polygon.edges().collect::<Vec<(&V2, &V2)>>(),
            vec![
                (&V2::new(0.0, 0.0), &V2::new(0.0, 1.0)),
                (&V2::new(0.0, 1.0), &V2::new(1.0, 0.0)),
                (&V2::new(1.0, 0.0), &V2::new(0.0, 0.0))
            ]
        );
    }
}
