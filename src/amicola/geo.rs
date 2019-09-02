//! Geometry primitives.

use failure::Fail;
use nalgebra::{base::*, Matrix};
use std::convert::TryFrom;

pub type V2 = Matrix<f64, U2, U1, ArrayStorage<f64, U2, U1>>;
pub type V4 = Matrix<f64, U4, U1, ArrayStorage<f64, U4, U1>>;

#[derive(Debug, Fail, PartialEq)]
pub enum Error {
    #[fail(display = "Too few vertices to make polygon: _0")]
    TooFewVerticesForPolygon(usize),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Polygon {
    vertices: Vec<V2>,
}

impl Polygon {
    pub fn edges<'a>(&'a self) -> impl Iterator<Item = (&'a V2, &'a V2)> + 'a {
        let wrap_around = self
            .vertices
            .iter()
            .skip(self.vertices.len() - 1)
            .zip(self.vertices.iter().take(1));
        self.vertices
            .iter()
            .zip(self.vertices.iter().skip(1))
            .chain(wrap_around)
    }

    pub fn vertices(&self) -> &[V2] {
        &self.vertices
    }
}

impl TryFrom<Vec<V2>> for Polygon {
    type Error = Error;

    fn try_from(vertices: Vec<V2>) -> Result<Self, Self::Error> {
        if vertices.len() < 3 {
            Err(Error::TooFewVerticesForPolygon(vertices.len()))?;
        }

        Ok(Polygon { vertices })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn polygon_try_from_vec_v2_invalid() {
        assert_eq!(
            Polygon::try_from(vec![]),
            Err(Error::TooFewVerticesForPolygon(0))
        );
    }

    #[test]
    fn polygon_try_from_vec_v2_valid() {
        let expected_vertices = vec![V2::new(1.0, 1.0), V2::new(1.0, 1.0), V2::new(1.0, 1.0)];
        assert_eq!(
            Polygon::try_from(expected_vertices.clone()),
            Ok(Polygon {
                vertices: expected_vertices
            })
        );
    }

    #[test]
    fn polygon_edges() {
        let polygon = Polygon {
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
