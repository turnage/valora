//! Geometry primitives.

use failure::Fail;
use nalgebra::Vector2;
use std::convert::TryFrom;

pub type V2 = Vector2<f64>;

#[derive(Debug, Fail)]
pub enum Error {
    #[fail(display = "Too few vertices to make polygon: _0")]
    TooFewVerticesForPolygon(usize),
}

pub struct Polygon {
    vertices: Vec<V2>,
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
