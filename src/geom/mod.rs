pub mod poly;
pub mod ellipse;
pub mod point;


pub use self::point::*;
use errors::Result;
use raster::{Tessellate, Tessellation};
use shaders::Shader;

#[derive(Debug, Clone)]
pub enum Geometry {
    Poly(poly::Poly),
    Ellipse(ellipse::Ellipse),
}

impl From<poly::Poly> for Geometry {
    fn from(poly: poly::Poly) -> Geometry { Geometry::Poly(poly) }
}

impl From<ellipse::Ellipse> for Geometry {
    fn from(ellipse: ellipse::Ellipse) -> Geometry { Geometry::Ellipse(ellipse) }
}

impl Tessellate for Geometry {
    fn tessellate(&self, shader: &Shader) -> Result<Tessellation> {
        match *self {
            Geometry::Poly(ref poly) => poly.tessellate(shader),
            Geometry::Ellipse(ref ellipse) => ellipse.tessellate(shader),
        }
    }
}
