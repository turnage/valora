use errors::Result;
use geom::ellipse::Ellipse;
use geom::poly::Poly;
use raster::{Tessellate, Tessellation};
use shaders::Shader;

pub enum Element {
    Poly(Poly),
    Ellipse(Ellipse),
}

impl Tessellate for Element {
    fn tessellate(self, shader: Shader) -> Result<Tessellation> {
        match self {
            Element::Poly(poly) => poly.tessellate(shader),
            Element::Ellipse(ellipse) => ellipse.tessellate(shader),
        }
    }
}
