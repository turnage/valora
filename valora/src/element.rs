use geom::poly::Poly;
use raster::{Tessellate, Tessellation};
use errors::Result;
use shaders::Shader;

pub enum Element {
    Poly(Poly),
}

impl Tessellate for Element {
    fn tessellate(self, shader: Shader) -> Result<Tessellation> {
        match self {
            Element::Poly(poly) => poly.tessellate(shader),
        }
    }
}
