use color::Colorer;
use errors::Result;
use geom::Scale;
use std::rc::Rc;
use tessellation::Tessellate;

#[derive(Clone)]
pub struct Mesh<T: Tessellate + Clone> {
    pub src: T,
    pub colorer: Colorer,
}

impl<T: Tessellate + Clone + Scale> Scale for Mesh<T> {
    fn scale(self, scale: f32) -> Self { Self { src: self.src.scale(scale), ..self } }
}