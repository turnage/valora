use errors::Result;
use palette::Colora;
use raster::Tessellate;
use shaders::Shader;
use sketch::{Canvas, Draw, SketchContext};
use std::{ops::Deref, rc::Rc};
use animation::Tweener;

#[derive(Clone)]
pub struct Shaded<T> {
    t: T,
    shader: Rc<Shader>,
}

impl<T> Shaded<T> {
    pub fn color(color: Colora, t: T) -> Self {
        Self { t, shader: Rc::new(Shader::constant(color)) }
    }

    pub fn shade(shader: Rc<Shader>, t: T) -> Self { Self { t, shader } }
}

impl<T: 'static + Clone + Tessellate> Draw for Shaded<Tweener<T>> {
    fn draw(&self, ctx: &SketchContext) -> Result<Canvas> {
        Canvas::new().draw(self.shader.clone(), &self.t.tween(ctx.frame))
    }
}