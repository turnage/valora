use geom::{Ellipse, Instancer, Line, Point, Spawner, spawn};
use shaders::Shader;
use sketch::*;

pub struct DottedLine {
    shader: Shader,
    line: Line,
    spawner: Box<Spawner<Ellipse>>,
}

impl DottedLine {
    pub fn new(line: Line, shader: Shader, size: f32) -> Self {
        Self {
            line,
            shader,
            spawner: Box::new(Instancer::new(Ellipse::circle(Point::center(), size, 0.0))),
        }
    }
}

impl Sketch for DottedLine {
    fn draw(&self, _ctx: &SketchContext) -> Result<Canvas> {
        let mut canvas = Canvas::new();
        for dot in spawn(self.spawner.as_ref(), &self.line) {
            canvas.draw(self.shader.clone(), &dot)?;
        }
        Ok(canvas)
    }
}