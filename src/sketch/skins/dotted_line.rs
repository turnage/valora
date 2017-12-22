use geom::{Ellipse, Instancer, Line, Percent, Point, SubdivideEdges, spawn};
use shaders::Shader;
use sketch::*;

#[derive(Clone)]
pub struct DottedLine {
    shader: Shader,
    pub line: Line,
    spawner: Instancer<Ellipse>,
}

impl Percent for DottedLine {
    fn percent(self, percent: f32) -> Self { Self { line: self.line.percent(percent), ..self } }
}

impl SubdivideEdges for DottedLine {
    fn subdivide_edges(self) -> Self { Self { line: self.line.subdivide_edges(), ..self } }
}

impl DottedLine {
    pub fn new(line: Line, shader: Shader, size: f32) -> Self {
        Self { line, shader, spawner: Instancer::new(Ellipse::circle(Point::center(), size, 0.0)) }
    }
}

impl Draw for DottedLine {
    fn draw(&self, _ctx: &SketchContext) -> Result<Canvas> {
        let mut canvas = Canvas::new();
        for dot in spawn(&self.spawner, &self.line) {
            canvas.draw(self.shader.clone(), &dot)?;
        }
        Ok(canvas)
    }
}