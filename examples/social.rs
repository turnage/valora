extern crate rand;
extern crate valora;

use valora::errors::*;
use valora::geom::*;
use valora::palette::*;
use valora::raster::*;
use valora::shaders::*;
use valora::sketch::*;

struct Chain {
    segments: Vec<Line>,
    head: (Point, Point),
}

impl Chain {
    pub fn new(start: Point) -> Self { Self { head: (start, start), segments: Vec::new() } }

    pub fn grow_head(&mut self, end: Point) { self.head.1 = end; }

    pub fn bend(&mut self, start: Point) {
        self.segments.push(Line::new(self.head.0, self.head.1));
        self.head = (start, start);
    }
}

struct Social {
    line: Line,
    dotSpawner: Box<Spawner<Ellipse>>,
    dotShader: Shader,
}

impl Social {
    pub fn new() -> Self {
        Self {
            line: Line::new(Point::center(), Point { x: 0.0, y: 0.0 }).subdivides_edges_n(3),
            dotSpawner: Box::new(Instancer::new(Ellipse::circle(Point::center(), 0.01, 0.0))),
            dotShader: Shader::constant(Colora::rgb(1.0, 0.0, 0.0, 1.0)),
        }
    }
}

impl Sketch for Social {
    fn draw(&self, _ctx: &SketchContext) -> Result<Canvas> {
        let mut canvas = Canvas::new();
        for dot in spawn(self.dotSpawner.as_ref(), &self.line) {
            canvas.draw(&self.dotShader, &dot)?;
        }
        Ok(canvas)
    }
}

impl Seed for Social {
    fn seed(self, _ctx: &SketchContext) -> Result<Social> { Ok(Social::new()) }
}

fn main() {
    sketch(SketchCfg { size: 700, seed: None, root_frame_filename: None }, Social::new())
        .expect("working sketch");
}