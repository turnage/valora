use actors::{WalkHeuristic, Walker};
use errors::Result;
use geom::{Line, Point, Rect, SubdivideEdges};
use palette::Colora;
use properties::clipping::Bounded;
use rand::Rng;
use sketch::*;

pub struct DebugWalk(Walker);

impl Sketch for DebugWalk {
    fn draw(&self, ctx: &SketchContext) -> Result<Canvas> {
        let canvases = self.0
            .edges()
            .into_iter()
            .map(|(start, end)| Line::new(start, end).subdivide_edges_n(4))
            .map(|line| {
                     DottedLine::new(line, Shader::constant(Colora::rgb(1.0, 1.0, 0.0, 1.0)), 0.02)
                 })
            .map(|dl| dl.draw(ctx))
            .collect::<Result<Vec<Canvas>>>()?;
        Ok(canvases
               .into_iter()
               .fold(Canvas::new(), |bg, fg| fg.atop(bg)))
    }
    fn step(self,
            ctx: &SketchContext,
            _events: Vec<glium::glutin::WindowEvent>)
            -> Result<Option<Self>> {
        Ok(Some(DebugWalk(self.0.walk(1, &mut ctx.rng.clone()))))
    }
}

impl Seed for DebugWalk {
    fn seed(_ctx: &SketchContext) -> Result<Self> {
        let walker = Walker::new(Point { x: 0.0, y: 0.0 },
                                 WalkHeuristic::Greedy(Box::new(|point, rng| {
                                                                    (0..(rng.gen_range(1, 4)))
                .into_iter()
                .map(|_| point + (rng.gen::<Point>() / 7.0))
                .filter(|p| Rect::frame().in_bounds(*p))
                .collect()
                                                                })));
        Ok(DebugWalk(walker))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn looks_right() {
        sketch::<DebugWalk>(SketchCfg { size: 500, seed: None, root_frame_filename: None })
            .expect("working sketch");
    }
}