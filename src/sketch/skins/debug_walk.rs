use actors::{WalkHeuristic, Walker};
use errors::Result;
use geom::{Line, Point, Rect, SubdivideEdges};
use palette::Colora;
use properties::clipping::Bounded;
use rand::Rng;
use sketch::*;

pub struct DebugWalk {
    walker: Walker,
    strands: Vec<Tweener<DottedLine>>,
}

impl Draw for DebugWalk {
    fn draw(&self, ctx: &SketchContext) -> Result<Canvas> {
        let canvases = self.strands
            .iter()
            .map(|dl| dl.draw(ctx))
            .collect::<Result<Vec<Canvas>>>()?;
        Ok(canvases
               .into_iter()
               .fold(Canvas::new(), |bg, fg| fg.atop(bg)))
    }
}

impl Step for DebugWalk {
    fn step(mut self,
            ctx: &SketchContext,
            rng: &mut StdRng,
            _events: Vec<glium::glutin::WindowEvent>)
            -> Result<Option<Self>> {
        println!("{:?}", self.walker.edges().len());
        Ok(Some(DebugWalk {
                    strands: if self.strands
                           .last()
                           .map(|s| s.done(ctx.frame))
                           .unwrap_or(false) ||
                                self.strands.is_empty() {
                        self.strands
                            .extend(self.walker
                                        .step(rng)
                                        .into_iter()
                                        .map(|(p1, p2)| {
                                                 DottedLine::new(Line::new(p1, p2),
                                                                 Shader::constant(Colora::rgb(1.0,
                                                                                              1.0,
                                                                                              0.0,
                                                                                              1.0)),
                                                                 0.02)
                                             })
                                        .map(|dl| dl.subdivide_edges_n(5))
                                        .map(|dl| {
                                                 dl.anim_percent(0.0,
                                                                 1.0,
                                                                 Interpolation::Linear {
                                                                     start: ctx.frame,
                                                                     len: 100,
                                                                 })
                                             }));
                        self.strands
                    } else {
                        self.strands
                    },
                    walker: self.walker,
                }))
    }
}

impl Seed for DebugWalk {
    fn seed(_ctx: &SketchContext) -> Result<Self> {
        let walker = Walker::new(Point { x: 0.0, y: 0.0 },
                                 WalkHeuristic::Greedy(Box::new(|point, rng| {
                                                                    (0..(rng.gen_range(1, 4)))
                .into_iter()
                .map(|_| point + (rng.gen::<Point>() / 2.0))
                .filter(|p| Rect::frame().in_bounds(*p))
                .collect()
                                                                })));
        Ok(DebugWalk { walker, strands: Vec::new() })
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