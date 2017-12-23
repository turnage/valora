extern crate valora;

use palette::*;
use valora::*;

struct Circle {
    circle: Shaded<Tweener<Ellipse>>,
}

impl Draw for Circle {
    fn draw(&self, ctx: &SketchContext) -> Result<Canvas> {
        println!("printing {:?}", ctx.frame);
        self.circle.draw(ctx)
    }
}

impl Seed for Circle {
    fn seed(ctx: &SketchContext) -> Result<Self> {
        Ok(Self {
               circle: {
                   Shaded::color(Colora::rgb(1.0, 1.0, 0.0, 1.0),
                                 Tweener::id(Ellipse::circle(Point::center(), 0.3, 0.0))
                                     .anim_scale(1.1,
                                                 2.0,
                                                 Interpolation::Oscillation {
                                                     oscillation: Oscillation::Sine,
                                                     start: 0,
                                                     period: 100,
                                                 }))
               },
           })
    }
}

fn main() { sketch::<Circle>(SketchCfg { size: 500, root_frame_filename: None, seed: None }); }