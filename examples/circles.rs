extern crate rand;
extern crate valora;

use rand::Rng;
use valora::element::*;
use valora::errors::*;
use valora::geom::*;
use valora::palette::*;
use valora::shaders::*;
use valora::sketch::*;

pub struct Circles {
    count: usize,
    circles: Vec<(Shader, Ellipse)>,
}

impl Circles {
    pub fn new(count: usize) -> Self {
        let mut rng = rand::OsRng::new().unwrap();
        let shaders = (0..count)
            .into_iter()
            .map(|_| {
                Shader::linear(|point| {
                                   Colora::hsv(RgbHue::from(220.0 +
                                                            Point::center().distance(point) * 50.0),
                                               1.0,
                                               1.0,
                                               1.0)
                               })
            })
            .collect::<Vec<Shader>>();
        Circles {
            count,
            circles: shaders
                .into_iter()
                .zip((0..count)
                         .into_iter()
                         .map(|_| rand::random::<Point>())
                         .map(|p| Ellipse::circle(p, rng.gen_range(0.1, 0.3), 0.0)))
                .collect(),
        }
    }
}

impl Sketch for Circles {
    fn draw<'a>(&'a self, _ctx: &SketchContext) -> Result<Box<Iterator<Item = Element<'a>> + 'a>> {
        Ok(Box::new(self.circles.iter().map(Into::into)))
    }
}

impl Seed for Circles {
    fn seed(self, _ctx: &SketchContext) -> Result<Circles> { Ok(Circles::new(self.count)) }
}

fn main() {
    sketch(SketchCfg { size: 700, seed: None, root_frame_filename: None }, Circles::new(50))
        .expect("working sketch");
}