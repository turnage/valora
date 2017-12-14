
extern crate rand;
extern crate valora;

use rand::Rng;
use valora::errors::*;
use valora::geom::*;
use valora::palette::*;
use valora::render::*;
use valora::shaders::*;
use valora::sketch::*;

pub struct Circles {
    count: usize,
    circles: Vec<(Shader, Vec<Geometry>)>,
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
            circles: (0..count)
                .into_iter()
                .map(|_| rand::random::<Point>())
                .map(|p| ellipse::Ellipse::circle(p, rng.gen_range(0.1, 0.3), 0.0))
                .zip(shaders.into_iter())
                .map(|(c, s)| (s, vec![Geometry::Ellipse(c)]))
                .collect(),
        }
    }
}

impl Sketch for Circles {
    fn draw(&self, _ctx: &SketchContext) -> Result<Render> {
        self.circles
            .iter()
            .fold(Ok(Render::new()), |r, g| r.and_then(|r| r.add(g)))
    }
}

impl Seed for Circles {
    fn seed(self, _ctx: &SketchContext) -> Result<Circles> { Ok(Circles::new(self.count)) }
}

fn main() {
    sketch(SketchCfg { size: 700, seed: None, root_frame_filename: None }, Circles::new(50))
        .expect("working sketch");
}