
extern crate rand;
extern crate valora;

use rand::Rng;
use std::error::Error;
use valora::element::*;
use valora::geom::*;
use valora::palette::*;
use valora::shaders::*;
use valora::sketch::*;

pub struct Circles {
    count: usize,
}

impl Sketch for Circles {
    fn draw(&self, _ctx: &SketchContext) -> Result<Vec<(Shader, Element)>, String> {
        let mut rng = rand::OsRng::new()
            .map_err(|e| format!("{}", e.description()))?;
        let shaders = (0..(self.count))
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
        Ok((0..(self.count))
               .into_iter()
               .map(|_| rand::random::<Point>())
               .map(|p| ellipse::Ellipse::circle(p, rng.gen_range(0.1, 0.3), 0.0))
               .zip(shaders.into_iter())
               .map(|(c, s)| (s, Element::Ellipse(c)))
               .collect())
    }
}

fn main() {
    sketch(SketchCfg { size: 700, root_frame_filename: None }, Circles { count: 50 })
        .expect("working sketch");
}