extern crate rand;
extern crate valora;

use rand::Rng;

use std::error::Error;
use valora::geom::*;
use valora::sketch::*;
use valora::shaders::*;
use valora::palette::*;
use valora::element::*;

pub struct Circles {}

impl Sketch for Circles {
    fn draw(&self, _ctx: &SketchContext) -> Result<Vec<(Shader, Element)>, String> {
        let mut rng = rand::OsRng::new().map_err(|e| format!("{}", e.description()))?;
        let fill = Colora::hsv(RgbHue::from(220.0), 1.0, 1.0, 1.0);
        Ok((0..100)
            .into_iter()
            .map(|_| {
                Point {
                    x: rng.gen_range(0.0, 0.9),
                    y: rng.gen_range(0.0, 0.9),
                }
            })
            .map(|p| {
                (
                    Shader::constant(fill),
                    Element::Poly(poly::Poly::square(p, 0.1)),
                )
            })
            .collect())
    }
}

fn main() {
    sketch(SketchCfg { size: 700 }, Circles {}).expect("working sketch");
}
