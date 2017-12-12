extern crate rand;
extern crate valora;

use rand::Rng;
use std::error::Error;
use valora::element::*;
use valora::geom::*;
use valora::palette::*;
use valora::shaders::*;
use valora::sketch::*;

pub struct Space {}

impl Sketch for Space {
    fn draw(&self, _ctx: &SketchContext) -> Result<Vec<(Shader, Element)>, String> {
        let mut rng = rand::OsRng::new()
            .map_err(|e| format!("{}", e.description()))?;
        let mut fill =
            || Colora::hsv(RgbHue::from(220.0 + rng.gen_range(-20.0, 20.0)), 1.0, 1.0, 1.0);
        let size = 0.1;
        Ok((0..100)
               .into_iter()
               .map(|_| rand::random::<Point>())
               .map(|p| {
                        vec![Point { x: p.x + size / 2.0, y: p.y },
                             Point { x: p.x - size / 2.0, y: p.y },
                             Point { x: 0.5, y: 0.5 }]
                    })
               .map(|ps| (Shader::constant(fill()), Element::Poly(poly::Poly::Irregular(ps))))
               .collect())
    }
}

fn main() {
    sketch(SketchCfg { size: 700, root_frame_filename: None }, Space {}).expect("working sketch");
}
