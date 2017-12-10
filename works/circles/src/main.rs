extern crate rand;
extern crate valora;

use rand::Rng;

use std::error::Error;
use valora::geom::*;
use valora::sketch::*;

pub struct Circles {}

impl Sketch for Circles {
    fn draw(&self, ctx: &SketchContext) -> Result<Vec<Element>, String> {
        let mut rng = rand::OsRng::new().map_err(
            |e| format!("{}", e.description()),
        )?;
        Ok(
            (0..100)
                .into_iter()
                .map(|_| Point { x: rng.gen_range(0.0, 0.9), y: rng.gen_range(0.0, 0.9) })
                .map(|p| Element::Poly(shapes::square(p, 0.1)))
                .collect(),
        )
    }
}

fn main() { sketch(SketchCfg { size: 500 }, Circles {}).expect("working sketch"); }
