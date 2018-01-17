extern crate valora;

use valora::*;
use valora::noise::*;
use valora::rand::{Rng, StdRng};
use valora::palette::{Colora, RgbHue};
use std::rc::Rc;

const QUALITY: u32 = 1;

struct Noise {
    quality: u32
}

impl Sketch for Noise {
    fn sketch(&self, cfg: &SketchCfg, mut rng: StdRng) -> Result<Composition> {
        let fbm: Fbm<f32> = Fbm::new().set_seed(rng.gen());
        let image = image::ImageBuffer::from_fn(cfg.size * QUALITY, cfg.size * QUALITY, move |x, y| {
            let sample  = |x, y, d| fbm.get([x as f32 / (cfg.size * QUALITY) as f32, y as f32 / (cfg.size * QUALITY) as f32, d as f32]);
            valora::color::conversions::collapse({
                palette::Rgb::new(sample(x, y, 1), sample(x, y, 2), sample(x, y, 3))
            }) 
        });
        Ok(Composition::new()
            .solid_layer(Colorer::from(Colora::hsv(
                RgbHue::from(49.0),
                0.2,
                1.0,
                1.0,
            ))).add(image))
    }
}

fn main() {
    sketch(SketchCfg {
        size: 400,
        ..SketchCfg::default()
    }, Noise{
        quality: 1,
    }).expect("noise");
}
