extern crate valora;

use valora::*;
use valora::noise::*;
use valora::rand::{Rng, StdRng};
use valora::palette::{Colora, RgbHue};
use std::rc::Rc;

struct Noise {
    quality: u32,
}

impl Sketch for Noise {
    fn sketch(&self, cfg: &SketchCfg, mut rng: StdRng) -> Result<Composition> {
        let fbm: Fbm<f32> = Fbm::new().set_seed(rng.gen());
        let fbm2: Fbm<f32> = Fbm::new().set_seed(rng.gen());
        let orders: Vec<([[f32; 3]; 3], f32)> = (0..(rng.gen_range(1, 4))).into_iter().map(|i| ([
            [
                rng.gen_range(0.0, 10.0),
                rng.gen_range(0.0, 10.0),
                rng.gen_range(0.0, 10.0)
            ],
            [
                rng.gen_range(0.0, 10.0),
                rng.gen_range(0.0, 10.0),
                rng.gen_range(0.0, 10.0)
            ],
            [
                rng.gen_range(0.0, 10.0),
                rng.gen_range(0.0, 10.0),
                rng.gen_range(0.0, 10.0)
            ],
        ], rng.gen_range(0.0, 1.0))).collect();
        let image =
            image::ImageBuffer::from_fn(cfg.size * self.quality, cfg.size * self.quality, move |x, y| {
                let sample = |x, y, d| {
                    let x = x as f32 / (cfg.size * self.quality) as f32;
                    let y = y as f32 / (cfg.size * self.quality) as f32;
                    let mut p = [x, y, d];
                    for &(ref offset, q_weight) in &orders {
                        p = [
                            p[0] * (1.0 - q_weight) + q_weight * fbm.get([
                                    p[0] + offset[0][0],
                                    p[1] + offset[0][1],
                                    p[2] + offset[0][2]
                            ]),
                            p[1] * (1.0 - q_weight) + q_weight * fbm.get([
                                    p[0] + offset[1][0],
                                    p[1] + offset[1][1],
                                    p[2] + offset[1][2]
                            ]),
                            p[2] * (1.0 - q_weight) + q_weight * fbm.get([
                                    p[0] + offset[2][0],
                                    p[1] + offset[2][1],
                                    p[2] + offset[2][2]
                            ])
                        ];
                    }
                    fbm.get(p)
                };
                valora::color::conversions::collapse({
                    palette::Rgb::new(sample(x, y, 1.0), sample(x, y, 2.0), sample(x, y, 3.0))
                })
            });
        Ok(Composition::new()
            .solid_layer(Colorer::from(Colora::hsv(
                RgbHue::from(49.0),
                0.2,
                1.0,
                1.0,
            )))
            .add(image)
            .add(Mesh::from(Ellipse::circle(Point::center(), 0.2)).with_colorer(Colorer::black()).with_blend_mode(BlendMode::Normal)))
    }
}

fn main() {
    sketch(
        SketchCfg {
            size: 400,
            quality: 4,
            ..SketchCfg::default()
        },
        Noise { quality: 4 },
    ).expect("noise");
}
