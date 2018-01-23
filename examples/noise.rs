extern crate rayon;
extern crate valora;

use valora::*;
use valora::glossy;
use valora::noise::*;
use valora::rand::{Rng, StdRng};
use valora::palette::{Colora, RgbHue};
use valora::image::Pixel;
use std::rc::Rc;
use rayon::prelude::*;

struct Noise {
    quality: u32,
}

impl Sketch for Noise {
    fn sketch(&self, ctx: &SketchContext, mut rng: StdRng) -> Result<Composition> {
        let fbm: Fbm<f32> = Fbm::new().set_seed(rng.gen());
        let fbm2: Fbm<f32> = Fbm::new().set_seed(rng.gen());
        let orders: Vec<([[f32; 3]; 3], f32)> = (0..(rng.gen_range(1, 4)))
            .into_iter()
            .map(|i| {
                (
                    [
                        [
                            rng.gen_range(0.0, 10.0),
                            rng.gen_range(0.0, 10.0),
                            rng.gen_range(0.0, 10.0),
                        ],
                        [
                            rng.gen_range(0.0, 10.0),
                            rng.gen_range(0.0, 10.0),
                            rng.gen_range(0.0, 10.0),
                        ],
                        [
                            rng.gen_range(0.0, 10.0),
                            rng.gen_range(0.0, 10.0),
                            rng.gen_range(0.0, 10.0),
                        ],
                    ],
                    rng.gen_range(0.0, 1.0),
                )
            })
            .collect();
        let size = ctx.cfg.size * self.quality;
        let sample = |x, y, d| {
            let x = x as f32 / size as f32;
            let y = y as f32 / size as f32;
            let mut p = [x, y, d];
            for &(ref offset, q_weight) in &orders {
                p = [
                    p[0] * (1.0 - q_weight)
                        + q_weight
                            * fbm.get([
                                p[0] + offset[0][0],
                                p[1] + offset[0][1],
                                p[2] + offset[0][2],
                            ]),
                    p[1] * (1.0 - q_weight)
                        + q_weight
                            * fbm.get([
                                p[0] + offset[1][0],
                                p[1] + offset[1][1],
                                p[2] + offset[1][2],
                            ]),
                    p[2] * (1.0 - q_weight)
                        + q_weight
                            * fbm.get([
                                p[0] + offset[2][0],
                                p[1] + offset[2][1],
                                p[2] + offset[2][2],
                            ]),
                ];
            }
            fbm.get(p)
        };
        let image_src = (0..(size * size))
            .into_par_iter()
            .flat_map(|i| {
                let (x, y) = (i % size, i / size);
                valora::color::conversions::collapse({
                    palette::Rgb::new(sample(x, y, 1.0), sample(x, y, 2.0), sample(x, y, 3.0))
                }).channels()
                    .to_vec()
            })
            .collect();
        let image = image::ImageBuffer::from_vec(size, size, image_src).unwrap();
        Ok(Composition::new()
            .solid_layer(Colorer::white())
            .add(image)
            .add(
                sparkles(rng.gen_range(0, 4), &Rect::frame(), &mut rng)
                    .into_iter()
                    .map(|p| {
                            rng.clone().choose(&[Mesh::from(Ellipse::circle(p, rng.gen_range(0.01, 0.15))),
                                         Mesh::from(Ngon {
                                             n: rng.gen_range(3, 7),
                                             phase: rng.gen_range(0.0, 360.0),
                                             radius: rng.gen_range(0.03, 0.5),
                                             center: p,
                                             
                                         })
                            ]).unwrap().clone().with_colorer(Colorer::white())
                         .with_blend_mode(BlendMode::Subtract).with_draw_mode(DrawMode::Stroke { thickness: rng.gen_range(0.01, 0.1) })
                    })
                    .collect::<Vec<Mesh>>(),
            )
            .add(Shader::NearFilter(NearFilterCfg{
                start: Tween::Constant(40.0),
                step: Tween::Oscillation(Oscillation {
                    phase: 0,
                    period: 100,
                    amplitude: 50.0,
                }),
                steps: Tween::Constant(5.0),
                sign: Tween::Constant(-1.0),
            })))
    }
}

fn main() {
    sketch(
        SketchCfg {
            size: 1080,
            quality: 4,
            still: false,
            frame_limit: 100,
            seed: Some(8284898890572370643),
            root_frame_filename: Some(String::from("near_filter_oscillator")),
            ..SketchCfg::default()
        },
        Noise { quality: 4 },
    ).expect("noise");
}
