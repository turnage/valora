extern crate valora;

use valora::*;
use valora::palette::*;
use valora::rand::{Rng, StdRng};

struct Pendulum;

impl Sketch for Pendulum {
    fn sketch(&self, ctx: &SketchContext, mut rng: StdRng) -> Result<Composition> {
        let track = Ellipse::circle(Point::center(), 0.3);
        let ball = Mesh {
            src: Ellipse::circle(Point::center(), 0.03),
            colorer: Colorer::blue(),
            blend_mode: BlendMode::Normal,
            draw_mode: DrawMode::Stroke { thickness: 0.01 },
        };

        let cycle_frames = 500;
        let ball = Tween::path(track
                                   .clone()
                                   .scale(0.6)
                                   .path((90.0 as f32).to_radians()),
                               Interpolation::Linear { start: 0, len: cycle_frames },
                               ball)
                .cycle(0, 0, cycle_frames);

        let base_hue = rng.gen_range(0.0, 360.0);
        let hue_range = 200.0;
        let n = 30;
        let offset = 10;
        let pendulum = spawn(&Instancer::with((), move |_, point, index| {
            use std::f32::consts::PI;

            let percent = (index as f32) / (n as f32);
            let hue = if percent < 0.5 {
                base_hue + hue_range * (percent * 2.0)
            } else {
                base_hue + hue_range * ((1.0 - percent).abs() * 2.0)
            };
            Tween::from(Mesh {
                            src: Ellipse::circle(point, 0.9),
                            colorer: Colorer::from(Colora::hsv(RgbHue::from(hue), 1.0, 1.0, 1.0)),
                            blend_mode: BlendMode::Normal,
                            draw_mode: DrawMode::Fill,
                        })
                    .anim_scale(0.0, 1.0, Interpolation::Linear { start: index * offset, len: 100 })
        }),
                             &vec![Point::center()]
                                  .into_iter()
                                  .cycle()
                                  .take(n)
                                  .collect::<Vec<Point>>());

        let splotch = Ngon { n: 5, center: Point::center(), rotation: 0.0, radius: 0.1 }
            .as_irregular();
        let splotch = splotch.watercolor(WaterColorCfg {
                                             layers: rng.gen_range(50, 100),
                                             opacity: rng.gen_range(0.02, 0.08),
                                             depth: rng.gen_range(4, 8),
                                             spread: rng.gen_range(0.1, 0.15),
                                         },
                                         |splotch, i| {
            Mesh {
                src: splotch,
                colorer: Colorer::from(Colora::rgb(0.2, 0.2, 0.2, 1.0)),
                draw_mode: DrawMode::Fill,
                blend_mode: if i % 2 == 0 { BlendMode::Add } else { BlendMode::Subtract },
            }
        },
                                         &mut rng);
        let splotch: Vec<Vec<Mesh<IrregularPoly>>> = spawn(&Instancer::with(splotch,
                                                                            |src, point, i| {
            let percentage = (i as f32) / (src.len() as f32);
            src.clone()
                .into_iter()
                .map(|s| s.scale(1.0 + 1.0 * percentage))
                .collect()
        }),
                                                           &vec![Point::center()]
                                                                .into_iter()
                                                                .cycle()
                                                                .take(4)
                                                                .collect::<Vec<Point>>());

        Ok(Composition::new()
               .solid_layer(Colorer::white())
               .add(pendulum)
               .add(splotch))
    }
}

fn main() {
    sketch(SketchCfg {
               size: 1080,
               root_frame_filename: None, //Some(String::from("pendulum")),
               seed: None,
           },
           Pendulum {})
            .expect("sketch");
}
