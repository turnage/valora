extern crate valora;

use valora::*;
use valora::palette::*;
use valora::rand::{Rng, StdRng};

struct Pendulum;

impl Sketch for Pendulum {
    fn sketch(&self, _ctx: &SketchContext, mut rng: StdRng) -> Result<Composition> {
        let base_hue = rng.gen_range(0.0, 360.0);

        let splotch = Ngon {
            n:        5,
            center:   Point::center(),
            rotation: 0.0,
            radius:   0.1,
        }.as_irregular();
        let splotch = spawn(
            &WaterColor::new(
                splotch,
                WaterColorCfg {
                    spread: rng.gen_range(0.01, 0.4),
                    depth: 4,
                    anchor_layer: true,
                    blend_mode: BlendMode::Add,
                    color: Colora::hsv(RgbHue::from(base_hue), 0.73, 0.74, 0.001),
                    ..Default::default()
                },
                &mut rng,
            ),
            &sparkles(rng.gen_range(20, 80), &Rect::frame(), &mut rng),
            rng,
        );

        let hue_range = 200.0;
        let n = 30;
        let offset = 10;
        let pendulum = spawn(
            &Instancer::with((), move |_, cfg| {
                use std::f32::consts::PI;
                let hue = if cfg.percent < 0.5 {
                    base_hue + hue_range * (cfg.percent * 2.0)
                } else {
                    base_hue + hue_range * ((1.0 - cfg.percent).abs() * 2.0)
                };
                Tween::from(Mesh {
                    src:        Ellipse::circle(cfg.point, 0.9),
                    colorer:    if cfg.index == n - 1 {
                        Colorer::white()
                    } else {
                        Colorer::from(Colora::hsv(RgbHue::from(hue), 1.0, 1.0, 1.0))
                    },
                    blend_mode: BlendMode::Normal,
                    draw_mode:  DrawMode::Fill,
                }).anim_scale(
                    0.0,
                    1.,
                    Interpolation::Linear {
                        start: cfg.index * offset,
                        len:   100,
                    },
                )
            }),
            &vec![Point::center()]
                .into_iter()
                .cycle()
                .take(n)
                .collect::<Vec<Point>>(),
            rng,
        );

        Ok(Composition::new()
            .solid_layer(Colorer::from(Colora::hsv(
                RgbHue::from(49.0),
                0.2,
                0.0,
                1.0,
            )))
            .add(pendulum)
            .add(splotch)
            .add(Mesh {
                src:        Rect::frame(),
                colorer:    Colorer::from(Colora::hsv(RgbHue::from(49.0), 0.2, 1.0, 1.0)),
                blend_mode: BlendMode::MaskTransparent,
                draw_mode:  DrawMode::Fill,
            }))
    }
}

fn main() {
    sketch(
        SketchCfg {
            size:                1080,
            root_frame_filename: Some(String::from("jungle")),
            seed:                None,
        },
        Pendulum {},
    ).expect("sketch");
}
