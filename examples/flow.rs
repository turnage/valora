extern crate itertools;
extern crate rayon;
extern crate valora;

use valora::*;
use valora::glossy;
use valora::noise::*;
use valora::rand::{Rand, Rng, SeedableRng, StdRng};
use valora::palette::{Colora, Hue, LabHue, RgbHue, Shade};
use valora::image::Pixel;
use std::rc::Rc;
use rayon::prelude::*;
use itertools::{Either, Itertools};

use std::f32::consts::PI;
use std::cmp;

fn main() {
    sketch(
        SketchCfg::from_args(),
        |ctx: &SketchContext, mut rng: StdRng| -> Result<Composition> {
            let noise = Perlin::new().set_seed(rng.gen());

            let bg_base = rng.gen_range(180.0, 240.0);
            let bg_base_range = rng.gen_range(20.0, 50.0);
            let bg_opacity = rng.gen_range(0.2, 1.0);
            let bg_root = Mesh::from(Ellipse::circle(Point::center(), rng.gen_range(0.01, 0.03)));
            let bg = sparkles(10000, &Rect::frame(), &mut rng)
                .into_iter()
                .map(|p| {
                    Mesh::from(Ellipse::circle(p, 1.0))
                        .with_color(Colora::hsv(
                            RgbHue::from(bg_base + noise.get([p.x, p.y]) * bg_base_range),
                            bg_opacity,
                            1.0,
                            1.0,
                        ))
                        .transforms
                })
                .collect();

            let palette = uniform_palette(
                bg_base,
                bg_base_range,
                rng.gen_range(0.8, 1.0),
                rng.gen_range(0.8, 1.0),
                1.0,
                rng.gen_range(1, 7),
            );

            let stroke_count = 100;//rng.gen_range(80, 100);
            let min_size = 0.04;
            let stroke_size = if min_size > (1.0 / (stroke_count as f32).powf(0.8)) {
                min_size
            } else {
                (1.0 / (stroke_count as f32).sqrt())
            };
            let speed = rng.gen_range(0.001, 0.007);
            let stroke_opacity = rng.gen_range(0.3, 1.0);

            let warp_cfg = WarpCfg {
                variance: rng.gen_range(0.0, 1.0),
                axis: WarpAxis::Y,
                ..Default::default()
            };
            let watercolor_cfg = WaterColorCfg {
                layers: rng.gen_range(1, 5) * 2 + 1,
                spread: stroke_size * rng.gen_range(0.5, 2.0),
                anchor_layer: rng.gen(),
                ..WaterColorCfg::default()
            };
            let warper = |poly: Poly, rng: &mut StdRng| -> Poly {
                warp(poly, &warp_cfg, rng).subdivide_edges()
            };
            let mut warper_rng = rng.clone();
            let strokes: Vec<Mesh> = {
                let mut stroke = |center: Point| -> Vec<Mesh> {
                    let radius = stroke_size / 2.0;
                    let rect = Poly::from(Ngon {
                        n: warper_rng.gen_range(3, 10),
                        phase: warper_rng.gen(),
                        radius,
                        center,
                    }).place(warper_rng.gen::<Point>() * -4.0);
                    let color = Colora {
                        alpha: stroke_opacity,
                        ..*warper_rng.choose(palette.as_ref()).unwrap()
                    };
                    let stroke = generate(
                        &WaterColor::new(
                            rect,
                            &WaterColorCfg {
                                color,
                                ..watercolor_cfg
                            },
                            &mut warper_rng,
                        ),
                        warper_rng.clone(),
                    );
                    let offset = warper_rng.gen_range(0.0, PI / 3.0);
                    let period = 100;
                    let height = 0.15;
                    let rotation = warper_rng.gen::<f32>() * 2.0 * PI;
                    let phase = warper_rng.gen_range(30, 80);
                    let tail: Vec<Mesh> = stroke
                        .into_iter()
                        .map(move |m| {
                            m.with_pos(Tween::function(move |last: &MeshSnapshot, frame| {
                                let completion = (frame + phase) as f32 / (1.0 / speed);
                                Point {
                                    x: Ellipse::circle(Point::center(), height).circumpoint(completion * PI * 2.0).x,
                                    y: Ellipse::circle(Point::center(), height).circumpoint(completion * PI * 2.5).y,
                                }.orbit(Point::center(), rotation)
                                
                                /*let sample = noise.get([last.pos.x, last.pos.y]);
                                let theta = sample * 2.0 * PI;
                                let mut next =
                                    Ellipse::circle(last.pos, speed).circumpoint(theta + offset);
                                let x_osc = Oscillation {
                                    amplitude: 1.0,
                                    phase: 0,
                                    period: 500,
                                };
                                let y_osc = Oscillation {
                                    amplitude: 1.0,
                                    phase: 0,
                                    period: 1000,
                                };
                                next = last.pos + (next - last.pos) * Point {
                                    x: x_osc.oscillate(frame),
                                    y: y_osc.oscillate(frame),
                                };
                                if next.x > 1.0 + radius * 3.0 || next.x < 0.0 - radius * 3.0
                                    || next.y > 1.0 + radius * 3.0
                                    || next.y < 0.0 - radius * 3.0
                                {
                                    next = StdRng::from_seed(&[
                                        frame * 100,
                                        (last.origin.y * 10000.0) as usize,
                                    ]).gen();
                                }
                                next*/
                            })).with_rotation(Tween::function(move |last: &MeshSnapshot, _| {
                                    noise.get([last.pos.x, last.pos.y]) * 2.0 * PI + (PI / 2.0)
                                }))
                                .with_scale(Tween::function(move |last: &MeshSnapshot, _| {
                                    if last.pos.x > 1.0 + radius * 2.0
                                        || last.pos.x < 0.0 - radius * 2.0
                                        || last.pos.y > 1.0 + radius * 2.0
                                        || last.pos.y < 0.0 - radius * 2.0
                                    {
                                        0.0
                                    } else {
                                        match last.scale {
                                            x if x > 0.05 && x < 1.0 => x.powf(0.8),
                                            x if x > 1.0 => x - (x - 1.0) / 2.0,
                                            0.0 => 0.2,
                                            _ => 1.0,
                                        }
                                    }
                                }))
                                .with_blend_mode(BlendMode::Subtract)
                        })
                        .collect();
                    tail.clone()
                        .into_iter()
                        .chain(tail.into_iter().map(|m| {
                            let scale = m.transforms.scale.clone();
                            m.with_scale(scale.chain(|_, _, raw| raw / 1.5))
                                .with_color(color.lighten(0.2))
                        }))
                        .collect()
                };
                let sparkles: Vec<Point> = sparkles(
                    stroke_count,
                    &Rect::frame().scale(rng.gen_range(0.7, 1.0)),
                    &mut rng,
                );
                sparkles.into_iter().flat_map(stroke).collect()
            };

            Ok(Composition::new()
                .solid_layer(Colora::hsv(RgbHue::from(184.0), 0.2, 0.8, 1.0))
                .add(Instancer {
                    src: bg_root,
                    instances: bg,
                })
                .add(strokes))
        },
    ).expect("noise");
}
