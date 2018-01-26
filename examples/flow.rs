extern crate itertools;
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
use itertools::{Either, Itertools};

use std::f32::consts::PI;

fn main() {
    sketch(
        SketchCfg::from_args(),
        |ctx: &SketchContext, mut rng: StdRng| -> Result<Composition> {
            let noise = Perlin::new().set_seed(rng.gen());

            let palette = uniform_palette(
                rng.gen_range(0.0, 360.0),
                rng.gen_range(30.0, 90.0),
                rng.gen_range(0.2, 1.0),
                rng.gen_range(0.7, 1.0),
                1.0,
                rng.gen_range(1, 5),
            );

            let grid_size = rng.gen_range(5, 30);
            let grid_border = rng.gen_range(0.02, 0.3);
            let slot_size = (1.0 - grid_border) / grid_size as f32;
            let grid_spawn_points = grid(&GridCfg {
                points: GridPoints::Centers,
                width: 1.0 - grid_border,
                height: 1.0 - grid_border,
                tiles_wide: grid_size,
                tiles_high: grid_size,
                center: Point::center(),
            });

            let noise_samples = grid_spawn_points.iter().map(|p| noise.get([p.x, p.y]));
            let directions = noise_samples.map(|s| s * 2.0 * PI);

            let warp_cfg = WarpCfg {
                variance: rng.gen_range(0.0, 1.0),
                axis: WarpAxis::Y,
                ..Default::default()
            };
            let warper = |poly: Poly, rng: &mut StdRng| -> Poly {
                warp(poly, &warp_cfg, rng).subdivide_edges()
            };

            let (strokes, stroke_borders): (Vec<Mesh>, Vec<Mesh>) = {
                let mut stroke = |center: Point, theta| {
                    let rect_width = slot_size / 1.2;
                    let rect_height = slot_size / 1.8;
                    let rect = Poly::from(Rect::new(center, rect_width, rect_height)).place(center);
                    let stroke =
                        iterate_rand(rect, rng.gen_range(0, 5), &mut rng, &warper).place(center);
                    let tail = Mesh::from(stroke)
                        .with_color(*rng.choose(palette.as_ref()).unwrap())
                        .with_rotation(Tween::Oscillation(Oscillation {
                            phase: 0,
                            period: 100,
                            amplitude: theta,
                        }));
                    vec![
                        tail.clone(),
                        tail.with_color(Colora::rgb(0.0, 0.0, 0.0, 1.0))
                            .with_draw_mode(DrawMode::Stroke {
                                thickness: (rect_width * rect_height).sqrt()
                                    / rng.gen_range(4.0, 15.0),
                            }),
                    ]
                };
                grid_spawn_points
                    .iter()
                    .zip(directions)
                    .flat_map(|(p, theta)| stroke(*p, theta))
                    .map(|s| {
                        s.with_scale(Tween::Oscillation(Oscillation {
                            amplitude: slot_size * 6.0,
                            phase: 0,
                            period: 100,
                        }))
                    })
                    .partition_map(|m| match m.draw_mode {
                        DrawMode::Fill => Either::Left(m),
                        DrawMode::Stroke { .. } => Either::Right(m),
                    })
            };

            let borders = rng.gen_range(3, 100);
            Ok(Composition::new()
                .solid_layer(Colora::hsv(RgbHue::from(184.0), 0.2, 0.8, 1.0))
                .add(
                    (0..borders)
                        .into_iter()
                        .map(|i| {
                            Mesh::from(Rect::frame().scale((i + 1) as f32 / borders as f32))
                                .with_draw_mode(DrawMode::Stroke {
                                    thickness: 1.0 / (borders * 4) as f32,
                                })
                        })
                        .collect::<Vec<Mesh>>(),
                )
                .add(Instancer {
                    src: strokes[0].clone(),
                    instances: strokes.into_iter().map(|m| m.transforms).collect(),
                })
                .add(Instancer {
                    src: stroke_borders[0].clone(),
                    instances: stroke_borders.into_iter().map(|m| m.transforms).collect(),
                }))
        },
    ).expect("noise");
}
