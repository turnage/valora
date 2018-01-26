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
use std::cmp;

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

            let warp_cfg = WarpCfg {
                variance: rng.gen_range(0.0, 1.0),
                axis: WarpAxis::Y,
                ..Default::default()
            };
            let warper = |poly: Poly, rng: &mut StdRng| -> Poly {
                warp(poly, &warp_cfg, rng).subdivide_edges()
            };

            let stroke_count = rng.gen_range(20, 10000);
            let min_size = 0.04;
            let stroke_size = if min_size > (1.0 / (stroke_count as f32).sqrt()) {
                min_size
            } else {
                (1.0 / (stroke_count as f32).sqrt())
            };
            let speed = rng.gen_range(0.001, 0.007);
            let mut warper_rng = rng.clone();
            let strokes: Vec<Mesh> = {
                let mut stroke = |center: Point| {
                    let rect_width = stroke_size / 1.2;
                    let rect_height = stroke_size / 1.8;
                    let rect = Poly::from(Rect::new(center, rect_width, rect_height)).place(center);
                    let stroke =
                        iterate_rand(rect, warper_rng.gen_range(0, 5), &mut warper_rng, &warper)
                            .place(center);
                    let offset = warper_rng.gen_range(0.0, PI / 3.0);
                    let tail = Mesh::from(stroke)
                        .with_color(*warper_rng.choose(palette.as_ref()).unwrap())
                        .with_pos(Tween::function(move |last: &MeshSnapshot, _| {
                            let sample = noise.get([last.pos.x, last.pos.y]);
                            let theta = sample * 2.0 * PI;
                            let mut next =
                                Ellipse::circle(last.pos, speed).circumpoint(theta + offset);
                            if next.x > 1.0 + rect_width * 3.0 || next.x < 0.0 - rect_width * 3.0 {
                                next.x = 1.0 - next.x;
                            }
                            if next.y > 1.0 + rect_height * 3.0 || next.y < 0.0 - rect_height * 3.0
                            {
                                next.y = 1.0 - next.y;
                            }
                            next
                        }))
                        .with_rotation(Tween::function(move |last: &MeshSnapshot, _| {
                            noise.get([last.pos.x, last.pos.y]) * 2.0 * PI + (PI / 2.0) + offset
                        }));
                    vec![
                        tail.clone(),
                        tail.with_color(Colora::rgb(0.0, 0.0, 0.0, 1.0))
                            .with_scale(Tween::Constant(1.1))
                            .with_draw_mode(DrawMode::Stroke {
                                thickness: stroke_size / warper_rng.gen_range(10.0, 100.0),
                            }),
                    ]
                };
                let sparkles: Vec<Point> = sparkles(
                    rng.gen_range(20, 400),
                    &Rect::frame().scale(rng.gen_range(0.7, 1.0)),
                    &mut rng,
                );
                sparkles.into_iter().flat_map(stroke).collect()
            };

            Ok(Composition::new()
                .solid_layer(Colora::hsv(RgbHue::from(184.0), 0.2, 0.8, 1.0))
                .add(strokes))
        },
    ).expect("noise");
}
