extern crate rayon;
extern crate valora;
extern crate itertools;

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

            let palette = uniform_palette(rng.gen_range(0.0, 360.0), rng.gen_range(0.0, 60.0), rng.gen_range(0.2, 1.0), rng.gen_range(0.7, 1.0), 1.0, rng.gen_range(3, 5));

            let grid_size = rng.gen_range(20, 60);
            let slot_size = 1.0 / grid_size as f32;
            let grid_spawn_points: Vec<Point> = (0..(grid_size*grid_size)).into_iter().map(|i| {
                let x = i / grid_size;
                let y = i % grid_size;
                let bottom_left = Point {
                    x: x as f32 * slot_size,
                    y: y as f32 * slot_size,
                };
                Rect::square(bottom_left, slot_size).center()
            }).collect();
            let gridfree_points = sparkles(rng.gen_range(0, grid_size * grid_size), &Rect::frame(), &mut rng);

            let noise_samples = grid_spawn_points.iter().map(|p| noise.get([p.x, p.y]));
            let directions = noise_samples.map(|s| s * 2.0 * PI);

            let warp_cfg = WarpCfg{
                    variance: rng.gen_range(0.0, 1.0),
                    axis: WarpAxis::Y,
                    ..Default::default()
                };
            let warper = |poly: Poly, rng: &mut StdRng| -> Poly { warp(poly, &warp_cfg, rng).subdivide_edges() };
            let mut stroke = |center: Point, theta| {
                let rect =  Poly::from(Rect::new(
                        center.offset(-slot_size / 4.0),
                        slot_size / 1.2,
                        slot_size / 8.0));
                let stroke = iterate_rand(rect, 4, &mut rng, &warper);
                let tail = Mesh::from(stroke).with_color(*rng.choose(palette.as_ref()).unwrap()).with_rotation(Tween::Oscillation(
                    Oscillation {
                        phase: 0, 
                        period: 100,
                        amplitude: theta
                    }));
                vec![
                    tail.clone(),
                    tail.with_color(Colora::rgb(0.0, 0.0, 0.0, 1.0))
                        .with_draw_mode(DrawMode::Stroke { thickness: slot_size / 16.0 })
                ]
            };

            let (strokes, stroke_borders): (Vec<Mesh>, Vec<Mesh>) = grid_spawn_points.iter().map(|p| *p).chain(gridfree_points).zip(directions).flat_map(|(p, theta)| stroke(p, theta)).map(|s| s.with_scale(
                Tween::Oscillation(Oscillation{
                    amplitude: slot_size * 6.0,
                    phase: 0,
                    period: 100,
                })
            )).partition_map(|m| match m.draw_mode {
                DrawMode::Fill => Either::Left(m),
                DrawMode::Stroke { .. } => Either::Right(m),
            });

            Ok(Composition::new()
                .solid_layer(Colora::hsv(RgbHue::from(0.0), 0.05, 0.8, 1.0))
                .add(Instancer {
                    src: strokes[0].clone(),
                    instances: strokes
                })
                .add(Instancer{
                    src: stroke_borders[0].clone(),
                    instances: stroke_borders,
                }))
        },
    ).expect("noise");
}
