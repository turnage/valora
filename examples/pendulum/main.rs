#![feature(type_ascription)]

extern crate itertools;
#[macro_use]
extern crate lazy_static;
extern crate valora;

use valora::*;
use valora::noise::*;
use valora::rand::{Rng, StdRng};
use valora::rand::distributions::{IndependentSample, Normal};
use valora::color::Color;

use std::f32::consts::PI;

lazy_static! {
    static ref WHITE: Color = hsv(0.0, 0.0, 0.96);
    static ref RED: Color = hsv(360.0, 0.78, 1.0);
    static ref YELLOW: Color = hsv(32.0, 0.84, 1.0);
    static ref BLUE: Color = hsv(206.0, 1.0, 0.93);
    static ref BLUE_GRAY: Color = hsv(213.0, 0.2, 0.58);
    static ref GRAY: Color = hsv(0.0, 0.0, 0.85);
    static ref BLACK: Color = hsv(0.0, 0.0, 0.22);
}

fn pacman(start: f32, end: f32, x: f32) -> f32 {
    if x > end {
        start + x - end
    } else if x < start {
        end - (start - x)
    } else {
        x
    }
}

#[derive(Clone, Copy, Debug)]
struct StreamCfg {
    row: f32,
    spacing: f32,
    speed: f32,
}

fn stream(cfg: StreamCfg, elements: Vec<Mesh>) -> Vec<Mesh> {
    elements
        .into_iter()
        .enumerate()
        .map(|(i, e)| {
            let start = i as f32 * cfg.spacing;
            e.with_pos(Tween::function(move |last, frame| Point {
                x: if frame == 0 {
                    start
                } else {
                    pacman(-0.5, 1.5, last.pos.x + cfg.speed)
                },
                y: cfg.row,
            }))
        })
        .collect()
}

fn main() {
    compose(
        CompositionCfg::from_args(),
        |ctx: &CompositionCtx, mut rng: StdRng| -> Result<Composition> {
            let noise1 = Perlin::new().set_seed(rng.gen());
            let noise2 = Perlin::new().set_seed(rng.gen());
            let bg = Mesh::from(Rect::frame()).with_color(*WHITE);

            let row_count = rng.gen_range(10, 40);
            let row_centers = grid(&GridCfg {
                center: Point::center(),
                points: GridPoints::Centers,
                width: 1.0,
                height: 1.0,
                tiles_wide: 1,
                tiles_high: row_count,
            });
            let row_height = 1.0 / row_centers.len() as f32;
            let base_speed = 0.0025;
            let move_distortion = rng.gen_range(0.1, 0.3);

            let mut element_rng = rng.clone();
            let mut element = || {
                let t = element_rng.gen_range(1, 3);
                match t {
                    2 => Mesh::from(Ngon {
                        phase: element_rng.gen_range(0.0, 360.0),
                        n: element_rng.gen_range(3, 5),
                        center: Point::center(),
                        radius: row_height / 3.0,
                    }),
                    _ => Mesh::from(Ellipse::circle(Point::center(), row_height / 3.0)),
                }
            };
            let stream_grid: Vec<Mesh> = row_centers
                .into_iter()
                .flat_map(move |p: Point| -> Vec<Mesh> {
                    let ellipses: Vec<Mesh> = (0..row_count)
                        .into_iter()
                        .map(|_| {
                            element().with_color(*rng.choose(&[*BLACK, *BLUE_GRAY, *GRAY]).unwrap())
                        })
                        .collect();
                    let cfg = StreamCfg {
                        row: p.y + row_height / 3.0,
                        spacing: row_height * 2.0,
                        speed: (rng.gen_range(1, 7) * (*rng.choose(&[-1, 1]).unwrap())) as f32
                            * base_speed,
                    };
                    let mut stream_elems = stream(cfg, ellipses.clone());
                    stream_elems.append(&mut stream(
                        StreamCfg {
                            spacing: cfg.spacing * rng.gen_range(0.994, 1.001),
                            ..cfg
                        },
                        ellipses,
                    ).into_iter()
                        .map(|m| m.with_color(*rng.choose(&[*RED, *BLUE, *YELLOW]).unwrap()))
                        .collect());
                    stream_elems
                        .into_iter()
                        .map(move |m| {
                            let (pos1, pos2) = (m.transforms.pos.clone(), m.transforms.pos.clone());
                            let m = m.with_scale(Tween::function(move |last, frame| {
                                let pos = pos1.tween(last, frame);
                                noise1.get([pos.x, pos.y]) * 0.7 + noise2.get([pos.x, pos.y]) * 0.7
                            }));
                            m.with_pos(pos2.chain(move |_, _, nested| Point {
                                y: nested.y
                                    + (noise1.get([nested.x, nested.y]) - 0.5) * move_distortion,
                                ..nested
                            }))
                        })
                        .collect()
                })
                .collect();
            Ok(Composition::new().add(bg).add(stream_grid))
        },
    ).expect("noise");
}
