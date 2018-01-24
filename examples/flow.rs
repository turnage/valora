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

use std::f32::consts::PI;

fn main() {
    sketch(
        SketchCfg::from_args(),
        |ctx: &SketchContext, mut rng: StdRng| -> Result<Composition> {
            let noise = Perlin::new().set_seed(rng.gen());

            let grid_size = rng.gen_range(5, 100);
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

            let noise_samples = grid_spawn_points.iter().map(|p| noise.get([p.x, p.y]));
            let directions = noise_samples.map(|s| s * 2.0 * PI);

            let particle = |center: Point, theta| {
                //let circle = Mesh::from(Ellipse::circle(center, slot_size / 4.0)).with_colorer(Colorer::black());
                let tail = Mesh::from(Poly::from(Rect::new(center.offset(-slot_size / 4.0), slot_size / 1.2, slot_size / 8.0))).with_colorer(Colorer::white()).with_rotation(Tween::Oscillation(Oscillation{
                    phase: 0,
                    period: 100, 
                    amplitude: theta
                }));
                vec![tail]
            };

            let particle_size = 0.3 * slot_size;
            let particles: Vec<Mesh> = grid_spawn_points.iter().zip(directions).flat_map(|(p, theta)| particle(*p, theta)).collect();

            Ok(Composition::new()
                .solid_layer(Colorer::blue())
                .add(Instancer {
                    src: particles[0].clone(),
                    instances: particles
                }))
        },
    ).expect("noise");
}
