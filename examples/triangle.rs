use std::{convert::*, iter::*};
use valora::*;

use itertools::{iproduct, Itertools};
use nalgebra::distance;
use rand::distributions::*;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Ellipse {
    pub center: V2,
    pub width: f32,
    pub height: Option<f32>,
    pub phase: f32,
}

impl Ellipse {
    pub fn circle(center: V2, radius: f32) -> Self {
        Ellipse {
            center,
            width: radius,
            height: None,
            phase: 0.0,
        }
    }

    pub fn new(center: V2, width: f32, height: f32) -> Self {
        Self {
            center,
            width,
            height: Some(height),
            phase: 0.0,
        }
    }

    pub fn with_phase(self, phase: f32) -> Self { Self { phase, ..self } }

    pub fn circumphase(&self, p: &V2) -> f32 { (p.y - self.center.y).atan2(p.x - self.center.x) }

    pub fn circumpoint(&self, angle: f32) -> V2 {
        V2::new(
            self.center.x + angle.cos() * self.width,
            self.center.y + angle.sin() * self.height.unwrap_or(self.width),
        )
    }
}

#[derive(Copy, Clone, Debug)]
pub struct NgonIter {
    phase: f32,
    r: f32,
    n: usize,
    c: V2,
    i: usize,
}

impl NgonIter {
    pub fn new(phase: f32, r: f32, c: V2, n: usize) -> Self {
        Self {
            phase,
            r,
            n,
            c,
            i: 0,
        }
    }
}

impl Iterator for NgonIter {
    type Item = V2;

    fn next(&mut self) -> Option<Self::Item> {
        if self.i == self.n {
            return None;
        }

        let completion = self.i as f32 / self.n as f32;
        let theta = (completion * std::f32::consts::PI * 2.0) + self.phase;
        self.i += 1;

        Some(V2::new(
            self.c.x + theta.sin() * self.r,
            self.c.y + theta.cos() * self.r,
        ))
    }
}

impl Draw for NgonIter {
    fn draw(&self, comp: &mut Composition) {
        for v in *self {
            comp.line_to(v);
        }
        comp.fill();
    }
}

fn centroid<'a>(vs: impl Iterator<Item = &'a V2>) -> V2 {
    let mut min = V2::new(std::f32::MAX, std::f32::MAX);
    let mut max = V2::new(std::f32::MIN, std::f32::MIN);
    for v in vs {
        if v.x < min.x {
            min.x = v.x;
        }
        if v.y < min.y {
            min.y = v.y;
        }
        if v.x > max.x {
            max.x = v.x;
        }
        if v.y > max.y {
            max.y = v.y;
        }
    }

    (min + max) / 2.0
}

const NOISE_SHADER: &str = include_str!("../src/shaders/poke.frag");

fn main() {
    let options = Options::from_args();
    run(options, |gpu, world, mut rng, mut render_gate| {
        let noise_shader_builder = gpu.build_shader(NOISE_SHADER).expect("to compile glsl");
        let noise_shader = noise_shader_builder.build().expect("to build noise shader");
        render_gate.render_frames(|ctx, mut comp| {
            comp.set_color(V4::new(1.0, 1.0, 0.7, 1.0));
            for v in world.full_frame().vertices() {
                comp.line_to(*v);
            }
            comp.fill();

            let center = world.center();
            comp.set_color(V4::new(1.0, 0.0, 0.0, 0.5));
            println!("Enqueing triangles for render....");
            for i in 0..100 {
                let y_off = (i as f32 / 100.0) * 100.0;
                let x_off = i as f32 % 100.0;
                let triangle =
                    NgonIter::new(0.0, 30.0, V2::new(center.x + x_off, center.y + y_off), 3);
                //comp.set_shader(noise_shader.clone());
                comp.draw(triangle);
            }
            println!("Enqued; render begins now");
        })
    })
    .expect("to run composition");
}
