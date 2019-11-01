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

    pub fn triangle(phase: f32, r: f32, c: V2) -> Self { Self::new(phase, r, c, 3) }
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

#[derive(Clone, Copy, Debug)]
pub enum TileFocus {
    Center,
}

pub struct GridIter {
    cols: usize,
    rows: usize,
}

impl GridIter {
    pub fn new(cols: usize, rows: usize) -> Self { Self { cols, rows } }

    pub fn by_tile(
        &self,
        region_width: f32,
        region_height: f32,
        tile_focus: TileFocus,
    ) -> impl Iterator<Item = V2> {
        let tile_width = region_width / (self.cols as f32);
        let tile_height = region_height / (self.rows as f32);
        iproduct!(0..(self.cols), 0..(self.rows)).map(move |(i, j)| match tile_focus {
            TileFocus::Center => {
                let x = (i as f32) * tile_width + (tile_width / 2.0);
                let y = (j as f32) * tile_height + (tile_height / 2.0);
                V2::new(x, y)
            }
        })
    }
}

pub struct CosineColours {
    a: V3,
    b: V3,
    c: V3,
    d: V3,
}

impl CosineColours {
    pub fn new(a: V3, b: V3, c: V3, d: V3) -> Self { Self { a, b, c, d } }

    pub fn sample(&self, t: f32) -> V3 {
        self.a + self.b.component_mul(&(self.c * t + self.d).map(f32::cos))
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

const NOISE_SHADER: &str = include_str!("noise.frag");

fn main() {
    use noise::{Fbm, NoiseFn};

    let fbm = Fbm::default();

    let options = Options::from_args();
    run(options, |gpu, world, mut rng, mut render_gate| {
        let noise_shader_builder = gpu.build_shader(NOISE_SHADER).expect("to compile glsl");
        let noise_shader = noise_shader_builder.build().expect("to build noise shader");
        let palette = CosineColours::new(
            V3::new(0.5, 0.5, 0.5),
            V3::new(0.5, 0.5, 0.5),
            V3::new(2.0, 1.0, 1.0),
            V3::new(0.0, 0.25, 0.25),
        );
        render_gate.render_frames(|ctx, mut comp| {
            comp.set_color(V4::new(1.0, 1.0, 0.7, 1.0));
            for v in world.full_frame().vertices() {
                comp.line_to(*v);
            }
            comp.fill();

            comp.set_sample_depth(SampleDepth::Super64);
            let triangles = GridIter::new(50, 50)
                .by_tile(world.width * 1.5, world.height * 1.2, TileFocus::Center)
                .map(|p| {
                    let c = palette.sample(
                        (p.x + 40.0 + (fbm.get([p.x as f64, p.y as f64]) * 30.0) as f32) / 80.0,
                    );
                    (
                        V4::new(c.x, c.y, c.z, 1.0),
                        NgonIter::triangle(0.0, 25.0, V2::new(p.x, p.y - 18.0 * c.x)),
                    )
                })
                .for_each(|(c, t)| {
                    comp.set_color(c);
                    comp.draw(t);
                });

            comp.set_sample_depth(SampleDepth::Single);
            comp.set_color(V4::new(1.0, 1.0, 1.0, 0.07));
            comp.set_shader(noise_shader.clone());
            for v in world.full_frame().vertices() {
                comp.line_to(*v);
            }
            comp.fill();

            println!("Enqued; render begins now");
        })
    })
    .expect("to run composition");
}
