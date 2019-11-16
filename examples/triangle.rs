use std::{convert::*, iter::*};
use valora::*;

use itertools::{iproduct, Itertools};
use nalgebra::distance;
use noise::{Fbm, NoiseFn};
use rand::{distributions::*, prelude::*};
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
        if self.i == self.n + 1 {
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
        for (i, v) in (*self).enumerate() {
            comp.line_to(v);
        }
    }
}

pub struct GridIter {
    cols: usize,
    rows: usize,
}

impl GridIter {
    pub fn new(cols: usize, rows: usize) -> Self { Self { cols, rows } }

    pub fn tiles(&self, region_width: f32, region_height: f32) -> impl Iterator<Item = Rect> {
        let tile_width = region_width / (self.cols as f32);
        let tile_height = region_height / (self.rows as f32);
        iproduct!(0..(self.cols), 0..(self.rows)).map(move |(i, j)| {
            let x = (i as f32) * tile_width;
            let y = (j as f32) * tile_height;
            Rect {
                bottom_left: V2::new(x, y),
                width: tile_width,
                height: tile_height,
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

pub struct Rect {
    bottom_left: V2,
    width: f32,
    height: f32,
}

impl Rect {
    pub fn center(&self) -> V2 {
        V2::new(
            self.bottom_left.x + self.width / 2.,
            self.bottom_left.y + self.height / 2.,
        )
    }
}

impl Draw for Rect {
    fn draw(&self, comp: &mut Composition) {
        comp.move_to(self.bottom_left);
        for v in [
            V2::new(self.bottom_left.x + self.width, self.bottom_left.y),
            V2::new(
                self.bottom_left.x + self.width,
                self.bottom_left.y + self.height,
            ),
            V2::new(self.bottom_left.x, self.bottom_left.y + self.height),
        ]
        .iter()
        .copied()
        {
            comp.line_to(v);
        }
    }
}

pub struct Filled<D>(D);

impl<D: Draw> Draw for Filled<D> {
    fn draw(&self, comp: &mut Composition) {
        self.0.draw(comp);
        comp.fill();
    }
}

pub struct Stroked<D> {
    element: D,
    thickness: f32,
}

impl<D: Draw> Draw for Stroked<D> {
    fn draw(&self, comp: &mut Composition) {
        self.element.draw(comp);
        comp.set_stroke_thickness(self.thickness);
        comp.stroke();
    }
}

fn triangle_fan(
    fbm: &Fbm,
    palette: &CosineColours,
    d: usize,
    max: usize,
    phase_offset: f32,
    r: f32,
    sign: f32,
    p: V2,
    rng: &mut StdRng,
    comp: &mut Composition,
) {
    if d > max {
        return;
    }

    let noise = ((fbm.get([p.x as f64, p.y as f64]) as f32) + 1.) / 2. * sign;
    let color = palette.sample(d as f32 * noise);
    comp.set_color(V4::new(color.x, color.y, color.z, 1.));
    comp.draw(Filled(NgonIter::triangle(
        noise * std::f32::consts::PI,
        2. * r / (d as f32),
        p,
    )));
    let phase = noise * 2. * std::f32::consts::PI;
    let next_point = Ellipse::circle(p, r).circumpoint(phase);

    let children = rng.gen_range(1, 4);
    triangle_fan(
        fbm,
        palette,
        d + 1,
        max,
        phase_offset,
        r,
        sign,
        next_point,
        rng,
        comp,
    );
}

const NOISE_SHADER: &str = include_str!("noise.frag");

fn main() {
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
            comp.set_color(V4::new(1.0, 1.0, 1.0, 1.0));
            comp.draw(Filled(world.full_frame()));
            comp.set_sample_depth(SampleDepth::Super8);

            let c = world.center();
            let r = 10.;
            comp.set_color(V4::new(1., 0., 0., 1.));
            comp.draw(Stroked {
                element: NgonIter::triangle(0., 50., c),
                thickness: 5.,
            });
            /*for i in 0..1000 {
                triangle_fan(
                    &fbm,
                    &palette,
                    0,
                    1000,
                    10.,
                    r,
                    if i % 2 == 0 { 1. } else { -1. },
                    c + V2::new((i % 10) as f32, 0.) + V2::new(0., (i / 10) as f32),
                    rng,
                    comp,
                );
            }*/

            println!("Enqued; render begins now");
        })
    })
    .expect("to run composition");
}
