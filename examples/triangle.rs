use std::{convert::*, iter::*};
use valora::prelude::*;

use itertools::{iproduct, Itertools};
use nalgebra::distance;
use noise::{Fbm, NoiseFn};
use palette::encoding::{srgb::Srgb, TransferFn};
use rand::{distributions::*, prelude::*};
use std::rc::Rc;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Ellipse {
    pub center: P2,
    pub width: f32,
    pub height: Option<f32>,
    pub phase: f32,
}

impl Ellipse {
    pub fn circle(center: P2, radius: f32) -> Self {
        Ellipse {
            center,
            width: radius,
            height: None,
            phase: 0.0,
        }
    }

    pub fn new(center: P2, width: f32, height: f32) -> Self {
        Self {
            center,
            width,
            height: Some(height),
            phase: 0.0,
        }
    }

    pub fn with_phase(self, phase: f32) -> Self { Self { phase, ..self } }

    pub fn circumphase(&self, p: &P2) -> f32 { (p.y - self.center.y).atan2(p.x - self.center.x) }

    pub fn circumpoint(&self, angle: f32) -> P2 {
        P2::new(
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
    c: P2,
    i: usize,
}

impl NgonIter {
    pub fn new(phase: f32, r: f32, c: P2, n: usize) -> Self {
        Self {
            phase,
            r,
            n,
            c,
            i: 0,
        }
    }

    pub fn triangle(phase: f32, r: f32, c: P2) -> Self { Self::new(phase, r, c, 3) }
}

impl Iterator for NgonIter {
    type Item = P2;

    fn next(&mut self) -> Option<Self::Item> {
        if self.i == self.n + 1 {
            return None;
        }

        let completion = self.i as f32 / self.n as f32;
        let theta = (completion * std::f32::consts::PI * 2.0) + self.phase;
        self.i += 1;

        Some(P2::new(
            self.c.x + theta.sin() * self.r,
            self.c.y + theta.cos() * self.r,
        ))
    }
}

impl Paint for NgonIter {
    fn paint(&self, comp: &mut Canvas) {
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
                bottom_left: P2::new(x, y),
                width: tile_width,
                height: tile_height,
            }
        })
    }
}

pub struct CosineColours {
    a: LinSrgb,
    b: LinSrgb,
    c: LinSrgb,
    d: LinSrgb,
}

impl CosineColours {
    pub fn new(a: LinSrgb, b: LinSrgb, c: LinSrgb, d: LinSrgb) -> Self { Self { a, b, c, d } }

    pub fn sample(&self, t: f32) -> LinSrgb {
        self.c
            .component_wise(&self.d, |c, d| (c * t + d))
            .component_wise_self(|v| v * std::f32::consts::PI * 2.)
            .component_wise_self(f32::cos)
            .component_wise(&self.a, std::ops::Add::add)
    }
}

fn centroid<'a>(vs: impl Iterator<Item = &'a P2>) -> P2 {
    let mut min = P2::new(std::f32::MAX, std::f32::MAX);
    let mut max = P2::new(std::f32::MIN, std::f32::MIN);
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

    (min + max.to_vector()) / 2.0
}

pub struct Rect {
    bottom_left: P2,
    width: f32,
    height: f32,
}

impl Rect {
    pub fn center(&self) -> P2 {
        P2::new(
            self.bottom_left.x + self.width / 2.,
            self.bottom_left.y + self.height / 2.,
        )
    }
}

impl Paint for Rect {
    fn paint(&self, comp: &mut Canvas) {
        comp.move_to(self.bottom_left);
        for v in [
            P2::new(self.bottom_left.x + self.width, self.bottom_left.y),
            P2::new(
                self.bottom_left.x + self.width,
                self.bottom_left.y + self.height,
            ),
            P2::new(self.bottom_left.x, self.bottom_left.y + self.height),
        ]
        .iter()
        .copied()
        {
            comp.line_to(v);
        }
    }
}

pub struct Filled<D>(D);

impl<D: Paint> Paint for Filled<D> {
    fn paint(&self, comp: &mut Canvas) {
        self.0.paint(comp);
        comp.fill();
    }
}

pub struct Stroked<D> {
    element: D,
    thickness: f32,
}

impl<D: Paint> Paint for Stroked<D> {
    fn paint(&self, comp: &mut Canvas) {
        self.element.paint(comp);
        comp.set_stroke_thickness(self.thickness);
        comp.stroke();
    }
}

pub struct Squig {
    center: P2,
    r: f32,
}

impl Paint for Squig {
    fn paint(&self, comp: &mut Canvas) {
        let start = 0.;
        let end = std::f32::consts::PI;
        let circle = Ellipse::circle(self.center, self.r);

        let phase = std::f32::consts::PI / 2.;

        comp.move_to(circle.circumpoint(start + phase));
        comp.cubic_to(
            circle.circumpoint(end / 4. + phase),
            circle.circumpoint(end / 2. + phase),
            circle.circumpoint(end + phase),
        );
        comp.cubic_to(
            circle.circumpoint(-end / 2. + phase),
            circle.circumpoint(-end / 4. + phase),
            circle.circumpoint(start + phase),
        );
        comp.close();
    }
}

const NOISE_SHADER: &str = include_str!("stripe.frag");

fn main() {
    let options = Options::from_args();
    run_fn(options, |gpu, world, rng| {
        let glsl = gpu.compile_glsl(NOISE_SHADER).expect("to compile glsl");
        let stripe_shader = gpu.build_shader(glsl, UniformBuffer::default())?;
        let palette = CosineColours::new(
            LinSrgb::new(0.5, 0.5, 0.5),
            LinSrgb::new(0.5, 0.5, 0.5),
            LinSrgb::new(2.0, 1.0, 0.0),
            LinSrgb::new(0.5, 0.20, 0.25),
        );
        let grid_size = rng.gen_range(2, 20);
        let fbm = Fbm::default();

        Ok(move |ctx: Context, canvas: &mut Canvas| {
            if ctx.frame == 0 {
                canvas.set_color(LinSrgb::new(1., 1., 1.));
                canvas.paint(Filled(ctx.world));
            }

            let time = ctx.frame as f32 / 24.;

            let signal = time.rem_euclid(4.) / 4.;
            let rgb = palette.sample(signal);
            canvas.set_color(rgb);

            //canvas.set_shader(self.stripe_shader.clone());

            let r = 10. + time * 10.;
            for c in GridIter::new(grid_size, grid_size)
                .tiles(ctx.world.width, ctx.world.height)
                .map(|r| r.center())
            {
                let c = Ellipse::circle(c, time * 10.).circumpoint(
                    fbm.get([c.x as f64, c.y as f64, time as f64]) as f32
                        * std::f32::consts::PI
                        * 2.,
                );
                canvas.paint(Filled(Squig { center: c, r: r }));
            }
        })
    })
    .expect("to run composition");
}
