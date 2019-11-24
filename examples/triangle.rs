use std::{convert::*, iter::*};
use valora::prelude::*;

use itertools::{iproduct, Itertools};
use nalgebra::distance;
use noise::{Fbm, NoiseFn};
use palette::encoding::{srgb::Srgb, TransferFn};
use rand::{distributions::*, prelude::*};
use std::rc::Rc;

#[derive(Copy, Clone, Debug)]
pub struct NgonIter {
    phase: f32,
    r: f32,
    n: usize,
    c: P2,
    i: usize,
}

impl NgonIter {
    pub fn new(r: f32, c: P2, n: usize) -> Self {
        Self {
            phase: 0.,
            r,
            n,
            c,
            i: 0,
        }
    }

    pub fn triangle(r: f32, c: P2) -> Self { Self::new(r, c, 3) }

    pub fn rotate(&mut self, phase: f32) { self.phase += phase; }
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
            .component_wise(&self.b, std::ops::Mul::mul)
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

fn noise_shift(p: P2, noise: f32, amount: f32) -> P2 {
    let theta = noise * PI;
    Ellipse::circle(p, amount).circumpoint(Angle::radians(theta))
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
        let border = rng.gen_range(10., 100.);
        let frame = Rect {
            origin: P2::new(border, border),
            size: S2::new(world.width - border * 2., world.height - border * 2.),
        };

        #[derive(Clone)]
        pub struct AimedWalk {
            start: P2,
            end: P2,
            t: f32,
            step: f32,
            fbm: Fbm,
            world: World,
        }

        impl AimedWalk {
            pub fn new(start: P2, end: P2, step: f32, world: World) -> Self {
                Self {
                    start,
                    end,
                    t: 0.,
                    step,
                    fbm: Fbm::default(),
                    world,
                }
            }
        }

        impl Iterator for AimedWalk {
            type Item = P2;
            fn next(&mut self) -> Option<Self::Item> {
                if self.t > 1. {
                    return None;
                }

                let next_t = self.t + self.step;
                let raw_p = self.start.lerp(self.end, self.t);
                let noise_sample = self.world.normalize(raw_p);
                let noise = self.fbm.get([noise_sample.x as f64, noise_sample.y as f64]) as f32;
                let p = noise_shift(raw_p, noise, 100.);

                self.t = next_t;
                Some(p)
            }
        }

        impl Paint for AimedWalk {
            fn paint(&self, canvas: &mut Canvas) { canvas.paint(FlatIterPath::from(self.clone())) }
        }

        let initial_line = AimedWalk::new(
            frame.origin,
            frame.origin + frame.size.to_vector(),
            0.01,
            *world,
        );
        let bubble_container = Ellipse::circle(world.center(), 100.);
        let bubble_sampler = bubble_container.uniform_circle_sampler();

        Ok(move |ctx: Context, canvas: &mut Canvas| {
            if ctx.frame == 0 {
                canvas.set_color(LinSrgb::new(1., 1., 1.));
                canvas.paint(Filled(ctx.world));
                canvas.set_color(LinSrgb::new(0., 0., 0.));
                canvas.paint(Stroked {
                    element: frame,
                    thickness: 0.3,
                });
            }

            let time = ctx.frame as f32 / 24.;
            let rgb = palette.sample(time / 30.);

            for spawn_point in std::iter::from_fn(|| Some(bubble_sampler.sample(ctx.rng))).take(100)
            {
                canvas.set_color(rgb);
                canvas.paint(Filled(Ellipse::circle(spawn_point, 5.)));
            }

            canvas.set_color(rgb);
            canvas.paint(Stroked {
                element: initial_line.clone(),
                thickness: 10.,
            });
        })
    })
    .expect("to run composition");
}
