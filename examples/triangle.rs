use std::{convert::*, iter::*};
use valora::prelude::*;

use itertools::{iproduct, Itertools};
use nalgebra::distance;
use noise::{Fbm, NoiseFn};
use palette::encoding::{srgb::Srgb, TransferFn};
use rand::{distributions::*, prelude::*};
use std::rc::Rc;

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

struct WarpCfg {
    fbm: Fbm,
}

fn warp_polygon(poly: Polygon, cfg: WarpCfg, rng: &mut StdRng) -> Polygon {
    poly.vertices_with_neighbors()
        .map(|(left, v, right)| {
            let strength = (right - left.to_vector()).to_vector().length() / 2.;
            let noise = cfg.fbm.get([v.x as f64 * 10., v.y as f64 * 10.]) as f32;
            let strength = strength - (strength / 4.) + noise.abs() * strength / 4.;

            let offset = Normal::new(0., strength as f64).sample(rng) as f32;
            let theta = Angle::radians(rng.gen_range(0., PI * 2.));
            let shifted = Ellipse::circle(v, offset).circumpoint(theta);
            shifted
        })
        .collect()
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
        let bubble_container = Ellipse::circle(world.center(), 200.);
        let bubble_sampler = bubble_container.uniform_circle_sampler();

        let splotch: Vec<Polygon> = bubble_sampler
            .sample_iter(&mut rng.clone())
            .map(|v| {
                std::iter::successors(Some(Polygon::from(Ngon::new(v, 8, 10.))), |poly| {
                    Some(warp_polygon(
                        poly.clone().subdivide(),
                        WarpCfg { fbm: fbm.clone() },
                        rng,
                    ))
                })
                .nth(2)
                .unwrap()
            })
            .take(1000)
            .collect();

        Ok(move |ctx: Context, canvas: &mut Canvas| {
            if ctx.frame == 0 {
                canvas.set_color(LinSrgb::new(1., 1., 1.));
                canvas.paint(Filled(ctx.world));
            }

            let time = ctx.frame as f32 / 24.;
            let rgb = palette.sample(time * 10.);

            for (i, layer) in splotch.iter().enumerate() {
                canvas.set_color(palette.sample(i as f32 * 0.1));
                canvas.paint(Filled(layer));
            }
        })
    })
    .expect("to run composition");
}
