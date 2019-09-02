use std::convert::*;
use std::iter::*;
use valora::*;

use itertools::{iproduct, Itertools};
use nalgebra::distance;
use rand::distributions::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Ellipse {
    pub center: V2,
    pub width: f64,
    pub height: Option<f64>,
    pub phase: f64,
}

impl Ellipse {
    pub fn circle(center: V2, radius: f64) -> Self {
        Ellipse {
            center,
            width: radius,
            height: None,
            phase: 0.0,
        }
    }

    pub fn new(center: V2, width: f64, height: f64) -> Self {
        Self {
            center,
            width,
            height: Some(height),
            phase: 0.0,
        }
    }

    pub fn with_phase(self, phase: f64) -> Self {
        Self { phase, ..self }
    }

    pub fn circumphase(&self, p: &V2) -> f64 {
        (p.y - self.center.y).atan2(p.x - self.center.x)
    }

    pub fn circumpoint(&self, angle: f64) -> V2 {
        V2::new(
            self.center.x + angle.cos() * self.width,
            self.center.y + angle.sin() * self.height.unwrap_or(self.width),
        )
    }
}

pub struct NgonIter {
    phase: f64,
    r: f64,
    n: usize,
    c: V2,
    i: usize,
}

impl NgonIter {
    pub fn new(phase: f64, r: f64, c: V2, n: usize) -> Self {
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

        let completion = self.i as f64 / self.n as f64;
        let theta = (completion * std::f64::consts::PI * 2.0) + self.phase;
        self.i += 1;

        Some(V2::new(
            self.c.x + theta.sin() * self.r,
            self.c.y + theta.cos() * self.r,
        ))
    }
}

fn centroid<'a>(vs: impl Iterator<Item = &'a V2>) -> V2 {
    let mut min = V2::new(std::f64::MAX, std::f64::MAX);
    let mut max = V2::new(std::f64::MIN, std::f64::MIN);
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

#[derive(Clone)]
pub struct Splotch {
    poly: Polygon,
}

impl Splotch {
    fn warped<'a, S>(&'a self) -> impl Generate<S, Output = Splotch> + 'a {
        move |ctx: &Context<S>, rng: &mut StdRng| {
            let offset = rng.gen_range(0.0, 1.0);
            let c = centroid(self.poly.vertices().iter());
            let dist = Normal::new(0.0, ctx.width / 100.0);
            let phase_dist = Normal::new(0.0, std::f64::consts::PI / 2.0);
            Self {
                poly: Polygon::try_from(
                    self.poly
                        .vertices()
                        .iter()
                        .map(|v| {
                            let offset = dist.sample(rng);
                            let circle = Ellipse::circle(c, (v - c).norm() + offset);
                            let phase = circle.circumphase(v);
                            let phase_offset = phase + phase_dist.sample(rng);
                            circle.circumpoint(phase_offset)
                        })
                        .collect::<Vec<V2>>(),
                )
                .unwrap(),
            }
        }
    }

    fn subdivided<'a, S>(&'a self) -> impl Generate<S, Output = Splotch> + 'a {
        move |ctx: &Context<S>, rng: &mut StdRng| {
            let original_vertices = self.poly.vertices();
            let mut vertices: Vec<V2> = original_vertices
                .iter()
                .tuple_windows::<(_, _)>()
                .flat_map(|(v1, v2)| {
                    let mut next = Some((v1 + v2) / 2.0);
                    std::iter::successors(Some(*v1), move |_| next.take())
                })
                .collect();
            let wrap_around =
                (original_vertices.first().unwrap() + original_vertices.last().unwrap()) / 2.0;
            vertices.push(wrap_around);

            Self {
                poly: Polygon::try_from(vertices).unwrap(),
            }
        }
    }
}

impl Render for Splotch {
    fn render(&self, comp: &mut Composition) {
        for v in self.poly.vertices() {
            comp.line_to(*v);
        }
        comp.fill();
    }
}

pub struct Paint;

impl Composer<()> for Paint {
    fn init(rng: &mut StdRng) -> () {
        ()
    }

    fn draw(&mut self, ctx: &Context<()>, rng: &mut StdRng, comp: &mut Composition) -> () {
        comp.set_shader(Shader::Solid(V4::new(1.0, 1.0, 1.0, 1.0)));
        for v in ctx.full_frame().vertices() {
            comp.line_to(*v);
        }
        comp.fill();

        let mut sub_rng = rng.clone();
        let mut draw_splotch = |color, center, size, layers| {
            let base = Splotch {
                poly: Polygon::try_from(
                    NgonIter::new(1.0, size / 1.5, center, 3).collect::<Vec<V2>>(),
                )
                .unwrap(),
            };

            comp.set_shader(Shader::Solid(color));
            for i in 0..layers {
                if i % 100 == 0 {
                    println!("Finished {:?}", i);
                }
                //let subdivided = base.subdivided().generate(ctx, rng);
                //let warped = subdivided.warped().generate(ctx, rng);

                for v in base.poly.vertices() {
                    comp.line_to(*v);
                }

                comp.fill();
            }
        };
        let rows = 10;
        let cols = 10;
        let radius = ctx.width / rows as f64;
        iproduct!(1..(rows - 1), 1..(cols - 1)).for_each(|(i, j)| {
            let x = ctx.width / cols as f64 * i as f64 + radius / 2.0;
            let y = ctx.height / rows as f64 * j as f64 + radius / 2.0;
            draw_splotch(V4::new(1.0, 0.0, 0.0, 1.0), V2::new(x, y), radius, 1);
        });

        /*

        let variance = ctx.width / 12.0;
        for _ in 0..4 {
            draw_splotch(
                V4::new(1.0, 1.0, 1.0, 1.0 / 200.0),
                ctx.center()
                    + V2::new(
                        sub_rng.gen_range(-variance, variance),
                        sub_rng.gen_range(-variance, variance),
                    ),
                ctx.width / 8.0,
                300,
            );
        }*/

        ()
    }
}

fn main() {
    run::<(), _>(Paint);
}
