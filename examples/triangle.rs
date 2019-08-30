use std::convert::*;
use std::iter::*;
use valora::*;

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
            let dist = Normal::new(0.0, ctx.width / 10.0);
            Self {
                poly: Polygon::try_from(
                    self.poly
                        .vertices()
                        .iter()
                        .map(|v| {
                            let offset = dist.sample(rng);
                            let circle = Ellipse::circle(c, (v - c).norm() + offset);
                            circle.circumpoint(circle.circumphase(v))
                        })
                        .collect::<Vec<V2>>(),
                )
                .unwrap(),
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
        let base = Splotch {
            poly: Polygon::try_from(
                NgonIter::new(1.0, ctx.width / 3.0, ctx.center(), 4).collect::<Vec<V2>>(),
            )
            .unwrap(),
        };

        comp.set_shader(Shader::Solid(V4::new(1.0, 1.0, 1.0, 1.0)));
        for v in ctx.full_frame().vertices() {
            comp.line_to(*v);
        }
        comp.fill();

        comp.set_shader(Shader::Solid(V4::new(1.0, 0.0, 0.0, 0.01)));
        let warper = base.warped();
        for i in 0..150 {
            println!("Writing warped {:?}", i);
            let warped = warper.generate(ctx, rng);

            for v in warped.poly.vertices() {
                comp.line_to(*v);
            }

            comp.fill();
        }

        ()
    }
}

fn main() {
    run::<(), _>(Paint);
}
