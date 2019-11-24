// Regular ngons.

use crate::{Canvas, ClosedPath, FlatIterPath, Paint, P2};

#[derive(Copy, Clone, Debug)]
pub struct Ngon {
    phase: f32,
    r: f32,
    n: usize,
    c: P2,
    i: usize,
}

impl Ngon {
    pub fn new(c: P2, n: usize, r: f32) -> Self {
        Self {
            phase: 0.,
            r,
            n,
            c,
            i: 0,
        }
    }

    pub fn triangle(c: P2, r: f32) -> Self { Self::new(c, 3, r) }

    pub fn square(c: P2, r: f32) -> Self { Self::new(c, 4, r) }

    pub fn rotate(&mut self, phase: f32) { self.phase += phase; }
}

impl Iterator for Ngon {
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

impl Paint for Ngon {
    fn paint(&self, comp: &mut Canvas) { comp.paint(ClosedPath::from(FlatIterPath::from(*self))) }
}
