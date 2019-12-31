// Regular ngons.

use crate::{Canvas, FlatIterPath, Paint, Scale, Translate, P2, V2};

#[derive(Copy, Clone, Debug)]
pub struct Ngon {
    pub phase: f32,
    pub radius: f32,
    pub n: usize,
    pub center: P2,
    i: usize,
}

impl Ngon {
    pub fn new(center: P2, n: usize, radius: f32) -> Self {
        Self {
            phase: 0.,
            radius,
            n,
            center,
            i: 0,
        }
    }

    pub fn triangle(center: P2, radius: f32) -> Self {
        Self::new(center, 3, radius)
    }

    pub fn square(center: P2, radius: f32) -> Self {
        let mut diamond = Self::diamond(center, radius);
        diamond.phase -= std::f32::consts::PI / 4.;
        diamond
    }

    pub fn diamond(center: P2, radius: f32) -> Self {
        Self::new(center, 4, radius)
    }

    pub fn rotate(&mut self, phase: f32) {
        self.phase += phase;
    }
}

impl Iterator for Ngon {
    type Item = P2;

    fn next(&mut self) -> Option<Self::Item> {
        if self.i == self.n {
            return None;
        }

        let completion = self.i as f32 / self.n as f32;
        let theta = (completion * std::f32::consts::PI * 2.0) + self.phase;
        self.i += 1;

        Some(P2::new(
            self.center.x + theta.sin() * self.radius,
            self.center.y + theta.cos() * self.radius,
        ))
    }
}

impl Scale for Ngon {
    fn scale(self, factor: f32) -> Self {
        Self {
            radius: self.radius * factor,
            ..self
        }
    }
}

impl Translate for Ngon {
    fn translate(self, translation: V2) -> Self {
        Self {
            center: self.center + translation,
            ..self
        }
    }
}

impl Paint for Ngon {
    fn paint(&self, comp: &mut Canvas) {
        comp.paint(FlatIterPath::new(*self, /*closed=*/ true))
    }
}
