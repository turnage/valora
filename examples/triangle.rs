use std::convert::*;
use std::iter::*;
use valora::*;

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

fn main() {
    let mut comp = Composition::with_dimensions(5000, 5000);
    for v in NgonIter::new(1.0, 500.0, V2::new(2500.0, 2500.0), 4) {
        comp.line_to(v);
    }

    comp.fill();

    let result = comp.finalize();
    result.save("comp.png").expect("To save surface");
}
