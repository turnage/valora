//! Ellipses

use crate::{transforms::Scale, Angle, Canvas, Paint, Translate, P2, PI, V2};
use rand::{distributions::Distribution, Rng};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Ellipse {
    pub center: P2,
    pub radii: V2,
    pub phase: Angle,
}

impl Ellipse {
    pub fn circle(center: P2, radius: f32) -> Self {
        Ellipse {
            center,
            radii: V2::new(radius, radius),
            phase: Angle::radians(0.),
        }
    }

    pub fn new(center: P2, radii: V2) -> Self {
        Self {
            center,
            radii,
            phase: Angle::radians(0.),
        }
    }

    pub fn with_phase(self, phase: Angle) -> Self { Self { phase, ..self } }

    pub fn circumphase(&self, p: &P2) -> f32 { (p.y - self.center.y).atan2(p.x - self.center.x) }

    pub fn circumpoint(&self, angle: Angle) -> P2 {
        P2::new(
            self.center.x + angle.radians.cos() * self.radii.x,
            self.center.y + angle.radians.sin() * self.radii.y,
        )
    }

    pub fn uniform_circle_sampler(&self) -> UniformCircleSampler { UniformCircleSampler(*self) }
}

impl Scale for Ellipse {
    fn scale(self, factor: f32) -> Self {
        Self {
            radii: self.radii * factor,
            ..self
        }
    }
}

impl Paint for Ellipse {
    fn paint(&self, canvas: &mut Canvas) {
        canvas.move_to(self.circumpoint(self.phase));
        canvas.arc(self.center, self.radii, Angle::radians(PI * 2.), self.phase);
        canvas.close_path();
    }
}

impl Translate for Ellipse {
    fn translate(self, translation: V2) -> Self {
        Self {
            center: self.center + translation,
            ..self
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct UniformCircleSampler(Ellipse);

impl Distribution<P2> for UniformCircleSampler {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> P2 {
        let s = rng.gen_range(0., 1.);
        let theta = rng.gen_range(0., PI * 2.);
        self.0.scale(s).circumpoint(Angle::radians(theta))
    }
}
