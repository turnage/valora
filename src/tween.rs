use std::sync::{Mutex,Arc};
use std::fmt;

#[derive(Clone)]
pub enum Tween {
    Keyframes(Vec<Keyframe>),
    Oscillation(Oscillation),
    Constant(f32),
}

impl fmt::Debug for Tween {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Tween::Keyframes(ref keyframes) => unimplemented!(),
            Tween::Oscillation(ref oscillation) => oscillation.fmt(f),
            Tween::Constant(v) => v.fmt(f),
        }
    }
}

impl Tween {
    pub fn tween(&self, frame: usize) -> f32 {
        match *self {
            Tween::Keyframes(ref keyframes) => unimplemented!(),
            Tween::Oscillation(ref oscillation) => oscillation.oscillate(frame),
            Tween::Constant(v) => v,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Keyframe {
    offset: usize,
    value: f32,
    interpolation: Interpolation,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Oscillation {
    pub phase: usize,
    pub period: usize,
    pub amplitude: f32,
}

impl Oscillation {
    pub fn oscillate(&self, frame: usize) -> f32 {
        use std::f32::consts::PI;

        let x = frame as f32 - self.phase as f32;
        let x = x * ((2.0 * PI) / (self.period as f32)) - (PI / 2.0);
        f32::sin(x) * self.amplitude
    }
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum Interpolation {
    Linear { start: usize, len: usize },
    Oscillation(Oscillation),
    Constant(f32),
}

impl Interpolation {
    pub fn interpolate(&self, frame: usize) -> f32 {
        self.clamp(self.raw(frame))
    }

    fn raw(&self, frame: usize) -> f32 {
        match *self {
            Interpolation::Linear { start, len } => {
                if frame < start {
                    0.0
                } else {
                    (frame - start) as f32 / len as f32
                }
            }
            Interpolation::Oscillation(oscillation) => oscillation.oscillate(frame),
            Interpolation::Constant(completion) => completion,
        }
    }

    fn clamp(&self, raw: f32) -> f32 {
        match *self {
            Interpolation::Oscillation { .. } => (raw + 1.0) / 2.0,
            _ => {
                if raw < 0.0 {
                    0.0
                } else if raw > 1.0 {
                    1.0
                } else {
                    raw
                }
            }
        }
    }
}
