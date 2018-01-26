use std::sync::{Arc, Mutex};
use std::fmt;
use mesh::MeshSnapshot;

#[derive(Clone)]
pub enum Tween<V: Clone> {
    Keyframes(Vec<Keyframe>),
    Oscillation((Oscillation, Arc<Fn(f32) -> V>)),
    Constant(V),
    Function(Arc<Fn(&MeshSnapshot, usize) -> V>),
}

impl<V: Clone + fmt::Debug> fmt::Debug for Tween<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Tween::Keyframes(ref keyframes) => unimplemented!(),
            Tween::Oscillation((ref oscillation, _)) => oscillation.fmt(f),
            Tween::Constant(ref v) => v.fmt(f),
            Tween::Function(_) => write!(f, "Function tween."),
        }
    }
}

impl<V: Clone> Tween<V> {
    pub fn tween(&self, last: &MeshSnapshot, frame: usize) -> V {
        match *self {
            Tween::Keyframes(ref keyframes) => unimplemented!(),
            Tween::Oscillation((ref oscillation, ref adapter)) => {
                adapter(oscillation.oscillate(frame))
            }
            Tween::Constant(ref v) => v.clone(),
            Tween::Function(ref f) => f(last, frame),
        }
    }

    pub fn function<F: Fn(&MeshSnapshot, usize) -> V + 'static>(f: F) -> Self {
        Tween::Function(Arc::new(f))
    }
}

impl Tween<f32> {
    pub fn oscillation(osc: Oscillation) -> Self {
        Tween::Oscillation((osc, Arc::new(|f| f)))
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
