use color::Colorer;
use geom::{Path, Place, Scale};
use mesh::Mesh;
use palette::Colora;
use std::rc::Rc;

#[derive(Clone)]
pub struct Tween<T> {
    f: Rc<Fn(f32, usize) -> T>,
    interp: Interpolation,
}

impl<S: 'static> Tween<S> {
    pub fn with<T: 'static, F: 'static + Fn(&S, usize) -> T>(self, f: F) -> Tween<T> {
        Tween {
            f: Rc::new(move |_, frame| f(&self.tween(frame), frame)),
            interp: Interpolation::Constant(1.0),
        }
    }

    pub fn tween(&self, frame: usize) -> S { (self.f)(self.interp.interpolate(frame), frame) }

    pub fn cycle(self, delay: usize, start: usize, len: usize) -> Self {
        Self {
            f: Rc::new(move |_, frame| if frame < delay {
                           self.tween(start)
                       } else {
                           self.tween(((frame - delay) % len) + start)
                       }),
            interp: Interpolation::Constant(1.0),
        }
    }
}

impl<T: 'static + Clone> From<T> for Tween<T> {
    fn from(src: T) -> Self {
        Self {
            f: Rc::new(move |_, _| src.clone()),
            interp: Interpolation::Linear { start: 0, len: 0 },
        }
    }
}

impl<T: 'static + Clone + Scale> Tween<T> {
    pub fn anim_scale(self, start: f32, end: f32, interp: Interpolation) -> Self {
        Tween {
            f: Rc::new(move |completion, frame| {
                           self.tween(frame)
                               .scale(start + completion * (end - start))
                       }),
            interp,
        }
    }
}

impl<T: 'static + Clone + Place> Tween<T> {
    pub fn path<P: 'static + Path>(path: P, interp: Interpolation, element: T) -> Self {
        Self {
            f: Rc::new(move |completion, _frame| element.clone().place(path.path(completion))),
            interp,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Oscillation {
    Sine,
    Cosine,
}

impl Oscillation {
    pub fn oscillate(&self, x: f32, period: f32) -> f32 {
        use std::f32::consts::PI;

        let x = x * ((2.0 * PI) / period) - (PI / 2.0);
        match *self {
            Oscillation::Sine => f32::sin(x),
            Oscillation::Cosine => f32::cos(x),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Interpolation {
    Linear { start: usize, len: usize },
    Oscillation { oscillation: Oscillation, start: usize, period: usize },
    Constant(f32),
}

impl Interpolation {
    pub fn interpolate(&self, frame: usize) -> f32 { self.clamp(self.raw(frame)) }

    fn raw(&self, frame: usize) -> f32 {
        match *self {
            Interpolation::Linear { start, len } => {
                if frame < start { 0.0 } else { (frame - start) as f32 / len as f32 }
            }
            Interpolation::Oscillation { ref oscillation, start, period } => {
                oscillation.oscillate(frame as f32 - start as f32, period as f32)
            }
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