use geom::Scale;

pub struct Tweener<T: 'static + Clone> {
    f: Box<Fn(f32, usize) -> T>,
    interp: Interpolation,
}

impl<T: 'static + Clone> Tweener<T> {
    pub fn id(src: T) -> Tweener<T> {
        Tweener {
            f: Box::new(move |_, _| src.clone()),
            interp: Interpolation::Linear { start: 0, len: 0 },
        }
    }

    pub fn tween(&self, frame: usize) -> T { (self.f)(self.interp.interpolate(frame), frame) }

    pub fn done(&self, frame: usize) -> bool { self.interp.done(frame) }
}

impl<T: 'static + Clone + Scale> Tweener<T> {
    pub fn anim_scale(self, start: f32, end: f32, interp: Interpolation) -> Self {
        Tweener {
            f: Box::new(move |completion, frame| {
                            self.tween(frame)
                                .scale(start + completion * (end - start))
                        }),
            interp,
        }
    }
}

pub enum Oscillation {
    Sine,
    Cosine,
}

impl Oscillation {
    pub fn oscillate(&self, x: f32, period: f32) -> f32 {
        use std::f32::consts::PI;

        let x = x * ((2.0 * PI) / period);
        match *self {
            Oscillation::Sine => f32::sin(x),
            Oscillation::Cosine => f32::cos(x),
        }
    }
}

pub enum Interpolation {
    Linear { start: usize, len: usize },
    Oscillation { oscillation: Oscillation, start: usize, period: usize },
}

impl Interpolation {
    pub fn interpolate(&self, frame: usize) -> f32 { self.clamp(self.raw(frame)) }

    pub fn done(&self, frame: usize) -> bool { self.raw(frame) > 1.0 }

    fn raw(&self, frame: usize) -> f32 {
        match *self {
            Interpolation::Linear { start, len } => (frame - start) as f32 / len as f32,
            Interpolation::Oscillation { ref oscillation, start, period } => {
                oscillation.oscillate(start as f32 - frame as f32, period as f32)
            }
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