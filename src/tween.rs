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
    Linear {
        start: usize,
        len: usize,
    },
    Oscillation {
        oscillation: Oscillation,
        start: usize,
        period: usize,
    },
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
            Interpolation::Oscillation {
                ref oscillation,
                start,
                period,
            } => oscillation.oscillate(frame as f32 - start as f32, period as f32),
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
