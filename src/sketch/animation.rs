pub struct Interpolator<T: Sized> {
    f: Box<Fn(f32) -> T>,
    interpolation: Interpolation,
}

impl<T: Sized> Interpolator<T> {
    pub fn interpolate(&self, frame: usize) -> T { (self.f)(self.interpolation.interpolate(frame)) }
}

pub enum Interpolation {
    Linear { start: usize, len: usize },
}

impl Interpolation {
    pub fn interpolate(&self, frame: usize) -> f32 {
        match *self {
            Interpolation::Linear { start, len } => (frame - start) as f32 / len as f32,
        }
    }
}