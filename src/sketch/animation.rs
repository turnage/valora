use errors::Result;
use geom::Percent;
use sketch::{Canvas, Draw, SketchContext};

pub trait Animate {
    type Output;
    fn tween(&self, frame: usize) -> Self::Output;
    fn done(&self, frame: usize) -> bool;
}

impl<A: Animate> Draw for A
    where A::Output: Draw
{
    fn draw(&self, ctx: &SketchContext) -> Result<Canvas> { self.tween(ctx.frame).draw(ctx) }
}

pub trait AnimPercent {
    type Output: Animate;
    fn anim_percent(self, start: f32, end: f32, interp: Interpolation) -> Self::Output;
}

impl<P: Percent + Clone> AnimPercent for P {
    type Output = Tweener<P>;
    fn anim_percent(self, start: f32, end: f32, interp: Interpolation) -> Tweener<P> {
        Tweener {
            src: self,
            f: Box::new(move |completion, src| src.percent(start + completion * (end - start))),
            interp,
        }
    }
}

pub struct Tweener<T: Clone> {
    src: T,
    f: Box<Fn(f32, T) -> T>,
    interp: Interpolation,
}

impl<T: Clone> Animate for Tweener<T> {
    type Output = T;
    fn tween(&self, frame: usize) -> T {
        (self.f)(self.interp.interpolate(frame), self.src.clone())
    }
    fn done(&self, frame: usize) -> bool { self.interp.done(frame) }
}

pub enum Interpolation {
    Linear { start: usize, len: usize },
}

impl Interpolation {
    pub fn interpolate(&self, frame: usize) -> f32 { Self::clamp(self.raw(frame)) }

    pub fn done(&self, frame: usize) -> bool { self.raw(frame) > 1.0 }

    fn raw(&self, frame: usize) -> f32 {
        match *self {
            Interpolation::Linear { start, len } => (frame - start) as f32 / len as f32,
        }
    }

    fn clamp(raw: f32) -> f32 {
        if raw < 0.0 {
            0.0
        } else if raw > 1.0 {
            1.0
        } else {
            raw
        }
    }
}