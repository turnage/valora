//! Paint trait and implementations.

use crate::canvas::Canvas;
use lyon_path::PathEvent;

/// A trait for types which can be represented on a `Canvas`.
pub trait Paint {
    /// Paints self in the composition.
    fn paint(&self, canvas: &mut Canvas);
}

/// Paints a path with a fill pattern.
pub struct Filled<D>(pub D);

impl<P: Paint> Paint for Filled<P> {
    fn paint(&self, comp: &mut Canvas) {
        self.0.paint(comp);
        comp.fill();
    }
}

/// Paints a path with a stroke.
pub struct Stroked<D> {
    pub element: D,
    pub width: f32,
}

impl<P: Paint> Paint for Stroked<P> {
    fn paint(&self, comp: &mut Canvas) {
        self.element.paint(comp);
        comp.set_stroke_width(self.width);
        comp.stroke();
    }
}

impl<P> Paint for P
where
    P: Iterator<Item = PathEvent> + Clone,
{
    fn paint(&self, canvas: &mut Canvas) {
        self.clone().for_each(|p| match p {
            PathEvent::Line { to, .. } => canvas.line_to(to),
            PathEvent::Quadratic { ctrl, to, .. } => canvas.quadratic_to(ctrl, to),
            PathEvent::Cubic {
                ctrl1, ctrl2, to, ..
            } => canvas.cubic_to(ctrl1, ctrl2, to),
            PathEvent::Begin { at } => canvas.move_to(at),
            PathEvent::End { close, .. } if close => canvas.close_path(),
            _ => {}
        });
    }
}
