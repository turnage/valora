//! Path types and tools.

use crate::P2;
use lyon_geom::LineSegment;
use lyon_path::PathEvent;

/// An adapter for path iterators that implements `Path` and closes the path at the end.
#[derive(Clone)]
pub struct ClosedPath<P> {
    src: P,
    done: bool,
}

impl<P> From<P> for ClosedPath<P>
where
    P: Iterator<Item = PathEvent> + Clone,
{
    fn from(src: P) -> Self { Self { src, done: false } }
}

impl<P> Iterator for ClosedPath<P>
where
    P: Iterator<Item = PathEvent>,
{
    type Item = PathEvent;
    fn next(&mut self) -> Option<Self::Item> {
        match self.src.next() {
            Some(e) => Some(e),
            None if !self.done => {
                self.done = true;
                Some(PathEvent::Close(LineSegment {
                    from: P2::new(0., 0.),
                    to: P2::new(0., 0.),
                }))
            }
            _ => None,
        }
    }
}

/// An adapter for iterators over points that implements `Path`.
#[derive(Debug, Copy, Clone)]
pub struct FlatIterPath<I> {
    src: I,
    last: Option<P2>,
}

impl<I> From<I> for FlatIterPath<I>
where
    I: Iterator<Item = P2>,
{
    fn from(src: I) -> Self { Self { src, last: None } }
}

impl<I> Iterator for FlatIterPath<I>
where
    I: Iterator<Item = P2>,
{
    type Item = PathEvent;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(last) = self.last.take() {
            let p = self.src.next()?;
            self.last = Some(p);
            Some(PathEvent::Line(LineSegment { from: last, to: p }))
        } else {
            let p = self.src.next()?;
            self.last = Some(p);
            Some(PathEvent::MoveTo(p))
        }
    }
}
