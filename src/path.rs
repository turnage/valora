//! Path types and tools.

use crate::P2;
use lyon_path::PathEvent;

/// An adapter for iterators over points that implements `Path`.
#[derive(Debug, Copy, Clone)]
pub struct FlatIterPath<I> {
    src: I,
    last: Option<P2>,
    first: Option<P2>,
    closed: bool,
}

impl<I> FlatIterPath<I>
where
    I: Iterator<Item = P2>,
{
    pub fn new(src: I, closed: bool) -> Self {
        Self {
            src,
            last: None,
            first: None,
            closed,
        }
    }
}

impl<I> Iterator for FlatIterPath<I>
where
    I: Iterator<Item = P2>,
{
    type Item = PathEvent;
    fn next(&mut self) -> Option<Self::Item> {
        let p = match self.src.next() {
            Some(p) => p,
            None => match (self.first.take(), self.last.take()) {
                (Some(first), Some(last)) => {
                    return Some(PathEvent::End {
                        last,
                        first,
                        close: self.closed,
                    })
                }
                _ => return None,
            },
        };

        let result = if let Some(last) = self.last {
            Some(PathEvent::Line { from: last, to: p })
        } else {
            assert!(self.first.is_none());
            self.first = Some(p);
            Some(PathEvent::Begin { at: p })
        };

        self.last = Some(p);
        result
    }
}
