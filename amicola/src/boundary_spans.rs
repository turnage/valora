use crate::Pixel;
use std::ops::Range;

pub(crate) struct SpanIter<I> {
    boundaries: I,
    cached: Option<Pixel>,
}

impl<I> SpanIter<I>
where
    I: Iterator<Item = Pixel>,
{
    pub(crate) fn from_boundaries(boundaries: I) -> Self {
        Self {
            boundaries,
            cached: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BoundarySpan {
    pub y: isize,
    pub xs: Range<isize>,
}

impl<I> Iterator for SpanIter<I>
where
    I: Iterator<Item = Pixel>,
{
    type Item = BoundarySpan;
    fn next(&mut self) -> Option<Self::Item> {
        let start = self.cached.take().or_else(|| self.boundaries.next())?;

        let mut span_length = 1;
        self.cached = self.boundaries.find(|Pixel { x, y }| {
            let contiguous = *y == start.y && *x == start.x + span_length;
            if contiguous {
                span_length += 1;
            }

            !contiguous
        });

        Some(BoundarySpan {
            y: start.y,
            xs: start.x..(start.x + span_length),
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn same_row() {
        let spans = SpanIter::from_boundaries(
            vec![
                Pixel { x: 0, y: 0 },
                Pixel { x: 1, y: 0 },
                Pixel { x: 2, y: 0 },
                Pixel { x: 4, y: 0 },
            ]
            .into_iter(),
        );

        assert_eq!(
            spans.collect::<Vec<BoundarySpan>>(),
            vec![
                BoundarySpan { y: 0, xs: 0..3 },
                BoundarySpan { y: 0, xs: 4..5 }
            ]
        );
    }

    #[test]
    fn new_row() {
        let spans = SpanIter::from_boundaries(
            vec![
                Pixel { x: 0, y: 0 },
                Pixel { x: 1, y: 0 },
                Pixel { x: 2, y: 0 },
                Pixel { x: 3, y: 1 },
                Pixel { x: 5, y: 1 },
                Pixel { x: 6, y: 1 },
            ]
            .into_iter(),
        );

        assert_eq!(
            spans.collect::<Vec<BoundarySpan>>(),
            vec![
                BoundarySpan { y: 0, xs: 0..3 },
                BoundarySpan { y: 1, xs: 3..4 },
                BoundarySpan { y: 1, xs: 5..7 }
            ]
        );
    }
}
