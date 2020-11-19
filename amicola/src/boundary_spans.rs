use crate::Pixel;

struct BoundarySpans {
    boundaries: std::collections::btree_set::IntoIter<Pixel>,
    cached: Option<Pixel>,
}

#[derive(Debug, Clone)]
struct BoundarySpan {
    y: isize,
    xs: Range<isize>,
}

impl Iterator for BoundarySpans {
    type Item = BoundarySpan;
    fn next(&mut self) -> Option<Self::Item> {
        let (range_start, range_y) = match self.cached.take() {
            Some(Pixel { x, y }) => (x, y),
            None => {
                let Pixel { x, y } = self.boundaries.next()?;
                (x, y)
            }
        };

        let mut range_end = range_start + 1;
        while let Some(Pixel { x, y }) = self.boundaries.next() {
            if y != range_y || x != range_end {
                self.cached = Some(Pixel { x, y });
                break;
            }

            range_end += 1;
        }

        Some(BoundarySpan {
            y: range_y,
            xs: range_start..range_end,
        })
    }
}
