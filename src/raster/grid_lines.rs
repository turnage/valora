//! Descriptions of a raster grid.

use super::path::*;

pub enum GridLinesIter<'a> {
    Bounds(&'a Bounds),
}

impl<'a> GridLinesIter<'a> {
    pub fn horizontal(&self) -> impl Iterator<Item = usize> {
        match self {
            GridLinesIter::Bounds(bounds) => Self::inclusive_iter(bounds.bottom, bounds.top),
        }
    }

    fn inclusive_iter(startf: f64, endf: f64) -> impl Iterator<Item = usize> {
        let start = startf.floor() as usize;
        let end = endf.ceil() as usize;

        (start..=end)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn horizontal() {
        let iter = GridLinesIter::Bounds(&Bounds {
            left: 0.0,
            right: 0.0,
            top: 3.1,
            bottom: 1.1,
        });

        assert_eq!(iter.horizontal().collect::<Vec<usize>>(), vec![1, 2, 3, 4]);
    }
}
