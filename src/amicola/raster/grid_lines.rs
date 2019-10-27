//! Descriptions of a raster grid.

use super::path::*;

pub enum GridLinesIter<'a> {
    Bounds(&'a Bounds),
}

impl<'a> GridLinesIter<'a> {
    pub fn horizontal(&self) -> impl Iterator<Item = isize> {
        match self {
            GridLinesIter::Bounds(bounds) => Self::inclusive_iter(bounds.bottom, bounds.top),
        }
    }

    pub fn vertical(&self) -> impl Iterator<Item = isize> {
        match self {
            GridLinesIter::Bounds(bounds) => Self::inclusive_iter(bounds.left, bounds.right),
        }
    }

    fn inclusive_iter(startf: f32, endf: f32) -> impl Iterator<Item = isize> {
        let start = startf.floor() as isize;
        let end = endf.ceil() as isize;

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

        assert_eq!(iter.horizontal().collect::<Vec<isize>>(), vec![1, 2, 3, 4]);
    }

    #[test]
    fn vertical() {
        let iter = GridLinesIter::Bounds(&Bounds {
            left: 1.1,
            right: 3.1,
            top: 0.0,
            bottom: 0.0,
        });

        assert_eq!(iter.vertical().collect::<Vec<isize>>(), vec![1, 2, 3, 4]);
    }
}
