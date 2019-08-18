//! Descriptions of a raster grid.

use super::path::*;

pub enum GridLinesIter {
    Bounds(Bounds),
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct GridLine {
    t: f64,
    fill: Fill,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Fill {
    Full,
    Partial,
}

impl GridLinesIter {
    pub fn horizontal(&self) -> impl Iterator<Item = GridLine> {
        match self {
            GridLinesIter::Bounds(bounds) => Self::inclusive_iter(bounds.bottom, bounds.top),
        }
    }

    pub fn vertical(&self) -> impl Iterator<Item = GridLine> {
        match self {
            GridLinesIter::Bounds(bounds) => Self::inclusive_iter(bounds.left, bounds.right),
        }
    }

    fn inclusive_iter(startf: f64, endf: f64) -> impl Iterator<Item = GridLine> {
        let start = startf.floor() as usize;
        let end = endf.ceil() as usize;

        (start..=end).map(move |v| {
            let t = v as f64;
            GridLine {
                t,
                fill: if t < startf {
                    Fill::Partial
                } else if t > endf {
                    Fill::Partial
                } else {
                    Fill::Full
                },
            }
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn horizontal() {
        let iter = GridLinesIter::Bounds(Bounds {
            left: 0.0,
            right: 0.0,
            top: 3.1,
            bottom: 1.1,
        });

        assert_eq!(
            iter.horizontal().collect::<Vec<GridLine>>(),
            vec![
                GridLine {
                    t: 1.0,
                    fill: Fill::Partial
                },
                GridLine {
                    t: 2.0,
                    fill: Fill::Full
                },
                GridLine {
                    t: 3.0,
                    fill: Fill::Full
                },
                GridLine {
                    t: 4.0,
                    fill: Fill::Partial
                }
            ]
        );
    }
}
