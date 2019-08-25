//! Module for working with paths and path segments.

use crate::geo::V2;
use failure::Fail;
use std::convert::*;

#[derive(Debug)]
pub struct MonotonicSegment {
    source: MonotonicElement,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Bounds {
    pub left: f64,
    pub right: f64,
    pub top: f64,
    pub bottom: f64,
}

#[derive(Debug, Fail, PartialEq)]
pub enum Error {
    #[fail(display = "Vertical segment is an invalid element.")]
    HorizontalSegmentIsInvalidElement,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Intersection {
    /// Where on the excluded axis (x or y) the intersection occurs.
    pub axis: f64,
    /// Where along the segment [0.0, 1.0] the intersection occurs.
    pub t: f64,
}

impl MonotonicSegment {
    pub fn try_from(src: impl TryInto<MonotonicElement, Error = Error>) -> Result<Self, Error> {
        Ok(MonotonicSegment {
            source: src.try_into()?,
        })
    }

    pub fn sample_y(&self, y: f64) -> Option<Intersection> {
        match self.source {
            MonotonicElement::LineSegment { m, bounds } => {
                if bounds.bottom <= y && y <= bounds.top {
                    match m {
                        Slope::Vertical => Some(Intersection {
                            axis: bounds.right,
                            t: (y - bounds.bottom) / (bounds.top - bounds.bottom),
                        }),
                        Slope::Defined { m, b } => {
                            let x = (y - b) / m;
                            Some(Intersection {
                                axis: x,
                                t: (x - bounds.left) / (bounds.right - bounds.left),
                            })
                        }
                    }
                } else {
                    None
                }
            }
        }
    }

    pub fn sample_x(&self, x: f64) -> Option<Intersection> {
        match self.source {
            MonotonicElement::LineSegment { m, bounds } => {
                if bounds.left <= x && x <= bounds.right {
                    match m {
                        Slope::Vertical => None,
                        Slope::Defined { m, b } => {
                            let y = m * x + b;
                            Some(Intersection {
                                axis: y,
                                t: (y - bounds.bottom) / (bounds.top - bounds.bottom),
                            })
                        }
                    }
                } else {
                    None
                }
            }
        }
    }

    pub fn bookends(&self) -> (V2, V2) {
        match self.source {
            MonotonicElement::LineSegment { m, bounds } => match m {
                Slope::Vertical => (
                    V2::new(bounds.left, bounds.bottom),
                    V2::new(bounds.right, bounds.top),
                ),
                Slope::Defined { m, b } => (
                    V2::new(bounds.left, m * bounds.left + b),
                    V2::new(bounds.right, m * bounds.right + b),
                ),
            },
        }
    }

    pub fn bounds(&self) -> &Bounds {
        match &self.source {
            MonotonicElement::LineSegment { bounds, .. } => bounds,
        }
    }
}

#[derive(Debug, PartialEq)]
enum MonotonicElement {
    LineSegment { m: Slope, bounds: Bounds },
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Slope {
    Vertical,
    Defined { m: f64, b: f64 },
}

impl<'a> TryFrom<(&'a V2, &'a V2)> for MonotonicElement {
    type Error = Error;
    fn try_from((a, b): (&'a V2, &'a V2)) -> Result<Self, Self::Error> {
        if a.y == b.y {
            Err(Error::HorizontalSegmentIsInvalidElement)?;
        }

        let (left, right) = if a.x < b.x { (a, b) } else { (b, a) };
        let (top, bottom) = if a.y > b.y { (a.y, b.y) } else { (b.y, a.y) };
        let dx = right.x - left.x;
        let dy = right.y - left.y;
        let m = dy / dx;
        let m = if dx == 0.0 {
            Slope::Vertical
        } else {
            Slope::Defined {
                b: left.y - m * left.x,
                m: dy / dx,
            }
        };

        Ok(MonotonicElement::LineSegment {
            m,
            bounds: Bounds {
                left: left.x,
                right: right.x,
                top,
                bottom,
            },
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn monotonic_element_try_from_line_segment_valid() {
        assert_eq!(
            MonotonicElement::try_from((&V2::new(3.0, 1.0), &V2::new(4.0, 2.0))),
            Ok(MonotonicElement::LineSegment {
                m: Slope::Defined { m: 1.0, b: -2.0 },
                bounds: Bounds {
                    left: 3.0,
                    right: 4.0,
                    top: 2.0,
                    bottom: 1.0
                }
            })
        );
    }

    #[test]
    fn monotonic_element_try_from_line_segment_valid_steep_slope() {
        assert_eq!(
            MonotonicElement::try_from((&V2::new(3.0, 1.0), &V2::new(4.0, 3.0))),
            Ok(MonotonicElement::LineSegment {
                m: Slope::Defined { m: 2.0, b: -5.0 },
                bounds: Bounds {
                    left: 3.0,
                    right: 4.0,
                    top: 3.0,
                    bottom: 1.0
                }
            })
        );
    }

    #[test]
    fn monotonic_element_try_from_line_segment_invalid() {
        assert_eq!(
            MonotonicElement::try_from((&V2::new(3.0, 1.0), &V2::new(6.0, 1.0))),
            Err(Error::HorizontalSegmentIsInvalidElement)
        );
    }

    #[test]
    fn monotonic_segment_sample_line_segment() -> Result<(), Error> {
        let segment = MonotonicSegment::try_from((&V2::new(3.0, 1.0), &V2::new(4.0, 2.0)))?;
        assert_eq!(
            segment.sample_y(1.0),
            Some(Intersection { axis: 3.0, t: 0.0 })
        );
        assert!(segment
            .sample_y(1.5)
            .map(|i| (i.axis - 3.5).abs() < 0.1)
            .unwrap_or(false));
        assert_eq!(
            segment.sample_y(2.0),
            Some(Intersection { axis: 4.0, t: 1.0 })
        );
        assert_eq!(segment.sample_y(2.1), None);

        Ok(())
    }

    #[test]
    fn monotonic_element_try_from_triangle_edges() {
        assert_eq!(
            MonotonicElement::try_from((&V2::new(0.0, 0.0), &V2::new(0.0, 100.0))),
            Ok(MonotonicElement::LineSegment {
                m: Slope::Vertical,
                bounds: Bounds {
                    left: 0.0,
                    right: 0.0,
                    top: 100.0,
                    bottom: 0.0
                }
            })
        );

        assert_eq!(
            MonotonicElement::try_from((&V2::new(0.0, 100.0), &V2::new(100.0, 0.0))),
            Ok(MonotonicElement::LineSegment {
                m: Slope::Defined { m: -1.0, b: 100.0 },
                bounds: Bounds {
                    left: 0.0,
                    right: 100.0,
                    top: 100.0,
                    bottom: 0.0
                }
            })
        );

        assert_eq!(
            MonotonicElement::try_from((&V2::new(100.0, 0.0), &V2::new(0.0, 0.0))),
            Err(Error::HorizontalSegmentIsInvalidElement)
        );
    }
}
