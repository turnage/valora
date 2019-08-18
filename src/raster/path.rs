//! Module for working with paths and path segments.

use crate::geo::V2;
use failure::Fail;
use std::convert::*;

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
enum Error {
    #[fail(display = "Vertical segment is an invalid element.")]
    VerticalSegmentIsInvalidElement,
}

impl MonotonicSegment {
    fn try_from(src: impl TryInto<MonotonicElement, Error = Error>) -> Result<Self, Error> {
        Ok(MonotonicSegment {
            source: src.try_into()?,
        })
    }

    pub fn sample(&self, t: f64) -> Option<f64> {
        match self.source {
            MonotonicElement::LineSegment { m, b, bounds } => {
                if bounds.left <= t && t < bounds.right {
                    Some(t * m + b)
                } else {
                    None
                }
            }
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
    LineSegment { m: f64, b: f64, bounds: Bounds },
}

enum Slope {
    M(f64),
    Vertical { x: f64 },
}

impl TryFrom<(V2, V2)> for MonotonicElement {
    type Error = Error;
    fn try_from((a, b): (V2, V2)) -> Result<Self, Self::Error> {
        if a.x == b.x {
            Err(Error::VerticalSegmentIsInvalidElement)?;
        }

        let (left, right) = if a.x < b.x { (a, b) } else { (b, a) };
        let (top, bottom) = if a.y > b.y { (a.y, b.y) } else { (b.y, a.y) };
        let dx = right.x - left.x;
        let dy = right.y - left.y;
        let m = dx / dy;

        Ok(MonotonicElement::LineSegment {
            m,
            b: left.y - m * left.x,
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
            MonotonicElement::try_from((V2::new(3.0, 1.0), V2::new(4.0, 2.0))),
            Ok(MonotonicElement::LineSegment {
                m: 1.0,
                b: -2.0,
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
    fn monotonic_element_try_from_line_segment_invalid() {
        assert_eq!(
            MonotonicElement::try_from((V2::new(3.0, 1.0), V2::new(3.0, 2.0))),
            Err(Error::VerticalSegmentIsInvalidElement)
        );
    }

    #[test]
    fn monotonic_segment_sample_line_segment() -> Result<(), Error> {
        let element = MonotonicSegment::try_from((V2::new(3.0, 1.0), V2::new(4.0, 2.0)))?;
        assert_eq!(element.sample(3.0), Some(1.0));
        assert!(element
            .sample(3.5)
            .map(|d| (d - 1.5).abs() < 0.1)
            .unwrap_or(false));
        assert_eq!(element.sample(4.0), None);

        Ok(())
    }
}
