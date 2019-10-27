//! Module for working with paths and path segments.

use crate::amicola::V2;
use failure::Fail;
use std::convert::*;

#[derive(Debug)]
pub struct MonotonicSegment {
    source: MonotonicElement,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Bounds {
    pub left: f32,
    pub right: f32,
    pub top: f32,
    pub bottom: f32,
}

#[derive(Debug, Fail, PartialEq)]
pub enum Error {
    #[fail(display = "Vertical segment is an invalid element.")]
    HorizontalSegmentIsInvalidElement,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Intersection {
    /// Where on the excluded axis (x or y) the intersection occurs.
    pub axis: f32,
    /// Where along the segment [0.0, 1.0] the intersection occurs.
    pub t: f32,
}

impl MonotonicSegment {
    pub fn try_from(src: impl TryInto<MonotonicElement, Error = Error>) -> Result<Self, Error> {
        Ok(MonotonicSegment {
            source: src.try_into()?,
        })
    }

    pub fn sample_y(&self, y: f32) -> Option<Intersection> {
        match self.source {
            MonotonicElement::LineSegment {
                m,
                bounds,
                start,
                length,
                ..
            } => {
                if bounds.bottom <= y && y <= bounds.top {
                    match m {
                        Slope::Vertical => Some(Intersection {
                            axis: bounds.right,
                            t: (y - bounds.bottom) / (bounds.top - bounds.bottom),
                        }),
                        Slope::Defined { m, b } => {
                            let x = (y - b) / m;
                            let p = V2::new(x, y);
                            Some(Intersection {
                                axis: x,
                                t: (p - start).norm() / length,
                            })
                        }
                    }
                } else {
                    None
                }
            }
        }
    }

    pub fn sample_x(&self, x: f32) -> Option<Intersection> {
        match self.source {
            MonotonicElement::LineSegment {
                m,
                bounds,
                start,
                length,
                ..
            } => {
                if bounds.left <= x && x <= bounds.right {
                    match m {
                        Slope::Vertical => None,
                        Slope::Defined { m, b } => {
                            let y = m * x + b;
                            let p = V2::new(x, y);
                            Some(Intersection {
                                axis: y,
                                t: (p - start).norm() / length,
                            })
                        }
                    }
                } else {
                    None
                }
            }
        }
    }

    pub fn sample_t(&self, t: f32) -> Option<V2> {
        if t < 0.0 || t > 1.0 {
            return None;
        }

        match self.source {
            MonotonicElement::LineSegment { start, end, .. } => Some((end - start) * t + start),
        }
    }

    pub fn bookends(&self) -> (V2, V2) {
        match self.source {
            MonotonicElement::LineSegment { start, end, .. } => (start, end),
        }
    }

    pub fn bounds(&self) -> &Bounds {
        match &self.source {
            MonotonicElement::LineSegment { bounds, .. } => bounds,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum MonotonicElement {
    LineSegment {
        m: Slope,
        bounds: Bounds,
        start: V2,
        end: V2,
        length: f32,
    },
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Slope {
    Vertical,
    Defined { m: f32, b: f32 },
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
            start: *a,
            end: *b,
            length: (a - b).norm(),
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
                },
                start: V2::new(3.0, 1.0),
                end: V2::new(4.0, 2.0),
                length: 1.4142135623730951,
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
                },
                start: V2::new(3.0, 1.0),
                end: V2::new(4.0, 3.0),
                length: 2.23606797749979,
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
                },
                start: V2::new(0.0, 0.0),
                end: V2::new(0.0, 100.0),
                length: 100.0,
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
                },
                start: V2::new(0.0, 100.0),
                end: V2::new(100.0, 0.0),
                length: 141.4213562373095,
            })
        );

        assert_eq!(
            MonotonicElement::try_from((&V2::new(100.0, 0.0), &V2::new(0.0, 0.0))),
            Err(Error::HorizontalSegmentIsInvalidElement)
        );
    }
}
