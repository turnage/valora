//! Descriptions of a raster grid.

use arrayvec::ArrayVec;
use euclid::{Point2D, Rect, UnknownUnit};
use float_ord::FloatOrd;
use itertools::{Itertools, Position};
use lyon_geom::LineSegment;
use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::convert::TryFrom;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, PartialOrd, Ord)]
pub enum Axis {
    X(isize),
    Y(isize),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct RawHit {
    pub t: f64,
    pub axis: Option<Axis>,
}

impl PartialOrd for RawHit {
    fn partial_cmp(&self, other: &RawHit) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for RawHit {
    fn cmp(&self, other: &RawHit) -> Ordering {
        match FloatOrd(self.t).cmp(&FloatOrd(other.t)) {
            // We allow replacement in the case that the current hit is a dummy
            // marker for the start and end of the segment.
            Ordering::Equal if self.axis.is_some() => self.axis.cmp(&other.axis),
            ordering => ordering,
        }
    }
}

impl Eq for RawHit {}

pub fn horizontal_grid_lines<U>(bounds: Rect<f64, U>) -> impl Iterator<Item = isize> {
    inclusive_iter(bounds.min_y(), bounds.max_y())
}

pub fn vertical_grid_lines<U>(bounds: Rect<f64, U>) -> impl Iterator<Item = isize> {
    inclusive_iter(bounds.min_x(), bounds.max_x())
}

fn inclusive_iter(startf: f64, endf: f64) -> impl Iterator<Item = isize> {
    let start = startf.floor() as isize;
    let end = endf.ceil() as isize;

    (start..=end)
}

pub fn raw_hits(segment: LineSegment<f64>) -> Vec<RawHit> {
    let bounds = segment.bounding_rect();
    let mut hits = vec![];
    hits.push(RawHit { t: 0., axis: None });
    hits.push(RawHit { t: 1., axis: None });

    {
        let mut insert = |hit: RawHit| match hit.t {
            0. => hits[0] = hit,
            1. => hits[1] = hit,
            _ => hits.push(hit),
        };

        for vertical_line in vertical_grid_lines(bounds) {
            let x = vertical_line as f64;
            if let Some(t) = segment.vertical_line_intersection_t(x) {
                insert(RawHit {
                    t,
                    axis: Some(Axis::Y(vertical_line)),
                });
            }
        }

        for horizontal_line in horizontal_grid_lines(bounds) {
            let y = horizontal_line as f64;
            if let Some(t) = segment.horizontal_line_intersection_t(y) {
                insert(RawHit {
                    t,
                    axis: Some(Axis::X(horizontal_line)),
                });
            }
        }
    }

    hits.sort_by_key(|h| FloatOrd(h.t));
    hits
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Hit {
    pub x: f64,
    pub pixel: Pixel,
}

impl PartialOrd for Hit {
    fn partial_cmp(&self, other: &Hit) -> Option<Ordering> {
        match self.pixel.y.cmp(&other.pixel.y) {
            Ordering::Equal => Some(FloatOrd(self.x).cmp(&FloatOrd(other.x))),
            o => Some(o),
        }
    }
}

impl Ord for Hit {
    fn cmp(&self, other: &Hit) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Eq for Hit {}

impl Hash for Hit {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pixel.x.hash(state);
        self.pixel.y.hash(state);
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub struct Pixel {
    pub x: isize,
    pub y: isize,
}

impl PartialOrd for Pixel {
    fn partial_cmp(&self, other: &Pixel) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Pixel {
    fn cmp(&self, other: &Pixel) -> Ordering {
        match self.y.cmp(&other.y) {
            Ordering::Equal => self.x.cmp(&other.x),
            other => other,
        }
    }
}

pub fn scanline_entries(segment: LineSegment<f64>) -> (BTreeSet<Pixel>, Vec<Hit>) {
    if segment.from.y == segment.to.y {
        return (BTreeSet::new(), vec![]);
    }

    let mut raw_hits = raw_hits(segment);

    let (boundaries, mut hits) = {
        let mut hits = vec![];
        let mut insert_hit = |hit: RawHit, p: Point2D<f64, UnknownUnit>| {
            if let Some(Axis::X(y)) = hit.axis {
                hits.push(Hit {
                    x: p.x,
                    pixel: Pixel {
                        y,
                        x: p.x.floor() as isize,
                    },
                });
            }
        };
        let boundaries: BTreeSet<Pixel> = raw_hits
            .into_iter()
            .tuple_windows::<(_, _)>()
            .with_position()
            .flat_map(|hit_pair| {
                let (h1, h2) = hit_pair.into_inner();

                let p1 = segment.sample(h1.t);
                let p2 = segment.sample(h2.t);

                match hit_pair {
                    Position::Last(_) => {
                        insert_hit(h1, p1);
                        insert_hit(h2, p2);
                    }
                    _ => {
                        insert_hit(h1, p1);
                    }
                }

                let to_pixel = |p: Point2D<f64, UnknownUnit>| {
                    let x = p.x.floor() as isize;
                    let y = p.y.floor() as isize;
                    Pixel { x, y }
                };
                let midpoint = (p1 + p2.to_vector()) / 2.;
                ArrayVec::from([p1, p2, midpoint]).into_iter().map(to_pixel)
            })
            .collect();

        (boundaries, hits)
    };

    hits.sort();

    (boundaries, hits)
}

#[cfg(test)]
mod test {
    use super::*;
    use lyon_path::math::point;
    use maplit::btreeset;

    #[test]
    fn simple() {
        let hits = raw_hits(LineSegment {
            from: point(0.5, 0.5),
            to: point(1.5, 1.5),
        });

        assert_eq!(
            hits,
            vec![
                RawHit { t: 0.0, axis: None },
                RawHit {
                    t: 0.5,
                    axis: Some(Axis::Y(1)),
                },
                RawHit {
                    t: 0.5,
                    axis: Some(Axis::X(1)),
                },
                RawHit { t: 1.0, axis: None },
            ]
        );
    }

    #[test]
    fn axis_aligned() {
        let hits = raw_hits(LineSegment {
            from: point(0., 0.),
            to: point(1., 1.),
        });

        assert_eq!(
            hits,
            vec![
                RawHit {
                    t: 0.,
                    axis: Some(Axis::X(0)),
                },
                RawHit {
                    t: 1.,
                    axis: Some(Axis::X(1)),
                },
            ]
        );
    }

    #[test]
    fn simple_triangle() {
        let a = point(0., 0.);
        let b = point(0., 2.);
        let c = point(2., 0.);

        let ab = LineSegment { from: a, to: b };
        let bc = LineSegment { from: b, to: c };
        let ca = LineSegment { from: c, to: a };

        let (boundaries_ab, hits_ab) = scanline_entries(ab);
        assert_eq!(
            boundaries_ab,
            btreeset![
                Pixel { x: 0, y: 0 },
                Pixel { x: 0, y: 1 },
                Pixel { x: 0, y: 2 }
            ]
        );
        assert_eq!(
            hits_ab,
            vec![
                Hit {
                    pixel: Pixel { x: 0, y: 0 },
                    x: 0.0,
                },
                Hit {
                    x: 0.0,
                    pixel: Pixel { x: 0, y: 1 },
                },
                Hit {
                    x: 0.0,
                    pixel: Pixel { x: 0, y: 2 },
                },
            ]
        );

        let (boundaries_bc, hits_bc) = scanline_entries(bc);
        assert_eq!(
            boundaries_bc,
            btreeset![
                Pixel { x: 0, y: 1 },
                Pixel { x: 0, y: 2 },
                Pixel { x: 1, y: 1 },
                Pixel { x: 1, y: 0 },
                Pixel { x: 2, y: 0 },
            ]
        );
        assert_eq!(
            hits_bc,
            vec![
                Hit {
                    x: 2.0,
                    pixel: Pixel { x: 2, y: 0 },
                },
                Hit {
                    x: 1.0,
                    pixel: Pixel { x: 1, y: 1 },
                },
                Hit {
                    pixel: Pixel { x: 0, y: 2 },
                    x: 0.0,
                },
            ]
        );

        let (boundaries_ca, hits_ca) = scanline_entries(ca);
        assert_eq!(
            boundaries_ca,
            btreeset![
                // The scanline is horizontal.
            ]
        );
        assert_eq!(
            hits_ca,
            vec![
                // The scanline is horizontal.
            ]
        );
    }

    #[test]
    fn assumptions() {
        let axis_aligned = LineSegment {
            from: point(0., 0.),
            to: point(1., 1.),
        };

        assert_eq!(
            axis_aligned.vertical_line_intersection_t(0.),
            Some(0.),
            "Line Segment intersections are not inclusive."
        );

        let axis_aligned = LineSegment {
            from: point(0., 2.),
            to: point(2., 0.),
        };

        assert_eq!(
            axis_aligned.vertical_line_intersection_t(2.),
            Some(1.),
            "Line Segment intersections are not inclusive."
        );
    }
}
