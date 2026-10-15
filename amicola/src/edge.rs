//! Path outlines as line segments.

use kurbo::{PathEl, Point};
use std::cmp::Ordering;

/// A line segment of a path outline, oriented top to bottom.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Edge {
    pub top: f64,
    pub bottom: f64,
    pub x_min: f64,
    pub x_max: f64,
    /// The contribution of the edge to the winding number of points to its
    /// right: 1 if the segment travels down (+y), -1 if it travels up, and 0 if
    /// it is horizontal.
    pub winding: i32,
    x_top: f64,
    dxdy: f64,
}

impl Edge {
    pub fn new(from: Point, to: Point) -> Option<Self> {
        if !from.is_finite() || !to.is_finite() {
            return None;
        }

        let (upper, lower, winding) = match from.y.partial_cmp(&to.y)? {
            Ordering::Less => (from, to, 1),
            Ordering::Greater => (to, from, -1),
            Ordering::Equal => (from, to, 0),
        };
        let dxdy = match winding {
            0 => 0.,
            _ => (lower.x - upper.x) / (lower.y - upper.y),
        };

        Some(Self {
            top: upper.y,
            bottom: lower.y,
            x_min: from.x.min(to.x),
            x_max: from.x.max(to.x),
            winding,
            x_top: upper.x,
            dxdy,
        })
    }

    pub fn is_horizontal(&self) -> bool {
        self.winding == 0
    }

    /// Whether the edge crosses the horizontal line at `y`.
    ///
    /// Edges cover `[top, bottom)` so that the scanline through a vertex
    /// joining two edges crosses exactly one of them.
    pub fn crosses(&self, y: f64) -> bool {
        self.top <= y && y < self.bottom
    }

    /// The x coordinate where the edge crosses the horizontal line at `y`.
    pub fn x_at(&self, y: f64) -> f64 {
        self.x_top + (y - self.top) * self.dxdy
    }

    /// Whether any part of the edge lies at or below `y`.
    pub fn reaches(&self, y: f64) -> bool {
        self.bottom > y || (self.is_horizontal() && self.top >= y)
    }

    /// The horizontal extent of the part of the edge between `top` and
    /// `bottom`.
    pub fn x_extent_within(&self, top: f64, bottom: f64) -> (f64, f64) {
        if self.is_horizontal() {
            return (self.x_min, self.x_max);
        }

        let a = self.x_at(self.top.max(top));
        let b = self.x_at(self.bottom.min(bottom));
        (a.min(b).max(self.x_min), a.max(b).min(self.x_max))
    }
}

/// Flattens a path into edges, implicitly closing every subpath.
pub(crate) fn path_edges(path: impl IntoIterator<Item = PathEl>, tolerance: f64) -> Vec<Edge> {
    let mut builder = EdgeBuilder::default();
    kurbo::flatten(path, tolerance, |el| match el {
        PathEl::MoveTo(p) => builder.move_to(p),
        PathEl::LineTo(p) => builder.line_to(p),
        PathEl::ClosePath => builder.close(),
        PathEl::QuadTo(..) | PathEl::CurveTo(..) => unreachable!("flattened paths have no curves"),
    });
    builder.close();
    builder.edges
}

#[derive(Default)]
struct EdgeBuilder {
    edges: Vec<Edge>,
    start: Option<Point>,
    last: Option<Point>,
}

impl EdgeBuilder {
    fn move_to(&mut self, p: Point) {
        self.close();
        self.start = Some(p);
        self.last = Some(p);
    }

    fn line_to(&mut self, p: Point) {
        match self.last {
            Some(last) => self.edges.extend(Edge::new(last, p)),
            None => self.start = Some(p),
        }
        self.last = Some(p);
    }

    fn close(&mut self) {
        if let (Some(last), Some(start)) = (self.last, self.start) {
            if last != start {
                self.edges.extend(Edge::new(last, start));
            }
        }
        self.last = self.start;
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use kurbo::BezPath;

    #[test]
    fn orientation() {
        let down = Edge::new(Point::new(0., 0.), Point::new(2., 4.)).unwrap();
        assert_eq!((down.top, down.bottom, down.winding), (0., 4., 1));
        assert_eq!(down.x_at(2.), 1.);

        let up = Edge::new(Point::new(2., 4.), Point::new(0., 0.)).unwrap();
        assert_eq!((up.top, up.bottom, up.winding), (0., 4., -1));
        assert_eq!(up.x_at(2.), 1.);
    }

    #[test]
    fn half_open_crossing() {
        let edge = Edge::new(Point::new(0., 1.), Point::new(0., 3.)).unwrap();
        assert!(!edge.crosses(0.5));
        assert!(edge.crosses(1.));
        assert!(edge.crosses(2.9));
        assert!(!edge.crosses(3.));
    }

    #[test]
    fn non_finite_points_are_dropped() {
        assert_eq!(
            Edge::new(Point::new(f64::NAN, 0.), Point::new(1., 1.)),
            None
        );
    }

    #[test]
    fn subpaths_are_closed() {
        let mut path = BezPath::new();
        path.move_to((0., 0.));
        path.line_to((4., 0.));
        path.line_to((4., 4.));
        path.move_to((10., 10.));
        path.line_to((12., 10.));
        path.line_to((12., 12.));
        path.close_path();

        let windings: Vec<i32> = path_edges(path, 0.1).iter().map(|e| e.winding).collect();
        assert_eq!(windings, vec![0, 1, -1, 0, 1, -1]);
    }
}
