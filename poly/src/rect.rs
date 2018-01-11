//! Rectangle definition and implementations.

use point::Point;
use poly::Poly;
use transforms::Translate;
use properties::{Bounded};

#[derive(Clone, Debug, Copy)]
pub struct Rect {
    pub bottom_left: Point,
    pub width: f32,
    pub height: f32,
}

impl Poly for Rect {
    fn vertices(&self) -> Vec<Point> {
        vec![self.bottom_left,
             Point { x: self.bottom_left.x, y: self.bottom_left.y + self.height },
             Point { x: self.bottom_left.x + self.width, y: self.bottom_left.y + self.height },
             Point { x: self.bottom_left.x + self.width, y: self.bottom_left.y }]
    }
}

impl Translate for Rect {
    fn translate(self, delta: Point) -> Self {
        Rect::new(self.bottom_left + delta, self.height, self.width)
    }
}

impl Bounded for Rect {
    fn in_bounds(&self, point: Point) -> bool {
        point.x >= self.bottom_left.x && point.x < self.bottom_left.x + self.width &&
        point.y >= self.bottom_left.y && point.y < self.bottom_left.y + self.height
    }
    fn bounding_box(&self) -> Rect { self.clone() }
}

impl Rect {
    pub fn square(bottom_left: Point, size: f32) -> Self { Self::new(bottom_left, size, size) }

    pub fn frame() -> Self { Self::square(Point { x: 0.0, y: 0.0 }, 1.0) }

    pub fn new(bottom_left: Point, width: f32, height: f32) -> Self {
        Self { height, width, bottom_left }
    }
}
