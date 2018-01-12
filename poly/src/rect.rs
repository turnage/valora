//! Rectangle definition and implementations.

use point::Point;

#[derive(Clone, Debug, Copy, PartialEq)]
pub struct Rect {
    pub bottom_left: Point,
    pub width:       f32,
    pub height:      f32,
}

impl Rect {
    pub fn square(bottom_left: Point, size: f32) -> Self { Self::new(bottom_left, size, size) }

    pub fn frame() -> Self { Self::square(Point { x: 0.0, y: 0.0 }, 1.0) }

    pub fn new(bottom_left: Point, width: f32, height: f32) -> Self {
        Self {
            height,
            width,
            bottom_left,
        }
    }

    pub fn center(&self) -> Point {
        self.bottom_left + Point {
            x: self.width / 2.0,
            y: self.height / 2.0,
        }
    }

    pub fn vertices(&self) -> Vec<Point> {
        vec![
            self.bottom_left,
            Point {
                x: self.bottom_left.x,
                y: self.bottom_left.y + self.height,
            },
            Point {
                x: self.bottom_left.x + self.width,
                y: self.bottom_left.y + self.height,
            },
            Point {
                x: self.bottom_left.x + self.width,
                y: self.bottom_left.y,
            },
        ]
    }

    pub fn scale(&self, scale: f32) -> Self {
        let center = self.center();
        Self {
            bottom_left: center + (self.bottom_left - center) * scale,
            width:       scale * self.width,
            height:      scale * self.height,
        }
    }
}
