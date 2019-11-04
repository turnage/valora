//! Rectangles.

use crate::space::V2;

pub struct Rect {
    pub bottom_left: V2,
    pub width: f32,
    pub height: f32,
}

impl Rect {
    pub fn center(&self) -> V2 {
        V2::new(
            self.bottom_left.x + self.width / 2.,
            self.bottom_left.y + self.height / 2.,
        )
    }
}
