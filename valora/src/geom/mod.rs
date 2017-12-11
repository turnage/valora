pub mod poly;

use lyon::math::TypedPoint2D;
use lyon::tessellation::FillVertex;

#[derive(Debug, Copy, Clone)]
pub struct Point {
    pub x: f32,
    pub y: f32,
}

impl Point {
    const WORLD_OFFSET: f32 = 1.0;
    const WORLD_FACTOR: f32 = 2.0;

    // OpenGL places the origin in the center of the screen. We rescale
    // and offset vertices one world unit so the origin is in the bottom
    // left, and y and x point up and right respectively. If you think
    // it should be done differently, you are wrong.
    fn fix_coord(coord: f32) -> f32 {
        (coord * Point::WORLD_FACTOR) - Point::WORLD_OFFSET
    }

    fn restore_coord(coord: f32) -> f32 {
        (coord - Point::WORLD_OFFSET) / Point::WORLD_FACTOR
    }
}

impl<U> Into<TypedPoint2D<f32, U>> for Point {
    fn into(self) -> TypedPoint2D<f32, U> {
        TypedPoint2D::new(Point::fix_coord(self.x), Point::fix_coord(self.y))
    }
}

impl<U> From<TypedPoint2D<f32, U>> for Point {
    fn from(point: TypedPoint2D<f32, U>) -> Point {
        Point {
            x: Point::restore_coord(point.x),
            y: Point::restore_coord(point.y),
        }
    }
}


impl From<FillVertex> for Point {
    fn from(point: FillVertex) -> Point {
        point.position.into()
    }
}
