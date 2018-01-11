use point::Point;
use rect::Rect;

pub trait Bounded {
    fn in_bounds(&self, point: Point) -> bool;
    fn bounding_box(&self) -> Rect;
}
