use point::Point;

pub trait Centered {
    fn center(&self) -> Point;
}

impl Centered for Vec<Point> {
    fn center(&self) -> Point {
        use properties::Distance;
        use std::f32;

        let mut min = Point { x: f32::MAX, y: f32::MAX };
        let mut max = Point { x: f32::MIN, y: f32::MIN };
        for v in self {
            if v.x < min.x {
                min.x = v.x;
            }
            if v.y < min.y {
                min.y = v.y;
            }
            if v.x > max.x {
                max.x = v.x;
            }
            if v.y > max.y {
                max.y = v.y;
            }
        }
        min.midpoint(&max)
    }
}
