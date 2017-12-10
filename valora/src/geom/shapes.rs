use geom::poly::Poly;
use geom::Point;

pub fn square(top_left: Point, size: f32) -> Poly {
    Poly {
        vertices: vec![
            top_left,
            Point {
                x: top_left.x,
                y: top_left.y + size,
            },
            Point {
                x: top_left.x + size,
                y: top_left.y + size,
            },
            Point {
                x: top_left.x + size,
                y: top_left.y,
            },
        ],
    }
}
