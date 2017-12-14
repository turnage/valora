use geom::Point;
use rand;

pub fn sparkles(n: usize, scale: f32) -> Vec<Point> {
    (0..n).map(|_| rand::random::<Point>() * scale).collect()
}