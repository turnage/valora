use poly::{Point, Rect};
use rand::Rng;

pub struct Sparkles(Vec<Point>);

pub fn sparkles<R: Rng>(n: usize, bounds: &Rect, rng: &mut R) -> Vec<Point> {
    (0..n)
        .map(|_| {
            Point {
                x: rng.gen_range(0.0, bounds.width),
                y: rng.gen_range(0.0, bounds.height),
            } + bounds.bottom_left
        })
        .collect()
}
