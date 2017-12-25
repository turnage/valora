use geom::{Distance, SubdivideEdges, Translate};
use lyon::tessellation::{math::Point2D, FillVertex};
use rand::{Rand, Rng};
use std::ops::*;

#[derive(Debug, Copy, Clone)]
pub struct Point {
    pub x: f32,
    pub y: f32,
}

impl Default for Point {
    fn default() -> Self { Self::center() }
}

impl Add for Point {
    type Output = Self;
    fn add(self, rhs: Self) -> Self { Self { x: self.x + rhs.x, y: self.y + rhs.y } }
}

impl AddAssign for Point {
    fn add_assign(&mut self, rhs: Self) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}

impl Sub for Point {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self { Self { x: self.x - rhs.x, y: self.y - rhs.y } }
}

impl SubAssign for Point {
    fn sub_assign(&mut self, rhs: Self) {
        self.x -= rhs.x;
        self.y -= rhs.y;
    }
}

impl Mul<Point> for Point {
    type Output = Self;

    fn mul(self, rhs: Point) -> Self { Self { x: self.x * rhs.x, y: self.y * rhs.y } }
}

impl MulAssign<Point> for Point {
    fn mul_assign(&mut self, rhs: Point) {
        self.x *= rhs.x;
        self.y *= rhs.y;
    }
}

impl Mul<f32> for Point {
    type Output = Self;

    fn mul(self, rhs: f32) -> Self { Self { x: self.x * rhs, y: self.y * rhs } }
}

impl MulAssign<f32> for Point {
    fn mul_assign(&mut self, rhs: f32) {
        self.x *= rhs;
        self.y *= rhs;
    }
}

impl Div<f32> for Point {
    type Output = Self;
    fn div(self, rhs: f32) -> Self { self * (1.0 / rhs) }
}

impl SubdivideEdges for Vec<Point> {
    fn subdivide_edges(self) -> Self {
        let p1s = self.iter();
        let p2s = self.iter().skip(1);
        let pairs = p1s.zip(p2s);
        let mut pairs: Vec<Point> = pairs
            .flat_map(|(p1, p2)| {
                          let midpoint = *p1 + ((*p2 - *p1) / 2.0);
                          vec![*p1, midpoint]
                      })
            .collect();
        if let Some(last) = self.last() {
            pairs.push(*last);
        }
        pairs
    }
}

impl Translate for Point {
    fn translate(self, delta: Point) -> Self { self + delta }
}

impl Distance<Self> for Point {
    fn distance(&self, dest: Self) -> f32 {
        let delta = (*self - dest).abs();
        (delta.x.powi(2) + delta.y.powi(2)).sqrt()
    }

    fn delta(&self, dest: Self) -> Self { (*self - dest).abs() }

    fn midpoint(&self, dest: Self) -> Self { *self + ((dest - *self) / 2.0) }

    fn manhattan_distance(&self, dest: Self) -> f32 {
        (self.x - dest.x).abs() + (self.y - dest.y).abs()
    }
}

impl Point {
    pub fn center() -> Point { Point { x: 0.5, y: 0.5 } }

    pub fn abs(self) -> Point { Point { x: self.x.abs(), y: self.y.abs() } }

    pub fn manhattan(self, point: Point) -> f32 {
        (self.x - point.x).abs() + (self.y - point.y).abs()
    }
}

impl Rand for Point {
    fn rand<R: Rng>(rng: &mut R) -> Point { Point { x: rng.next_f32(), y: rng.next_f32() } }
}

impl Into<Point2D<f32>> for Point {
    fn into(self) -> Point2D<f32> {
        Point2D::new(self.x, self.y)
    }
}

impl From<Point2D<f32>> for Point {
    fn from(point: Point2D<f32>) -> Point { Point { x: point.x, y: point.y } }
}

impl From<FillVertex> for Point {
    fn from(point: FillVertex) -> Point { point.position.into() }
}