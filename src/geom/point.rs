use errors;
use geom::transforms::SubdivideEdges;
use lyon::tessellation::{FillVertex, StrokeVertex};
use lyon::tessellation::math::Point2D;
use num::traits::{Num, NumOps, Signed};
use num::traits::identities::{One, Zero};
use properties::{Centered, Distance};
use rand::{Rand, Rng};
use rand::distributions::{IndependentSample, Normal, Sample};
use std::ops::*;
use transforms::{Scale, Translate};

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Point {
    pub x: f32,
    pub y: f32,
}

impl Default for Point {
    fn default() -> Self { Self::center() }
}

impl Sample<Point> for Normal {
    fn sample<R: Rng>(&mut self, rng: &mut R) -> Point {
        let (x, y): (f64, f64) = (self.sample(rng), self.sample(rng));
        Point { x: x as f32, y: y as f32 }
    }
}

impl IndependentSample<Point> for Normal {
    fn ind_sample<R: Rng>(&self, rng: &mut R) -> Point {
        let (x, y): (f64, f64) = (self.ind_sample(rng), self.ind_sample(rng));
        Point { x: x as f32, y: y as f32 }
    }
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

impl Div<Point> for Point {
    type Output = Self;
    fn div(self, rhs: Self) -> Self { Self { x: self.x / rhs.y, y: self.y / rhs.y } }
}

impl Div<f32> for Point {
    type Output = Self;
    fn div(self, rhs: f32) -> Self { self * (1.0 / rhs) }
}

impl Rem for Point {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self { Self { x: rhs.x % self.x, y: rhs.y % self.y } }
}

impl Neg for Point {
    type Output = Self;
    fn neg(self) -> Self { Self { x: -self.x, y: -self.y } }
}

impl Zero for Point {
    fn zero() -> Self { Self { x: 0.0, y: 0.0 } }
    fn is_zero(&self) -> bool { *self == Self::zero() }
}

impl One for Point {
    fn one() -> Self { Self { x: 1.0, y: 1.0 } }
}

impl NumOps for Point {}

impl Num for Point {
    type FromStrRadixErr = errors::Error;
    fn from_str_radix(str: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
        let v: f32 = f32::from_str_radix(str, radix)?;
        Ok(Self { x: v, y: v })
    }
}

impl Signed for Point {
    fn abs(&self) -> Self { Self { x: self.x.abs(), y: self.y.abs() } }
    fn abs_sub(&self, other: &Self) -> Self { (*self - *other).abs() }
    fn signum(&self) -> Self { Point { x: self.x.signum(), y: self.y.signum() } }
    fn is_positive(&self) -> bool { self.x.is_positive() && self.y.is_positive() }
    fn is_negative(&self) -> bool { self.x.is_negative() && self.y.is_negative() }
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

impl Scale for Vec<Point> {
    fn scale(self, scale: f32) -> Self {
        let centroid = self.centroid();
        let deltas: Vec<Point> = self.iter().map(|p| *p - centroid).collect();
        self.into_iter()
            .zip(deltas.into_iter())
            .map(|(p, d)| p + (d * scale))
            .collect()
    }
}

impl Translate for Point {
    fn translate(self, delta: Point) -> Self { self + delta }
}

impl Distance<Self> for Point {
    fn distance(&self, dest: &Self) -> f32 {
        let delta = (*self - *dest).abs();
        (delta.x.powi(2) + delta.y.powi(2)).sqrt()
    }

    fn delta(&self, dest: &Self) -> Self { (*self - *dest).abs() }

    fn midpoint(&self, dest: &Self) -> Self { *self + ((*dest - *self) / 2.0) }

    fn manhattan_distance(&self, dest: &Self) -> f32 {
        (self.x - dest.x).abs() + (self.y - dest.y).abs()
    }

    fn sign_to(&self, dest: &Self) -> Point {
        use num::Signed;
        (*dest - *self).signum()
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
    fn into(self) -> Point2D<f32> { Point2D::new(self.x, self.y) }
}

impl From<Point2D<f32>> for Point {
    fn from(point: Point2D<f32>) -> Point { Point { x: point.x, y: point.y } }
}

impl From<FillVertex> for Point {
    fn from(point: FillVertex) -> Point { point.position.into() }
}

impl From<StrokeVertex> for Point {
    fn from(point: StrokeVertex) -> Point { point.position.into() }
}