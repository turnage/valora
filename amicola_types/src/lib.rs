//! Point definition and trait implementations.

use num::traits::identities::{One, Zero};
use num::traits::{Num, Signed};
use std::ops::*;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Point {
    pub x: f32,
    pub y: f32,
}

impl Add for Point {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl AddAssign for Point {
    fn add_assign(&mut self, rhs: Self) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}

impl Sub for Point {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Self {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl SubAssign for Point {
    fn sub_assign(&mut self, rhs: Self) {
        self.x -= rhs.x;
        self.y -= rhs.y;
    }
}

impl Mul<Point> for Point {
    type Output = Self;

    fn mul(self, rhs: Point) -> Self {
        Self {
            x: self.x * rhs.x,
            y: self.y * rhs.y,
        }
    }
}

impl MulAssign<Point> for Point {
    fn mul_assign(&mut self, rhs: Point) {
        self.x *= rhs.x;
        self.y *= rhs.y;
    }
}

impl Mul<f32> for Point {
    type Output = Self;

    fn mul(self, rhs: f32) -> Self {
        Self {
            x: self.x * rhs,
            y: self.y * rhs,
        }
    }
}

impl MulAssign<f32> for Point {
    fn mul_assign(&mut self, rhs: f32) {
        self.x *= rhs;
        self.y *= rhs;
    }
}

impl Div<Point> for Point {
    type Output = Self;
    fn div(self, rhs: Self) -> Self {
        Self {
            x: self.x / rhs.y,
            y: self.y / rhs.y,
        }
    }
}

impl Div<f32> for Point {
    type Output = Self;
    fn div(self, rhs: f32) -> Self {
        self * (1.0 / rhs)
    }
}

impl Rem for Point {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self {
        Self {
            x: rhs.x % self.x,
            y: rhs.y % self.y,
        }
    }
}

impl Neg for Point {
    type Output = Self;
    fn neg(self) -> Self {
        Self {
            x: -self.x,
            y: -self.y,
        }
    }
}

impl Zero for Point {
    fn zero() -> Self {
        Self { x: 0.0, y: 0.0 }
    }
    fn is_zero(&self) -> bool {
        *self == Self::zero()
    }
}

impl One for Point {
    fn one() -> Self {
        Self { x: 1.0, y: 1.0 }
    }
}

impl Num for Point {
    type FromStrRadixErr = <f32 as Num>::FromStrRadixErr;
    fn from_str_radix(str: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
        let v: f32 = f32::from_str_radix(str, radix)?;
        Ok(Self { x: v, y: v })
    }
}

impl Signed for Point {
    fn abs(&self) -> Self {
        Self {
            x: self.x.abs(),
            y: self.y.abs(),
        }
    }
    fn abs_sub(&self, other: &Self) -> Self {
        (*self - *other).abs()
    }
    fn signum(&self) -> Self {
        Point {
            x: self.x.signum(),
            y: self.y.signum(),
        }
    }
    fn is_positive(&self) -> bool {
        self.x.is_positive() && self.y.is_positive()
    }
    fn is_negative(&self) -> bool {
        self.x.is_negative() && self.y.is_negative()
    }
}

impl Point {
    pub fn distance(&self, dest: &Self) -> f32 {
        let delta = (*self - *dest).abs();
        (delta.x.powi(2) + delta.y.powi(2)).sqrt()
    }

    pub fn delta(&self, dest: &Self) -> Self {
        (*self - *dest).abs()
    }

    pub fn midpoint(&self, dest: &Self) -> Self {
        *self + ((*dest - *self) / 2.0)
    }

    pub fn manhattan_distance(&self, dest: &Self) -> f32 {
        (self.x - dest.x).abs() + (self.y - dest.y).abs()
    }

    pub fn sign_to(&self, dest: &Self) -> Point {
        use num::Signed;
        (*dest - *self).signum()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
