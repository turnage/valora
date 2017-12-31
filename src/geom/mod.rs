pub mod poly;
pub mod ellipse;
pub mod point;
pub mod line;
pub mod spawner;
pub mod transforms;

pub use self::ellipse::*;
pub use self::line::*;
pub use self::point::*;
pub use self::poly::*;
pub use self::spawner::*;
pub use self::transforms::*;

use std::f32;

pub trait Path {
    fn path(&self, completion: f32) -> Point;
}

pub trait Percent: Sized {
    fn percent(self, percent: f32) -> Self;
}

pub trait Distance<Dest> {
    fn distance(&self, dest: &Dest) -> f32;
    fn delta(&self, dest: &Dest) -> Point;
    fn midpoint(&self, dest: &Dest) -> Point;
    fn manhattan_distance(&self, dest: &Dest) -> f32;
    /// Returns a unit point with the signs of the distance to dest. So,
    /// Point {x: -1, y: 1} if the dest is to the upper left of self.
    fn sign_to(&self, dest: &Dest) -> Point;
}

impl<C1: Centered, C2: Centered> Distance<C2> for C1 {
    default fn distance(&self, dest: &C2) -> f32 { self.centroid().distance(&dest.centroid()) }

    default fn delta(&self, dest: &C2) -> Point { self.centroid().delta(&dest.centroid()) }

    default fn midpoint(&self, dest: &C2) -> Point { self.centroid().midpoint(&dest.centroid()) }

    default fn manhattan_distance(&self, dest: &C2) -> f32 {
        self.centroid().manhattan_distance(&dest.centroid())
    }
    default fn sign_to(&self, dest: &C2) -> Point {
        use num::Signed;
        (dest.centroid() - self.centroid()).signum()
    }
}

pub trait Scale {
    fn scale(self, scale: f32) -> Self;
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

pub trait Centered {
    fn centroid(&self) -> Point;
}

impl Centered for Vec<Point> {
    fn centroid(&self) -> Point {
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

pub trait Place: Sized {
    fn place(self, dest: Point) -> Self;
}

pub trait Translate: Sized {
    fn translate(self, delta: Point) -> Self;
}