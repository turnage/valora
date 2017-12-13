pub mod poly;
pub mod ellipse;

use errors::Result;
use lyon::math::TypedPoint2D;
use lyon::tessellation::FillVertex;
use rand::{Rand, Rng};
use raster::{Tessellate, Tessellation};
use shaders::Shader;

#[derive(Debug, Clone)]
pub enum Geometry {
    Poly(poly::Poly),
    Ellipse(ellipse::Ellipse),
}

impl From<poly::Poly> for Geometry {
    fn from(poly: poly::Poly) -> Geometry { Geometry::Poly(poly) }
}

impl From<ellipse::Ellipse> for Geometry {
    fn from(ellipse: ellipse::Ellipse) -> Geometry { Geometry::Ellipse(ellipse) }
}

impl Tessellate for Geometry {
    fn tessellate(&self, shader: &Shader) -> Result<Tessellation> {
        match *self {
            Geometry::Poly(ref poly) => poly.tessellate(shader),
            Geometry::Ellipse(ref ellipse) => ellipse.tessellate(shader),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Point {
    pub x: f32,
    pub y: f32,
}

impl Point {
    const WORLD_OFFSET: f32 = 1.0;
    const WORLD_FACTOR: f32 = 2.0;

    pub fn center() -> Point { Point { x: 0.5, y: 0.5 } }

    pub fn abs(self) -> Point { Point { x: self.x.abs(), y: self.y.abs() } }

    pub fn scale(self, factor: f32) -> Point { Point { x: self.x * factor, y: self.y * factor } }

    pub fn distance(self, point: Point) -> f32 { self.raw_distance(point).sqrt() }

    pub fn raw_distance(self, point: Point) -> f32 {
        let delta = self.difference(point).abs();
        delta.x.powi(2) + delta.y.powi(2)
    }

    pub fn manhattan(self, point: Point) -> f32 {
        (self.x - point.x).abs() + (self.y - point.y).abs()
    }

    pub fn difference(self, point: Point) -> Point {
        Point { x: self.x - point.x, y: self.y - point.y }
    }

    // OpenGL places the origin in the center of the screen. We rescale
    // and offset vertices one world unit so the origin is in the bottom
    // left, and y and x point up and right respectively. If you think
    // it should be done differently, you are wrong.
    fn fix_coord(coord: f32) -> f32 { (coord * Point::WORLD_FACTOR) - Point::WORLD_OFFSET }

    fn restore_coord(coord: f32) -> f32 { (coord + Point::WORLD_OFFSET) / Point::WORLD_FACTOR }
}

impl Rand for Point {
    fn rand<R: Rng>(rng: &mut R) -> Point { Point { x: rng.next_f32(), y: rng.next_f32() } }
}

impl<U> Into<TypedPoint2D<f32, U>> for Point {
    fn into(self) -> TypedPoint2D<f32, U> {
        TypedPoint2D::new(Point::fix_coord(self.x), Point::fix_coord(self.y))
    }
}

impl<U> From<TypedPoint2D<f32, U>> for Point {
    fn from(point: TypedPoint2D<f32, U>) -> Point {
        Point { x: Point::restore_coord(point.x), y: Point::restore_coord(point.y) }
    }
}

impl From<FillVertex> for Point {
    fn from(point: FillVertex) -> Point { point.position.into() }
}
