//! Polygon definitions and traits with default implementations.

#![feature(specialization)]

extern crate num;
extern crate rand;

pub mod poly;
pub mod ellipse;
pub mod point;
pub mod rect;
pub mod ngon;
//pub mod line;

pub use self::ellipse::*;
pub use self::ngon::*;
//pub use self::line::*;
pub use self::point::*;
pub use self::poly::*;
pub use self::rect::*;
