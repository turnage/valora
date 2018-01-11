//! Polygon definitions and traits with default implementations.

#![feature(specialization)]

extern crate num;
extern crate rand;

pub mod poly;
pub mod properties;
pub mod ellipse;
pub mod point;
pub mod rect;
//pub mod line;
pub mod transforms;

pub use self::ellipse::*;
//pub use self::line::*;
pub use self::point::*;
pub use self::poly::*;
pub use self::transforms::*;
pub use self::properties::*;
pub use self::rect::*;
