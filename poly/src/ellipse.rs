//! Ellipse-estimating polygons.

use point::Point;
use properties::{Centered, Path};
use transforms::{Scale, Translate};

#[derive(Debug, Clone)]
pub struct Ellipse {
    pub center: Point,
    pub width: f32,
    pub height: Option<f32>,
}

impl Ellipse {
    pub fn circle(center: Point, radius: f32) -> Self {
        Ellipse {
            center,
            width: radius,
            height: None,
        }
    }

    pub fn new(center: Point, width: f32, height: f32) -> Self {
        Self {
            center,
            width,
            height: Some(height),
        }
    }

    pub fn circumpoint(&self, angle: f32) -> Point {
        Point {
            x: self.center.x + angle.cos() * self.width,
            y: self.center.y + angle.sin() * self.height.unwrap_or(self.width),
        }
    }

    /// Use this function to get a polygonal estimation of the ellipse at the given phase
    /// and resolution.
    pub fn circumpoints(&self, resolution: usize, phase: f32) -> Vec<Point> {
        use std::f32;

        let interval = (f32::consts::PI * 2.0) / (resolution as f32);
        (0..resolution)
            .into_iter()
            .map(|i| (i as f32) * interval + phase)
            .map(|a| self.circumpoint(a))
            .collect()
    }

    pub fn path(self, phase: f32) -> EllipsePath { EllipsePath { ellipse: self, phase } }
}

pub struct EllipsePath {
    ellipse: Ellipse,
    phase: f32,
}

impl Path for EllipsePath {
    fn path(&self, completion: f32) -> Point {
        use std::f32;

        let angle = completion * (f32::consts::PI * 2.0);
        self.ellipse.circumpoint(angle + self.phase)
    }
}

impl Scale for Ellipse {
    fn scale(self, scale: f32) -> Self {
        Self { width: self.width * scale, height: self.height.map(|h| h * scale), ..self }
    }
}

impl Centered for Ellipse {
    fn center(&self) -> Point { self.center }
}

impl Translate for Ellipse {
    fn translate(self, delta: Point) -> Self { Self { center: self.center + delta, ..self } }
}
