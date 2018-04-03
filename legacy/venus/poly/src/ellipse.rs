//! Ellipse-estimating polygons.

use point::Point;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct EllipseResolution(pub usize);

impl Default for EllipseResolution {
    fn default() -> Self {
        EllipseResolution(1000)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Ellipse {
    pub center: Point,
    pub width: f32,
    pub height: Option<f32>,
    pub resolution: EllipseResolution,
    pub phase: f32,
}

impl Ellipse {
    pub fn circle(center: Point, radius: f32) -> Self {
        Ellipse {
            center,
            width: radius,
            height: None,
            resolution: EllipseResolution::default(),
            phase: 0.0,
        }
    }

    pub fn new(center: Point, width: f32, height: f32) -> Self {
        Self {
            center,
            width,
            height: Some(height),
            resolution: EllipseResolution::default(),
            phase: 0.0,
        }
    }

    pub fn with_resolution(self, resolution: usize) -> Self {
        Self {
            resolution: EllipseResolution(resolution),
            ..self
        }
    }

    pub fn with_phase(self, phase: f32) -> Self {
        Self { phase, ..self }
    }

    pub fn circumpoint(&self, angle: f32) -> Point {
        Point {
            x: self.center.x + angle.cos() * self.width,
            y: self.center.y + angle.sin() * self.height.unwrap_or(self.width),
        }
    }

    pub fn circumpoints(&self) -> Vec<Point> {
        use std::f32;

        let resolution = self.resolution.0;
        let interval = (f32::consts::PI * 2.0) / (resolution as f32);
        (0..resolution)
            .into_iter()
            .map(|i| (i as f32) * interval + self.phase)
            .map(|a| self.circumpoint(a))
            .collect()
    }
}
