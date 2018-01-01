use geom::Point;
use lyon::math::Radians;
use properties::{Centered, Path};
use transforms::{Place, Scale, Translate};

#[derive(Debug, Clone)]
pub struct Ellipse {
    pub center: Point,
    pub width: f32,
    pub height: Option<f32>,
    pub rotation: Radians<f32>,
    pub tolerance: Option<f32>,
}

impl Ellipse {
    pub fn circle(center: Point, radius: f32) -> Self {
        Ellipse {
            center,
            width: radius,
            height: None,
            rotation: Radians::new(0.0),
            tolerance: None,
        }
    }

    pub fn circumpoint(&self, angle: f32) -> Point {
        Point {
            x: self.center.x + angle.cos() * self.width,
            y: self.center.y + angle.sin() * self.height.unwrap_or(self.width),
        }
    }

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
    fn centroid(&self) -> Point { self.center }
}

impl Place for Ellipse {
    fn place(self, dest: Point) -> Self { Self { center: dest, ..self } }
}

impl Translate for Ellipse {
    fn translate(self, delta: Point) -> Self { Self { center: self.center + delta, ..self } }
}