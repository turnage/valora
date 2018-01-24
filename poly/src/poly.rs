//! Simple polygon trait for polygons consisting of a single contour.

use ellipse::Ellipse;
use ngon::Ngon;
use point::Point;
use rect::Rect;
use std::f32;
use std::f32::consts::PI;

#[derive(Clone, Debug, PartialEq)]
pub enum Poly {
    Ellipse(Ellipse),
    Rect(Rect),
    Ngon(Ngon),
    Irregular(Vec<Point>),
}

impl Poly {
    pub fn vertices(&self) -> Vec<Point> {
        match *self {
            Poly::Ellipse(ref ellipse) => ellipse.circumpoints(),
            Poly::Rect(ref rect) => rect.vertices(),
            Poly::Ngon(ref ngon) => Ellipse::circle(ngon.center, ngon.radius)
                .with_resolution(ngon.n)
                .with_phase(ngon.phase)
                .circumpoints(),
            Poly::Irregular(ref vertices) => vertices.clone(),
        }
    }

    pub fn center(&self) -> Point {
        match *self {
            Poly::Ellipse(ref ellipse) => ellipse.center,
            Poly::Rect(ref rect) => rect.center(),
            Poly::Ngon(ref ngon) => ngon.center,
            Poly::Irregular(ref vertices) => {
                let mut min = Point {
                    x: f32::MAX,
                    y: f32::MAX,
                };
                let mut max = Point {
                    x: f32::MIN,
                    y: f32::MIN,
                };
                for v in vertices {
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

                Point::midpoint(&min, &max)
            }
        }
    }

    pub fn place(self, dest: Point) -> Self {
        match self {
            Poly::Ellipse(ellipse) => Poly::Ellipse(Ellipse {
                center: dest,
                ..ellipse
            }),
            Poly::Rect(rect) => Poly::Rect(Rect {
                bottom_left: dest + (rect.bottom_left - rect.center()),
                ..rect
            }),
            Poly::Ngon(ngon) => Poly::Ngon(Ngon {
                center: dest,
                ..ngon
            }),
            Poly::Irregular(vertices) => {
                let delta = dest - Poly::Irregular(vertices.clone()).center();
                Poly::Irregular(vertices.into_iter().map(|v| v + delta).collect())
            }
        }
    }

    pub fn subdivide_edges(self) -> Self {
        if let Poly::Irregular(vertices) = self {
            Poly::Irregular(
                vertices
                    .iter()
                    .zip(vertices.iter().skip(1).chain(vertices.iter().take(1)))
                    .flat_map(|(p1, p2)| vec![*p1, p1.midpoint(p2)])
                    .collect(),
            )
        } else {
            Poly::Irregular(self.vertices()).subdivide_edges()
        }
    }

    pub fn translate(self, delta: Point) -> Self {
        match self {
            Poly::Ellipse(ellipse) => Poly::Ellipse(Ellipse {
                center: ellipse.center + delta,
                ..ellipse
            }),
            Poly::Rect(rect) => Poly::Rect(Rect {
                bottom_left: rect.bottom_left + delta,
                ..rect
            }),
            Poly::Ngon(ngon) => Poly::Ngon(Ngon {
                center: ngon.center + delta,
                ..ngon
            }),
            Poly::Irregular(vertices) => {
                Poly::Irregular(vertices.into_iter().map(|p| p + delta).collect())
            }
        }
    }

    pub fn scale(self, scale: f32) -> Self {
        match self {
            Poly::Ellipse(ellipse) => Poly::Ellipse(Ellipse {
                width: ellipse.width * scale,
                height: ellipse.height.map(|h| h * scale),
                ..ellipse
            }),
            Poly::Rect(rect) => Poly::Rect(rect.scale(scale)),
            Poly::Ngon(ngon) => Poly::Ngon(Ngon {
                radius: ngon.radius * scale,
                ..ngon
            }),
            Poly::Irregular(vertices) => {
                let center = Poly::Irregular(vertices.clone()).center();
                Poly::Irregular(
                    vertices
                        .into_iter()
                        .map(|v| center + (v - center) * scale)
                        .collect(),
                )
            }
        }
    }

    pub fn rotate(self, phase: f32) -> Self {
        match self {
            Poly::Ellipse(ellipse) => Poly::Ellipse(ellipse),
            Poly::Rect(rect) => Poly::Irregular(Poly::Rect(rect).vertices()).rotate(phase),
            Poly::Ngon(ngon) => Poly::Ngon(Ngon {
                phase: phase,
                ..ngon
            }),
            Poly::Irregular(vertices) => {
                let center = Poly::Irregular(vertices.clone()).center();
                let theta = |v: Point| {
                    let delta = v - center;
                    delta.y.atan2(delta.x)
                };
                let phase_offset = theta(vertices[0]);
                Poly::Irregular(
                    vertices.into_iter()
                            .map(|v| {
                                let theta = theta(v) - phase_offset;
                                let theta = theta + phase;
                                Ellipse::circle(center, v.distance(&center)).circumpoint(theta)
                            })
                            .collect()
                )
            }
        }
    }

    pub fn perimeter(&self) -> f32 {
        match *self {
            Poly::Ellipse(ref ellipse) => match ellipse.height {
                Some(height) => {
                    let h = (ellipse.width - height).powi(2) / (ellipse.width + height).powi(2);
                    PI * (ellipse.width + height) * (1.0 + 0.25 * h + 0.015625 * h)
                }
                None => 2.0 * ellipse.width * PI,
            },
            ref poly => {
                let vertices = poly.vertices();
                (0..(vertices.len() - 1))
                    .into_iter()
                    .map(|i| Point::distance(&vertices[i], &vertices[i + 1]))
                    .sum()
            }
        }
    }

    pub fn perimeter_point(&self, completion: f32) -> Point {
        match *self {
            Poly::Ellipse(ref ellipse) => ellipse.circumpoint(completion * 2.0 * PI),
            ref poly => {
                let vertices = poly.vertices();
                let perimeter = poly.perimeter();
                let target = (completion % 1.0) * perimeter;
                let mut traversed = 0.0;
                let mut point = vertices[0];
                for i in 0..(vertices.len() - 1) {
                    let distance = Point::distance(&vertices[i], &vertices[i + 1]);
                    let delta = vertices[i + 1] - vertices[i];
                    let next = traversed + distance;
                    if next >= target {
                        let x = (target - traversed) / distance;
                        point = vertices[i] + (delta * x);
                        break;
                    }
                    traversed += distance;
                }
                point
            }
        }
    }
}

impl From<Ellipse> for Poly {
    fn from(ellipse: Ellipse) -> Self {
        Poly::Ellipse(ellipse)
    }
}

impl From<Rect> for Poly {
    fn from(rect: Rect) -> Self {
        Poly::Rect(rect)
    }
}

impl From<Ngon> for Poly {
    fn from(ngon: Ngon) -> Self {
        Poly::Ngon(ngon)
    }
}

impl From<Vec<Point>> for Poly {
    fn from(vertices: Vec<Point>) -> Self {
        Poly::Irregular(vertices)
    }
}
