use errors::Result;
use geom::{Centered, Distance, Place, Point, Translate};
use properties::clipping::Bounded;
use std::f32;
use tessellation::{Tessellate, Tessellation};

pub trait Poly: Sized {
    fn vertices<'a>(&'a self) -> &'a [Point];
}

impl<P: Poly> Centered for P {
    fn centroid(&self) -> Point {
        let mut min = Point { x: f32::MAX, y: f32::MAX };
        let mut max = Point { x: f32::MIN, y: f32::MIN };
        for v in self.vertices() {
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
        min.midpoint(max)
    }
}

impl<P: Poly + Translate> Place for P {
    fn place(self, dest: Point) -> Self {
        let delta = dest - self.centroid();
        self.translate(delta)
    }
}

#[derive(Debug, Clone)]
pub struct IrregularPoly {
    vertices: Vec<Point>,
}

impl Poly for IrregularPoly {
    fn vertices<'a>(&'a self) -> &'a [Point] { &self.vertices }
}

impl Translate for IrregularPoly {
    fn translate(self, delta: Point) -> Self {
        IrregularPoly { vertices: self.vertices().iter().map(|p| *p + delta).collect() }
    }
}

#[derive(Clone, Debug)]
pub struct Rect {
    pub bottom_left: Point,
    pub width: f32,
    pub height: f32,
    vertices: Vec<Point>,
}

impl Poly for Rect {
    fn vertices<'a>(&'a self) -> &'a [Point] { &self.vertices }
}

impl Translate for Rect {
    fn translate(self, delta: Point) -> Self {
        Rect::new(self.bottom_left + delta, self.height, self.width)
    }
}

impl Bounded for Rect {
    fn in_bounds(&self, point: Point) -> bool {
        point.x >= self.bottom_left.x && point.x < self.bottom_left.x + self.width &&
        point.y >= self.bottom_left.y && point.y < self.bottom_left.y + self.height
    }
    fn bounding_box(&self) -> Rect { self.clone() }
}

impl Rect {
    pub fn square(bottom_left: Point, size: f32) -> Self { Self::new(bottom_left, size, size) }

    pub fn frame() -> Self { Self::square(Point { x: 0.0, y: 0.0 }, 1.0) }

    pub fn new(bottom_left: Point, width: f32, height: f32) -> Self {
        Self {
            height,
            width,
            bottom_left,
            vertices: vec![bottom_left,
                           Point { x: bottom_left.x, y: bottom_left.y + height },
                           Point { x: bottom_left.x + width, y: bottom_left.y + height },
                           Point { x: bottom_left.x + width, y: bottom_left.y }],
        }
    }
}

impl<P: Poly> Tessellate for P {
    fn tessellate(&self) -> Result<Tessellation> {
        use lyon::tessellation::*;
        use lyon::tessellation::geometry_builder::{VertexBuffers, simple_builder};
        use lyon::math;

        let mut vertex_buffers: VertexBuffers<FillVertex> = VertexBuffers::new();
        basic_shapes::fill_polyline(self.vertices()
                                        .into_iter()
                                        .map(|v: &Point| -> math::Point { (*v).into() }),
                                    &mut FillTessellator::new(),
                                    &FillOptions::default(),
                                    &mut simple_builder(&mut vertex_buffers))?;
        Ok(Tessellation {
               vertices: vertex_buffers
                   .vertices
                   .into_iter()
                   .map(|p| p.position)
                   .collect(),
               indices: vertex_buffers
                   .indices
                   .into_iter()
                   .map(Into::into)
                   .collect(),
           })
    }
}
