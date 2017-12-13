use errors::Result;
use geom::Point;
use palette::Blend;
use pipeline::GpuVertex;
use properties::clipping::Bounded;
use raster::{Tessellate, Tessellation};
use shaders::Shader;

#[derive(Debug, Clone)]
pub enum Poly {
    Rect(Rect),
    Irregular(Vec<Point>),
}

#[derive(Clone, Copy, Debug)]
pub struct Rect {
    pub bottom_left: Point,
    pub width: f32,
    pub height: f32,
}

impl Bounded for Rect {
    fn in_bounds(&self, point: Point) -> bool {
        point.x >= self.bottom_left.x && point.x < self.bottom_left.x + self.width &&
        point.y >= self.bottom_left.y && point.y < self.bottom_left.y + self.height
    }
    fn bounding_box(&self) -> Rect { *self }
}

impl Poly {
    pub fn square(bottom_left: Point, size: f32) -> Poly {
        Poly::Rect(Rect { bottom_left, width: size, height: size })
    }

    pub fn vertices(&self) -> Vec<Point> {
        match *self {
            Poly::Rect(Rect { bottom_left, width, height }) => {
                vec![bottom_left,
                     Point { x: bottom_left.x, y: bottom_left.y + height },
                     Point { x: bottom_left.x + width, y: bottom_left.y + height },
                     Point { x: bottom_left.x + width, y: bottom_left.y }]
            }
            Poly::Irregular(ref vertices) => vertices.clone(),
        }
    }
}

impl Tessellate for Poly {
    fn tessellate(&self, shader: &Shader) -> Result<Tessellation> {
        use lyon::tessellation::*;
        use lyon::tessellation::geometry_builder::{VertexBuffers, simple_builder};

        let mut vertex_buffers: VertexBuffers<FillVertex> = VertexBuffers::new();
        basic_shapes::fill_polyline(self.vertices().into_iter().map(Into::into),
                                    &mut FillTessellator::new(),
                                    &FillOptions::default(),
                                    &mut simple_builder(&mut vertex_buffers))?;
        Ok(Tessellation {
               vertices: vertex_buffers
                   .vertices
                   .into_iter()
                   .map(|v| (v, shader.shade(v.into()).into_premultiplied()))
                   .map(|(v, c)| {
                            GpuVertex {
                                position: [v.position.x, v.position.y],
                                color: [c.red, c.green, c.blue],
                            }
                        })
                   .collect(),
               indices: vertex_buffers
                   .indices
                   .into_iter()
                   .map(Into::into)
                   .collect(),
           })
    }
}
