use errors::Result;
use geom::Point;
use lyon::math::Radians;
use palette::Blend;
use pipeline::GpuVertex;
use raster::{Tessellate, Tessellation};
use shaders::Shader;

#[derive(Debug, Clone)]
pub struct Ellipse {
    center: Point,
    width: f32,
    height: f32,
    rotation: Radians<f32>,
}

impl Ellipse {
    pub fn circle(center: Point, radius: f32, rotation: f32) -> Self {
        Ellipse {
            center,
            width: radius,
            height: radius,
            rotation: Radians::new(rotation.to_radians()),
        }
    }
}

impl Tessellate for Ellipse {
    fn tessellate(self, shader: &Shader) -> Result<Tessellation> {
        use lyon::tessellation::*;
        use lyon::tessellation::geometry_builder::{VertexBuffers, simple_builder};
        use lyon::path_iterator::math::Vec2;

        let mut vertex_buffers: VertexBuffers<FillVertex> = VertexBuffers::new();
        basic_shapes::fill_ellipse(self.center.into(),
                                   Vec2::new(self.width, self.height),
                                   self.rotation,
                                   0.001,
                                   &mut simple_builder(&mut vertex_buffers));
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