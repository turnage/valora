use geom::Point;
use shaders::Shader;
use errors::Result;
use pipeline::Vertex;
use raster::{Tessellate, Tessellation};
use palette::Blend;

pub enum Poly {
    Rect {
        bottom_left: Point,
        width: f32,
        height: f32,
    },
    Irregular(Vec<Point>),
}

impl Poly {
    pub fn square(bottom_left: Point, size: f32) -> Poly {
        Poly::Rect {
            bottom_left,
            width: size,
            height: size,
        }
    }

    fn vertices(&self) -> Vec<Point> {
        match *self {
            Poly::Rect {
                bottom_left,
                width,
                height,
            } => vec![
                bottom_left,
                Point {
                    x: bottom_left.x,
                    y: bottom_left.y + height,
                },
                Point {
                    x: bottom_left.x + width,
                    y: bottom_left.y + height,
                },
                Point {
                    x: bottom_left.x + width,
                    y: bottom_left.y,
                },
            ],
            Poly::Irregular(ref vertices) => vertices.clone(),
        }
    }
}

impl Tessellate for Poly {
    fn tessellate(self, shader: Shader) -> Result<Tessellation> {
        use lyon::tessellation::*;
        use lyon::tessellation::geometry_builder::{simple_builder, VertexBuffers};

        let mut vertex_buffers: VertexBuffers<FillVertex> = VertexBuffers::new();
        basic_shapes::fill_polyline(
            self.vertices().into_iter().map(Into::into),
            &mut FillTessellator::new(),
            &FillOptions::default(),
            &mut simple_builder(&mut vertex_buffers),
        )?;
        Ok(Tessellation {
            vertices: vertex_buffers
                .vertices
                .into_iter()
                .map(|v| (v, shader.shade(v.into()).into_premultiplied()))
                .map(|(v, c)| {
                    Vertex {
                        pos: [v.position.x, v.position.y],
                        color: [c.red, c.green, c.blue],
                    }
                })
                .collect(),
            indices: vertex_buffers.indices,
        })
    }
}
