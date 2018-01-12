use color::Colorer;
use errors::Result;
use gpu::GpuVertex;
use lyon::path_iterator::math::TypedPoint2D;
use lyon::tessellation::*;
use lyon::tessellation::geometry_builder::{simple_builder, VertexBuffers};
use poly::{Point, Poly};

#[derive(Debug, Default)]
pub struct Tessellation {
    pub vertices: Vec<GpuVertex>,
    pub indices:  Vec<u32>,
}

impl Tessellation {
    fn from_stroke_buffer(buffer: VertexBuffers<StrokeVertex>, colorer: Colorer) -> Self {
        let mut tessellation = Tessellation::default();
        for v in buffer.vertices {
            let point = Point {
                x: v.position.x,
                y: v.position.y,
            };
            let color = colorer.color(point);
            tessellation.vertices.push(GpuVertex::from((point, color)));
        }
        tessellation.indices = buffer.indices.into_iter().map(Into::into).collect();
        tessellation
    }
}

    pub fn tessellate_fill(poly: &Poly, colorer: Colorer) -> Result<Tessellation> {
        use tess2::safe::*;

        let tess2_verts: Vec<Vertex> = poly.vertices()
            .into_iter()
            .map(|v| Vertex { x: v.x, y: v.y })
            .collect();
        let triangles = fill(&tess2_verts)?;

        Ok(Tessellation {
            vertices: triangles
                .vertices
                .into_iter()
                .map(|v| {
                    let point = Point { x: v.x, y: v.y };
                    let color = colorer.color(point);
                    (point, color).into()
                })
                .collect(),
            indices:  triangles.indices,
        })
    }

    pub fn tessellate_stroke(poly: &Poly, thickness: f32, colorer: Colorer) -> Result<Tessellation> {
        let mut vertex_buffers: VertexBuffers<StrokeVertex> = VertexBuffers::new();
        basic_shapes::stroke_polyline(
            poly.vertices()
                .into_iter()
                .map(|v| TypedPoint2D::new(v.x, v.y)),
            true,
            &StrokeOptions::default().with_line_width(thickness),
            &mut simple_builder(&mut vertex_buffers),
        );
        Ok(Tessellation::from_stroke_buffer(vertex_buffers, colorer))
    }
