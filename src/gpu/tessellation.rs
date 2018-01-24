use errors::Result;
use gpu::GpuVertex;
use lyon::path_iterator::math::TypedPoint2D;
use lyon::tessellation::*;
use lyon::tessellation::geometry_builder::{simple_builder, VertexBuffers};
use poly::{Point, Poly};
use tess2::*;
use palette::Colora;
use mesh::{Mesh, DrawMode};
use std::iter::FromIterator;

#[derive(Debug, Default)]
pub struct Tessellation {
    pub vertices: Vec<GpuVertex>,
    pub indices: Vec<u32>,
}

impl Tessellation {
    fn from_stroke_buffer(buffer: VertexBuffers<StrokeVertex>, color: Colora) -> Self {
        let mut tessellation = Tessellation::default();
        for v in buffer.vertices {
            let point = Point {
                x: v.position.x,
                y: v.position.y,
            };
            let color = color;
            tessellation.vertices.push(GpuVertex::from((point, color)));
        }
        tessellation.indices = buffer.indices.into_iter().map(Into::into).collect();
        tessellation
    }
}

impl FromIterator<Tessellation> for Tessellation {
    fn from_iter<T: IntoIterator<Item = Tessellation>>(iter: T) -> Self {
        let mut vertices = Vec::new();
        let mut indices = Vec::new();
        for mut tess in iter {
            let index_offset = vertices.len() as u32;
            indices.extend(tess.indices.into_iter().map(|i| i + index_offset));
            vertices.append(&mut tess.vertices);
        }
        Tessellation {
            vertices,
            indices
        }
    }
}

pub fn tessellate(src: &Mesh) -> Result<Tessellation> {
    match src.draw_mode {
        DrawMode::Fill => tessellate_fill(&src.src, src.color),
        DrawMode::Stroke { thickness } => {
            tessellate_stroke(&src.src, thickness, src.color)
        }
    }
}

pub fn tessellate_fill(poly: &Poly, color: Colora) -> Result<Tessellation> {
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
                (point, color).into()
            })
            .collect(),
        indices: triangles.indices,
    })
}

pub fn tessellate_stroke(poly: &Poly, thickness: f32, color: Colora) -> Result<Tessellation> {
    let mut vertex_buffers: VertexBuffers<StrokeVertex> = VertexBuffers::new();
    basic_shapes::stroke_polyline(
        poly.vertices()
            .into_iter()
            .map(|v| TypedPoint2D::new(v.x, v.y)),
        true,
        &StrokeOptions::default().with_line_width(thickness),
        &mut simple_builder(&mut vertex_buffers),
    );
    Ok(Tessellation::from_stroke_buffer(vertex_buffers, color))
}
