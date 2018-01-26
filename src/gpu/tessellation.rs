use errors::Result;
use lyon::path_iterator::math::TypedPoint2D;
use lyon::tessellation::*;
use lyon::tessellation::geometry_builder::{simple_builder, VertexBuffers};
use poly::{Point, Poly};
use tess2::*;
use mesh::{DrawMode, Mesh};
use std::iter::FromIterator;

#[derive(Debug, Default)]
pub struct Tessellation {
    pub vertices: Vec<Point>,
    pub indices: Vec<u32>,
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
        Tessellation { vertices, indices }
    }
}

pub fn tessellate(src: &Mesh) -> Result<Tessellation> {
    match src.draw_mode {
        DrawMode::Fill => tessellate_fill(&src.src),
        DrawMode::Stroke { thickness } => tessellate_stroke(&src.src, thickness),
    }
}

pub fn tessellate_fill(poly: &Poly) -> Result<Tessellation> {
    let tess2_verts: Vec<Vertex> = poly.vertices()
        .into_iter()
        .map(|v| Vertex { x: v.x, y: v.y })
        .collect();
    let triangles = fill(&tess2_verts)?;

    Ok(Tessellation {
        vertices: triangles
            .vertices
            .into_iter()
            .map(|v| Point { x: v.x, y: v.y })
            .collect(),
        indices: triangles.indices,
    })
}

pub fn tessellate_stroke(poly: &Poly, thickness: f32) -> Result<Tessellation> {
    let mut vertex_buffers: VertexBuffers<StrokeVertex> = VertexBuffers::new();
    basic_shapes::stroke_polyline(
        poly.vertices()
            .into_iter()
            .map(|v| TypedPoint2D::new(v.x, v.y)),
        true,
        &StrokeOptions::default().with_line_width(thickness),
        &mut simple_builder(&mut vertex_buffers),
    );
    Ok(Tessellation {
        vertices: vertex_buffers
            .vertices
            .into_iter()
            .map(|v| Point {
                x: v.position.x,
                y: v.position.y,
            })
            .collect(),
        indices: vertex_buffers.indices.into_iter().map(Into::into).collect(),
    })
}
