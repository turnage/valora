use errors::Result;
use geom::Point;
use pipeline::Vertex;
use raster::{fix_coord, Tessellate, Tessellation};

const WHITE: [f32; 3] = [1.0, 1.0, 1.0];

pub struct Poly {
    pub vertices: Vec<Point>,
}

impl Tessellate for Poly {
    fn tessellate(self) -> Result<Tessellation> {
        use lyon::path_iterator::*;
        use lyon::tessellation::*;
        use lyon::tessellation::geometry_builder::{simple_builder, VertexBuffers};

        let mut vertex_buffers: VertexBuffers<FillVertex> = VertexBuffers::new();
        {
            let mut buffers_builder = simple_builder(&mut vertex_buffers);
            let polyline = self.vertices.into_iter().map(Into::into);
            basic_shapes::fill_polyline(
                polyline,
                &mut FillTessellator::new(),
                &FillOptions::default(),
                &mut buffers_builder,
            )?;
        }
        Ok(Tessellation {
            vertices: vertex_buffers
                .vertices
                .into_iter()
                .map(|v| {
                    Vertex {
                        pos: [v.position.x, v.position.y],
                        color: WHITE,
                    }
                })
                .collect(),
            indices: vertex_buffers.indices,
        })
    }
}
