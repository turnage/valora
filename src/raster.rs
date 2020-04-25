//! Path rasterization.

use crate::{gpu::GpuVertex, Result, P2};
use lyon_path::Builder;
use lyon_tessellation::{
    BuffersBuilder, FillAttributes, FillOptions, FillTessellator, LineJoin, StrokeAttributes,
    StrokeOptions, StrokeTessellator, VertexBuffers,
};
use palette::LinSrgba;

/// The method by which the rasterizer will rasterize the vector path.
#[derive(Debug, Clone, Copy)]
pub enum Method {
    /// In fill method, the rasterizer will treat all the area inside the path as part of the
    /// raster area. In this method, paths are automatically closed by assuming an edge from the
    /// last to the first vertex.
    Fill,
    /// In stroke method, the rasterizer will treat the area immediately adjacent the path within
    /// the given width as part of the rastered area. In this method, paths are left open
    /// and no edge between the last and first vertex is assumed.
    Stroke(f32),
}

pub fn raster_path(
    builder: Builder,
    method: Method,
    color: LinSrgba,
) -> Result<(Vec<GpuVertex>, Vec<u32>)> {
    match method {
        Method::Fill => {
            let ctor = |v: P2, _: FillAttributes| -> P2 { v };
            let mut buffers: VertexBuffers<P2, u32> = VertexBuffers::new();
            let mut buffers_builder = BuffersBuilder::new(&mut buffers, ctor);

            let mut tessellator = FillTessellator::new();
            let result = tessellator.tessellate_path(
                &builder.build(),
                &FillOptions::default().with_tolerance(0.05),
                &mut buffers_builder,
            );
            match result {
                Ok(_) => {}
                Err(e) => panic!("Tessellation failed: {:?}", e),
            }

            Ok((
                buffers
                    .vertices
                    .into_iter()
                    .map(|v| GpuVertex {
                        vpos: [v.x, v.y],
                        vcol: [
                            color.color.red,
                            color.color.green,
                            color.color.blue,
                            color.alpha,
                        ],
                    })
                    .collect(),
                buffers.indices,
            ))
        }
        Method::Stroke(width) => {
            let ctor = |v: P2, _: StrokeAttributes| -> P2 { v };
            let mut buffers: VertexBuffers<P2, u32> = VertexBuffers::new();
            let mut buffers_builder = BuffersBuilder::new(&mut buffers, ctor);

            let mut tessellator = StrokeTessellator::new();
            tessellator
                .tessellate_path(
                    &builder.build(),
                    &StrokeOptions::default()
                        .with_line_join(LineJoin::MiterClip)
                        .with_line_width(width)
                        .with_tolerance(0.05),
                    &mut buffers_builder,
                )
                .expect("TODO: wrap error");

            Ok((
                buffers
                    .vertices
                    .into_iter()
                    .map(|v| GpuVertex {
                        vpos: [v.x, v.y],
                        vcol: [
                            color.color.red,
                            color.color.green,
                            color.color.blue,
                            color.alpha,
                        ],
                    })
                    .collect(),
                buffers.indices,
            ))
        }
    }
}
