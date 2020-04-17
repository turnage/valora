//! Path rasterization.

use crate::{gpu::GpuVertex, Result, P2};
use glium::index::PrimitiveType;
use lyon_path::Builder;
use lyon_tessellation::{
    BuffersBuilder, FillAttributes, FillOptions, FillTessellator, StrokeAttributes, StrokeOptions,
    StrokeTessellator, VertexBuffers,
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

pub struct RasterResult {
    pub primitive: PrimitiveType,
    pub vertices: Vec<GpuVertex>,
    pub indices: Vec<u32>,
}

pub fn format_shade_commands(
    color: LinSrgba,
    shade_commands: impl Iterator<Item = amicola::ShadeCommand>,
) -> RasterResult {
    let mut vertices = vec![];
    for cmd in shade_commands {
        match cmd {
            amicola::ShadeCommand::Boundary { x, y, coverage } => {
                let mut color = color;
                color.alpha *= coverage;
                vertices.push(GpuVertex {
                    vpos: [x as f32, y as f32],
                    vcol: [
                        color.color.red,
                        color.color.green,
                        color.color.blue,
                        color.alpha,
                    ],
                });

                vertices.push(GpuVertex {
                    vpos: [x as f32 + 1., y as f32],
                    vcol: [
                        color.color.red,
                        color.color.green,
                        color.color.blue,
                        color.alpha,
                    ],
                });
            }
            amicola::ShadeCommand::Span { x, y } => {
                vertices.push(GpuVertex {
                    vpos: [x.start as f32, y as f32],
                    vcol: [
                        color.color.red,
                        color.color.green,
                        color.color.blue,
                        color.alpha,
                    ],
                });

                vertices.push(GpuVertex {
                    vpos: [x.end as f32, y as f32],
                    vcol: [
                        color.color.red,
                        color.color.green,
                        color.color.blue,
                        color.alpha,
                    ],
                });
            }
        }
    }

    RasterResult {
        primitive: PrimitiveType::LinesList,
        indices: (0..(vertices.len() as u32)).collect(),
        vertices,
    }
}

pub fn raster_path(builder: Builder, method: Method, color: LinSrgba) -> Result<RasterResult> {
    match method {
        Method::Fill => Ok(format_shade_commands(
            color,
            amicola::fill_path(builder, amicola::SampleDepth::Super8),
        )),
        Method::Stroke(width) => {
            let ctor = |v: P2, _: StrokeAttributes| -> P2 { v };
            let mut buffers: VertexBuffers<P2, u32> = VertexBuffers::new();
            let mut buffers_builder = BuffersBuilder::new(&mut buffers, ctor);

            let mut tessellator = StrokeTessellator::new();
            tessellator
                .tessellate_path(
                    &builder.build(),
                    &StrokeOptions::default()
                        .with_line_width(width)
                        .with_tolerance(0.05),
                    &mut buffers_builder,
                )
                .expect("TODO: wrap error");

            Ok(RasterResult {
                primitive: PrimitiveType::TrianglesList,
                vertices: buffers
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
                indices: buffers.indices,
            })
        }
    }
}
