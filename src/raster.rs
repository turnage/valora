//! Path rasterization.

use crate::{gpu::GpuVertex, Result, P2};
use amicola::SampleDepth;
use geo_clipper::{Clipper, EndType, JoinType};
use geo_types::{Coordinate, Line, MultiPolygon, Polygon};
use glium::index::PrimitiveType;
use lyon_path::{iterator::Flattened, Builder, Event};
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
    Stroke(f64),
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

fn path_to_multipolygon(builder: Builder, sample_depth: SampleDepth) -> MultiPolygon<f64> {
    let samples_per_pixel: u64 = sample_depth.into();
    let path = builder.build();
    let path = Flattened::new(1.0 / samples_per_pixel as f32, path.into_iter());
    let exterior = path
        .filter_map(|event| match event {
            Event::Line { from, to } => Some(Coordinate {
                x: from.x as f64,
                y: from.y as f64,
            }),
            _ => None,
        })
        .collect();
    MultiPolygon::from(Polygon::new(exterior, vec![]))
}

pub fn raster_path(
    builder: Builder,
    method: Method,
    color: LinSrgba,
    sample_depth: amicola::SampleDepth,
) -> Result<RasterResult> {
    let multi_polygon = path_to_multipolygon(builder, sample_depth);
    let multi_polygon = match method {
        Method::Fill => multi_polygon,
        Method::Stroke(width) => {
            multi_polygon.offset(width as f64, JoinType::Miter(1.), EndType::OpenButt, 1.)
        }
    };

    let shade_commands = amicola::raster(multi_polygon.into_iter(), sample_depth);
    Ok(format_shade_commands(color, shade_commands))
}
