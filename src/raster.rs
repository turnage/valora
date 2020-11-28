//! Path rasterization.

use crate::{gpu::GpuVertex, Result, P2};
use amicola::SampleDepth;
use arrayvec::ArrayVec;
use geo_booleanop::boolean::BooleanOp;
use geo_types::{Coordinate, Line, LineString, MultiPolygon, Polygon};
use glium::index::PrimitiveType;
use itertools::Itertools;
use lyon_path::{iterator::Flattened, math::Point, Builder, Event};
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
    mut color: LinSrgba,
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
                let mut color = color;
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

fn tessellate_stroke(path: Builder, width: f32) -> VertexBuffers<P2, u32> {
    let ctor = |v: P2, _: StrokeAttributes| -> P2 { v };
    let mut buffers: VertexBuffers<P2, u32> = VertexBuffers::new();
    let mut buffers_builder = BuffersBuilder::new(&mut buffers, ctor);

    let mut tessellator = StrokeTessellator::new();
    tessellator
        .tessellate_path(
            &path.build(),
            &StrokeOptions::default()
                .with_line_join(LineJoin::MiterClip)
                .with_line_width(width)
                .with_tolerance(0.05),
            &mut buffers_builder,
        )
        .expect("TODO: wrap error");

    buffers
}

fn point_to_coord(p: Point) -> Coordinate<f64> {
    Coordinate {
        x: p.x as f64,
        y: p.y as f64,
    }
}

fn stroke_triangles(buffers: VertexBuffers<P2, u32>) -> impl Iterator<Item = Polygon<f64>> {
    let vertices = buffers.vertices;
    buffers
        .indices
        .into_iter()
        .tuples::<(_, _, _)>()
        .map(|(a, b, c)| ArrayVec::from([a, b, c]))
        .map(move |c| {
            c.into_iter()
                .map(|i| {
                    let v = vertices[i as usize];
                    Coordinate {
                        y: v.y as f64,
                        x: v.x as f64,
                    }
                })
                .collect()
        })
        .map(LineString)
        .map(|c| Polygon::new(c, vec![]))
}

fn event_to_coordinate(event: Event<Point, Point>) -> Option<Coordinate<f64>> {
    match event {
        Event::Line { from, to } => Some(point_to_coord(from)),
        Event::End { last, .. } => Some(point_to_coord(last)),
        _ => None,
    }
}

fn path_to_line_string(builder: Builder, sample_depth: SampleDepth) -> LineString<f64> {
    let samples_per_pixel: u64 = sample_depth.into();
    let path = builder.build();
    let path = Flattened::new(1.0 / samples_per_pixel as f32, path.into_iter());
    let path = path.filter_map(event_to_coordinate);

    path.collect()
}

pub fn raster_path(
    builder: Builder,
    method: Method,
    color: LinSrgba,
    sample_depth: amicola::SampleDepth,
) -> Result<RasterResult> {
    Ok(match method {
        Method::Fill => {
            let lines = path_to_line_string(builder, sample_depth);
            let shade_commands =
                amicola::raster(std::iter::once(Polygon::new(lines, vec![])), sample_depth);
            format_shade_commands(color, shade_commands)
        }
        Method::Stroke(width) => {
            let stroke = tessellate_stroke(builder, width as f32);
            let triangles = stroke_triangles(stroke);
            let shape = triangles.fold(MultiPolygon(vec![]), |acc, t| acc.union(&t));
            let shade_commands = amicola::raster(shape.into_iter(), sample_depth);
            format_shade_commands(color, shade_commands)
        }
    })
}
