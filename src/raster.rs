//! Path rasterization.

use crate::gpu::Instance;
use amicola::ShadeCommand;
use lyon_path::{Builder, PathEvent};
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

/// Rasterizes a path in output pixels, with its origin at the bottom left,
/// into instances to draw.
pub(crate) fn raster_path(
    path: Builder,
    method: Method,
    color: LinSrgba,
    width: u32,
    height: u32,
    sample_depth: amicola::SampleDepth,
) -> Vec<Instance> {
    let mut canvas = amicola::Canvas::new(width, height);
    canvas.set_sample_depth(sample_depth);

    // Output rows count down from the top.
    let flip = |p: lyon_path::math::Point| (p.x as f64, height as f64 - p.y as f64);
    for event in path.build().iter() {
        match event {
            PathEvent::Begin { at } => canvas.move_to(flip(at)),
            PathEvent::Line { to, .. } => canvas.line_to(flip(to)),
            PathEvent::Quadratic { ctrl, to, .. } => canvas.quadratic_to(flip(ctrl), flip(to)),
            PathEvent::Cubic {
                ctrl1, ctrl2, to, ..
            } => canvas.cubic_to(flip(ctrl1), flip(ctrl2), flip(to)),
            PathEvent::End { close: true, .. } => canvas.close_path(),
            PathEvent::End { close: false, .. } => {}
        }
    }

    let raster = match method {
        Method::Fill => canvas.fill(),
        Method::Stroke(width) => {
            canvas.set_stroke_width(width);
            canvas.stroke()
        }
    };

    let color = [
        color.color.red,
        color.color.green,
        color.color.blue,
        color.alpha,
    ];
    raster
        .map(|command| match command {
            ShadeCommand::Boundary { x, y, coverage } => Instance {
                rect: [x as f32, y as f32, x as f32 + 1., y as f32 + 1.],
                color,
                coverage,
            },
            ShadeCommand::Span { x, y } => Instance {
                rect: [x.start as f32, y as f32, x.end as f32, y as f32 + 1.],
                color,
                coverage: 1.,
            },
        })
        .collect()
}
