use errors::Result;
use geom::{Point, Rect};
use image;
use palette::Blend;
use shaders::Shader;

pub fn from_shader(size: u32, shader: &Shader) -> Result<(Shader, Rect)> {
    let buffer: image::ImageBuffer<image::Rgb<u8>, Vec<u8>> =
        image::ImageBuffer::from_fn(size, size, |x, y| {
            let point = Point { x: x as f32 / size as f32, y: y as f32 / size as f32 };
            let c = shader.color_vertex(point).into_premultiplied();
            let f = |c| (c * 255.0) as u8;
            image::Rgb { data: [f(c.red), f(c.green), f(c.blue)] }
        });
    Ok((Shader::texture(buffer), Rect::square(Point { x: 0.0, y: 0.0 }, 1.0)))
}