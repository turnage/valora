use geom::Point;
use image;
use palette::Blend;
use shaders::Shader;

pub struct Texture(pub image::ImageBuffer<image::Rgb<u8>, Vec<u8>>);

impl Texture {
    pub fn from_shader(size: u32, shader: &Shader) -> Self {
        let buffer = image::ImageBuffer::from_fn(size, size, |x, y| {
            let point = Point { x: x as f32 / size as f32, y: y as f32 / size as f32 };
            let c = shader.shade(point).into_premultiplied();
            let f = |c| (c * 255.0) as u8;
            image::Rgb { data: [f(c.red), f(c.green), f(c.blue)] }
        });
        Texture(buffer)
    }
}