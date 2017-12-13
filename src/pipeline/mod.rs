use errors::Result;
use geom::Point;
use geom::poly::Poly;
use glium::{self, Surface};
use image;
use raster::{Tessellate, Tessellation};
use render::Renderable;
use shaders::Shader;
use std;

#[derive(Copy, Clone)]
pub struct GpuVertex {
    pub position: [f32; 2],
    pub color: [f32; 3],
}

implement_vertex!(GpuVertex, position, color);

pub struct Pipeline {
    events_loop: glium::glutin::EventsLoop,
    display: glium::Display,
    shader_program: glium::program::Program,
    texture_program: glium::program::Program,
}

impl Pipeline {
    pub fn new(size: u32) -> Result<Pipeline> {
        let events_loop = glium::glutin::EventsLoop::new();
        let window = glium::glutin::WindowBuilder::new()
            .with_title("Valora".to_string())
            .with_dimensions(size, size);
        let context = glium::glutin::ContextBuilder::new()
            .with_multisampling(16)
            .with_vsync(true);
        let display = glium::Display::new(window, context, &events_loop)?;
        let shader_program = program!(&display,
            150 => {
                vertex: include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
                                               "/shaders/default.glslv")),
                fragment: include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
                                                 "/shaders/default.glslf")),
            }
        )?;
        let texture_program = program!(&display,
            150 => {
                vertex: include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
                                               "/shaders/texture.glslv")),
                fragment: include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
                                                 "/shaders/texture.glslf")),
            }
        )?;
        Ok(Pipeline { events_loop, shader_program, display, texture_program })
    }

    pub fn draw(&mut self, renderables: Vec<Renderable>) -> Result<()> {
        let mut frame = self.display.draw();
        frame.clear_color(0.0, 0.0, 0.0, 1.0);
        for renderable in renderables {
            match renderable {
                Renderable::Tessellations(ts) => {
                    self.draw_tesselations(&mut frame, ts)?;
                }
                Renderable::Texture(texture) => {
                    let tex = glium::texture::Texture2d::new(&self.display, texture)?;
                    let uniforms = uniform! {
                        matrix: [
                            [1.0, 0.0, 0.0, 0.0],
                            [0.0, 1.0, 0.0, 0.0],
                            [0.0, 0.0, 1.0, 0.0],
                            [0.0 , 0.0, 0.0, 1.0f32],
                        ],
                        tex: tex.sampled()
                          .magnify_filter(glium::uniforms::MagnifySamplerFilter::Linear)
                          .minify_filter(glium::uniforms::MinifySamplerFilter::Linear),
                    };
                    let (vertex_buffer, index_buffer) =
                        self.make_buffers(Poly::square(Point { x: 0.0, y: 0.0 }, 1.0)
                                              .tessellate(&Shader::empty())?)?;
                    frame
                        .draw(&vertex_buffer,
                              &index_buffer,
                              &self.texture_program,
                              &uniforms,
                              &Default::default())?;
                }
            }
        }
        frame.finish()?;
        Ok(())
    }

    fn draw_tesselations(&self,
                         frame: &mut glium::Frame,
                         tessellations: Vec<Tessellation>)
                         -> Result<()> {
        let bundle = tessellations
            .into_iter()
            .fold(Tessellation::default(), |mut bundle, mut tess| {
                let vertices_len = bundle.vertices.len() as u32;
                bundle
                    .indices
                    .extend(tess.indices
                                .into_iter()
                                .map(|i| i as u32 + vertices_len));
                bundle.vertices.append(&mut tess.vertices);
                bundle
            });

        let (vertex_buffer, index_buffer) = self.make_buffers(bundle)?;

        frame
            .draw(&vertex_buffer,
                  &index_buffer,
                  &self.shader_program,
                  &glium::uniforms::EmptyUniforms,
                  &Default::default())?;
        Ok(())
    }

    fn make_buffers(&self,
                    tessellation: Tessellation)
                    -> Result<(glium::VertexBuffer<GpuVertex>, glium::IndexBuffer<u32>)> {
        Ok((glium::VertexBuffer::new(&self.display, tessellation.vertices.as_slice())?,
            glium::IndexBuffer::new(&self.display,
                                    glium::index::PrimitiveType::TrianglesList,
                                    tessellation.indices.as_slice())?))
    }

    pub fn save_frame(&self, root_frame_filename: &str, frame: usize) -> Result<()> {
        let image: glium::texture::RawImage2d<u8> = self.display.read_front_buffer();
        let image =
            image::ImageBuffer::from_raw(image.width, image.height, image.data.into_owned())
                .unwrap();
        let image = image::DynamicImage::ImageRgba8(image).flipv();
        let path = format!("{}{:08}.png", root_frame_filename, frame);
        let mut output = std::fs::File::create(path)?;
        image.save(&mut output, image::ImageFormat::PNG).unwrap();
        Ok(())
    }

    pub fn events(self) -> Result<Option<(Self, Vec<glium::glutin::WindowEvent>)>> {
        let mut terminate = false;
        let mut events = Vec::new();
        let Pipeline { mut events_loop, shader_program, display, texture_program } = self;
        events_loop.poll_events(|event| {
            use glium::glutin::WindowEvent::*;
            match event {
                glium::glutin::Event::WindowEvent { window_id: _, event } => {
                    match event {
                        KeyboardInput {
                            input: glium::glutin::KeyboardInput {
                                virtual_keycode: Some(glium::glutin::VirtualKeyCode::Escape), ..
                            },
                            ..
                        } |
                        Closed => terminate = true,
                        _ => {
                            events.push(event);
                        }
                    }
                }
                _ => (),
            }
        });
        match terminate {
            true => Ok(None),
            false => {
                Ok(Some((Pipeline { events_loop, shader_program, display, texture_program },
                         events)))
            }
        }
    }
}
