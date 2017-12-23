use errors::Result;
use glium::{self, Surface};
use glium::uniforms::Uniforms;
use image;
use raster::Tessellation;
use shaders::Shader;
use std::{self, rc::Rc};

#[derive(Copy, Clone)]
pub struct GpuVertex {
    pub position: [f32; 2],
    pub color: [f32; 3],
}

implement_vertex!(GpuVertex, position, color);
implement_uniform_block!(GpuVertex, position, color);

pub struct Pipeline {
    events_loop: glium::glutin::EventsLoop,
    display: glium::Display,
}

pub struct DrawCmd<'a, 'b> {
    pub display: &'a glium::Display,
    frame: &'b mut glium::Frame,
    vertex_buffer: glium::VertexBuffer<GpuVertex>,
    index_buffer: glium::IndexBuffer<u32>,
}

impl<'a, 'b> DrawCmd<'a, 'b> {
    pub fn draw<U: Uniforms>(&mut self, shader: &glium::program::Program, u: &U) -> Result<()> {
        self.frame
            .draw(&self.vertex_buffer, &self.index_buffer, shader, u, &Default::default())
            .map_err(Into::into)
    }
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
        Ok(Pipeline { events_loop, display })
    }

    pub fn draw(&mut self, elements: Vec<(Rc<Shader>, Tessellation)>) -> Result<()> {
        let mut frame = self.display.draw();
        frame.clear_color(0.0, 0.0, 0.0, 1.0);
        for (shader, tessellation) in elements.into_iter() {
            let (vertex_buffer, index_buffer) = self.make_buffers(tessellation)?;
            shader
                .shade(DrawCmd {
                           display: &self.display,
                           frame: &mut frame,
                           vertex_buffer,
                           index_buffer,
                       })?;
        }
        frame.finish()?;
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
        let Pipeline { mut events_loop, display } = self;
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
            false => Ok(Some((Pipeline { events_loop, display }, events))),
        }
    }
}
