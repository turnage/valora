use errors::Result;
use glium::{self, Surface};
use image;
use raster::Tessellation;

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
    root_frame_filename: Option<String>,
}

impl Pipeline {
    pub fn new(size: u32, root_frame_filename: Option<String>) -> Result<Pipeline> {
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
        Ok(Pipeline { events_loop, shader_program, display, root_frame_filename })
    }

    pub fn draw(&mut self, tessellations: Vec<Tessellation>, frame: usize) -> Result<()> {
        use std;

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

        let index_buffer = glium::IndexBuffer::new(&self.display,
                                                   glium::index::PrimitiveType::TrianglesList,
                                                   bundle.indices.as_slice())?;
        let vertex_buffer = glium::VertexBuffer::new(&self.display, bundle.vertices.as_slice())?;

        let mut target = self.display.draw();
        target.clear_color(0.0, 0.0, 0.0, 0.0);
        target
            .draw(&vertex_buffer,
                  &index_buffer,
                  &self.shader_program,
                  &glium::uniforms::EmptyUniforms,
                  &Default::default())?;
        target.finish()?;

        if let Some(ref root_frame_filename) = self.root_frame_filename {
            let image: glium::texture::RawImage2d<u8> = self.display.read_front_buffer();
            let image =
                image::ImageBuffer::from_raw(image.width, image.height, image.data.into_owned())
                    .unwrap();
            let image = image::DynamicImage::ImageRgba8(image).flipv();
            let path = format!("{}-{:08}.png", root_frame_filename, frame);
            let mut output = std::fs::File::create(path).unwrap();
            image.save(&mut output, image::ImageFormat::PNG).unwrap();
        }

        Ok(())
    }

    pub fn events(self) -> Result<Option<(Self, Vec<glium::glutin::WindowEvent>)>> {
        let mut terminate = false;
        let mut events = Vec::new();
        let Pipeline { root_frame_filename, mut events_loop, shader_program, display } = self;
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
                        /*Resized(..) => {
                            gfx_glutin::update_views(&window, &mut target, &mut depth);
                        }*/
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
                Ok(Some((Pipeline { root_frame_filename, events_loop, shader_program, display },
                         events)))
            }
        }
    }
}
