use errors::Result;
use gfx;
use gfx::Device;
use gfx::pso::PipelineState;
use gfx::traits::FactoryExt;
use gfx_device_gl;
use gfx_window_glutin as gfx_glutin;
use glutin;
use raster::Tessellation;

pub type ColorFormat = gfx::format::Srgba8;
pub type DepthFormat = gfx::format::DepthStencil;

const BLACK: [f32; 4] = [0.0, 0.0, 0.0, 1.0];

gfx_defines! {
    vertex Vertex {
        pos: [f32; 2] = "a_Pos",
        color: [f32; 3] = "a_Color",
    }

    pipeline pipe {
        vbuf: gfx::VertexBuffer<Vertex> = (),
        out: gfx::RenderTarget<ColorFormat> = "Target0",
    }
}

pub struct Pipeline {
    depth: gfx::handle::DepthStencilView<
        gfx_device_gl::Resources,
        (gfx::format::D24_S8, gfx::format::Unorm),
    >,
    events_loop: glutin::EventsLoop,
    window: glutin::Window,
    factory: gfx_device_gl::Factory,
    target: gfx::handle::RenderTargetView<
        gfx_device_gl::Resources,
        (gfx::format::R8_G8_B8_A8, gfx::format::Srgb),
    >,
    encoder: gfx::Encoder<gfx_device_gl::Resources, gfx_device_gl::CommandBuffer>,
    pso: PipelineState<gfx_device_gl::Resources, pipe::Meta>,
    device: gfx_device_gl::Device,
}

impl Pipeline {
    pub fn new(size: u32) -> Result<Pipeline> {
        let events_loop = glutin::EventsLoop::new();
        let builder = glutin::WindowBuilder::new()
            .with_title("Valora".to_string())
            .with_dimensions(size, size)
            .with_vsync();
        let (window, device, mut factory, target, depth) =
            gfx_glutin::init::<ColorFormat, DepthFormat>(builder, &events_loop);

        let encoder = factory.create_command_buffer().into();
        let pso = factory.create_pipeline_simple(
            include_bytes!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/shaders/default.glslv"
            )),
            include_bytes!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/shaders/default.glslf"
            )),
            pipe::new(),
        )?;
        Ok(Pipeline {
            depth,
            events_loop,
            window,
            target,
            factory,
            encoder,
            pso,
            device,
        })
    }

    pub fn draw(&mut self, tessellations: Vec<Tessellation>) -> Result<()> {
        self.encoder.clear(&self.target, BLACK);
        for tessellation in tessellations {
            let (vertex_buffer, slice) = self.factory.create_vertex_buffer_with_slice(
                tessellation.vertices.as_slice(),
                tessellation.indices.as_slice(),
            );
            let data = pipe::Data {
                vbuf: vertex_buffer,
                out: self.target.clone(),
            };
            self.encoder.draw(&slice, &self.pso, &data);
        }
        self.encoder.flush(&mut self.device);
        self.window.swap_buffers()?;
        self.device.cleanup();
        Ok(())
    }

    pub fn events(self) -> Result<Option<(Self, Vec<glutin::WindowEvent>)>> {
        let mut terminate = false;
        let mut events = Vec::new();
        let Pipeline {
            mut depth,
            events_loop,
            window,
            mut target,
            factory,
            encoder,
            pso,
            device,
        } = self;
        events_loop.poll_events(
            |glutin::Event::WindowEvent {
                 window_id: _,
                 event,
             }| {
                use glutin::WindowEvent::*;
                match event {
                    KeyboardInput(_, _, Some(glutin::VirtualKeyCode::Escape), _) | Closed => {
                        terminate = true
                    }
                    Resized(_, _) => {
                        gfx_glutin::update_views(&window, &mut target, &mut depth);
                    }
                    _ => {
                        events.push(event);
                    }
                }
            },
        );
        match terminate {
            true => Ok(None),
            false => Ok(Some((
                Pipeline {
                    depth,
                    events_loop,
                    window,
                    target,
                    factory,
                    encoder,
                    pso,
                    device,
                },
                events,
            ))),
        }
    }
}
