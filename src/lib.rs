//! Tools for composing generative fine art.

mod canvas;
mod gpu;
mod raster;

pub use self::gpu::{Shader, UniformBuffer};
pub use canvas::*;
pub use glium::program::Program;
pub use palette::{self, Alpha, Blend, ComponentWise, Hue, IntoColor, LinSrgb, LinSrgba, Saturate};
pub use rand::{self, rngs::StdRng, Rng, SeedableRng};
pub use structopt::StructOpt;

use self::{gpu::*, raster::Method};
use failure::Error;
use glium::glutin::EventsLoop;
use image::{ImageBuffer, Rgba};
use lyon_path::math::Point;
use palette::{
    encoding::{srgb::Srgb, TransferFn},
    Component,
};
use rayon::prelude::*;
use std::{path::PathBuf, time::Duration};

pub type V2 = Point;

type Result<T> = std::result::Result<T, Error>;

#[derive(StructOpt, Debug, Clone)]
#[structopt(name = "valora")]
pub struct Options {
    #[structopt(flatten)]
    pub world: World,

    /// Prefix of output path. Output is <prefix>/<seed>/<frame_number>.png
    #[structopt(short = "o", long = "output", parse(from_os_str))]
    pub output: Option<PathBuf>,
}

/// The world in which the painting takes place.
#[derive(StructOpt, Debug, Copy, Clone)]
#[structopt(name = "world")]
pub struct World {
    /// The RNG seed for this painting.
    #[structopt(short = "e", long = "seed", default_value = "0")]
    pub seed: u64,

    /// The width in coordinate space of the painting.
    ///
    /// Coordinate space may differ from output space. If width is 500 but scale is 10, the painting
    /// will have a coordinate space width of 500, but the final output will have a width of 5000 pixels.
    #[structopt(short = "w", long = "width", default_value = "512")]
    pub width: f32,
    /// The height in coordinate space of the painting.
    ///
    /// Coordinate space may differ from output space. If height is 500 but scale is 10, the painting
    /// will have a coordinate space height of 500, but the final output will have a height of 5000 pixels.
    #[structopt(short = "h", long = "height", default_value = "650")]
    pub height: f32,

    /// The scale of the output.
    ///
    /// The final output space is (width*scale)x(height*scale). This value is useful for painting
    /// and doing work at one quickly rendering resolution, and later exporting at a much higher
    /// resolution while preserving the composition exactly.
    ///
    /// This value may be needed when writing shaders or using other raster graphics, to adjust them
    /// for the real output size. Vector painting such as with paths should not need to consider this.
    #[structopt(short = "s", long = "scale", default_value = "1.0")]
    pub scale: f32,

    /// The total number of frames in this painting.
    #[structopt(short = "f", long = "frames", default_value = "1")]
    pub frames: usize,

    /// The number of frames (to try) to render per second.
    #[structopt(short = "r", long = "frames_per_second", default_value = "24")]
    pub framerate: usize,
}

/// The context of the current render frame.
#[derive(Debug, Copy, Clone)]
pub struct FrameContext {
    /// The current frame in the composition.
    pub frame: usize,
}

impl World {
    pub fn normalize(&self, p: V2) -> V2 { V2::new(p.x / self.width, p.y / self.height) }

    pub fn center(&self) -> V2 { V2::new(self.width / 2.0, self.height / 2.0) }
}

impl Paint for World {
    fn paint(&self, comp: &mut Canvas) {
        comp.line_to(V2::new(0.0, 0.0));
        comp.line_to(V2::new(self.width, 0.0));
        comp.line_to(V2::new(self.width, self.height));
        comp.line_to(V2::new(0.0, self.height));
        comp.line_to(V2::new(0.0, 0.0));
    }
}

fn save_path_for_frame(mut base_path: PathBuf, seed: u64, frame: usize) -> PathBuf {
    base_path.push(format!("{}", seed));
    std::fs::create_dir_all(&base_path)
        .expect(&format!("To create save directory {:?}", base_path));
    base_path.push(format!("{}.png", frame));
    base_path
}

/// A render gate renders frames.
pub struct Renderer<'a> {
    gpu: &'a Gpu,
    events_loop: EventsLoop,
    options: Options,
    output_width: u32,
    output_height: u32,
}

impl<'a> Renderer<'a> {
    /// Render all of the frames for the composition. This will not return until until all frames of
    /// the composition have been rendered.
    pub fn render_frames(&mut self, mut f: impl FnMut(&FrameContext, &mut Canvas)) -> Result<()> {
        let default_shader = self
            .gpu
            .default_shader(self.output_width as f32, self.output_height as f32);

        let wait = Duration::from_secs_f64(1. / self.options.world.framerate as f64);
        let buffer = self
            .gpu
            .build_texture(self.output_width, self.output_height)?;
        for frame in 0..(self.options.world.frames) {
            let mut comp = Canvas::new(default_shader.clone(), self.options.world.scale);
            f(&FrameContext { frame }, &mut comp);

            if let Some(save_dir) = self.options.output.as_ref() {
                self.gpu.render(
                    self.output_width,
                    self.output_height,
                    comp,
                    &mut buffer.as_surface(),
                )?;
                let raw: glium::texture::RawImage2d<u8> = self.gpu.read_to_ram(&buffer)?;
                let image: ImageBuffer<Rgba<u8>, Vec<u8>> = ImageBuffer::from_raw(
                    self.output_width,
                    self.output_height,
                    raw.data
                        .into_par_iter()
                        .map(|v| v.convert::<f32>())
                        .map(|v: f32| <Srgb as TransferFn>::from_linear(v))
                        .map(|v| v.convert::<u8>())
                        .collect(),
                )
                .unwrap();

                image.save(save_path_for_frame(
                    save_dir.clone(),
                    self.options.world.seed,
                    frame,
                ))?;
            } else {
                let mut frame = self
                    .gpu
                    .get_frame()
                    .expect("Expected frame for windowed gpu context");
                self.gpu
                    .render(self.output_width, self.output_height, comp, &mut frame)?;
                frame.finish().expect("Swapping buffers");

                let mut quit = false;
                self.events_loop.poll_events(|event| {
                    use glutin::{DeviceEvent, Event, KeyboardInput, VirtualKeyCode};
                    match event {
                        Event::DeviceEvent {
                            event:
                                DeviceEvent::Key(KeyboardInput {
                                    virtual_keycode: Some(VirtualKeyCode::Escape),
                                    ..
                                }),
                            ..
                        } => {
                            quit = true;
                        }
                        _ => {}
                    }
                });
                if quit {
                    return Ok(());
                }

                std::thread::sleep(wait);
            }
        }
        Ok(())
    }
}

/// Run a composition.
pub fn run(
    options: Options,
    mut f: impl FnMut(&Gpu, &World, &mut StdRng, Renderer) -> Result<()>,
) -> Result<()> {
    let (width, height) = (
        options.world.width as f32 * options.world.scale,
        options.world.height as f32 * options.world.scale,
    );
    let mut rng = StdRng::seed_from_u64(options.world.seed);

    let (gpu, events_loop) = if options.output.is_some() {
        Gpu::new()?
    } else {
        Gpu::with_window(width as u32, height as u32)?
    };

    let gate = Renderer {
        gpu: &gpu,
        events_loop,
        options: options.clone(),
        output_width: width as u32,
        output_height: height as u32,
    };

    f(&gpu, &options.world, &mut rng, gate)
}
