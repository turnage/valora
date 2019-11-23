//! Tools for composing generative fine art.

mod canvas;
mod gpu;
mod raster;
mod render;

pub use self::gpu::{Gpu, Shader, UniformBuffer};
pub use canvas::*;
pub use glium::program::Program;
pub use palette::{self, Alpha, Blend, ComponentWise, Hue, IntoColor, LinSrgb, LinSrgba, Saturate};
pub use rand::{self, rngs::StdRng, Rng, SeedableRng};
pub use render::*;
pub use structopt::StructOpt;

use self::{gpu::*, raster::Method};
use failure::Error;
use lyon_path::math::Point;
use std::{path::PathBuf, time::Duration};

pub type V2 = Point;

pub type Result<T> = std::result::Result<T, Error>;

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

pub trait Painter: Sized {
    fn setup(gpu: &Gpu, world: &World, rng: &mut StdRng) -> Result<Self>;
    fn paint(&mut self, ctx: Context, canvas: &mut Canvas);
}

/// Run a painter.
pub fn run<P: Painter>(options: Options) -> Result<()> {
    let (output_width, output_height) = (
        (options.world.width as f32 * options.world.scale) as u32,
        (options.world.height as f32 * options.world.scale) as u32,
    );

    let (gpu, mut strategy) = if let Some(base_path) = options.output.clone() {
        let (gpu, _) = Gpu::new()?;
        let buffer = gpu.build_texture(output_width, output_height)?;

        (
            gpu,
            RenderStrategy::File {
                buffer,
                output_path: move |frame_number: usize, seed: u64| {
                    let mut base_path = base_path.clone();
                    base_path.push(format!("{}", seed));
                    std::fs::create_dir_all(&base_path)
                        .expect(&format!("To create save directory {:?}", base_path));
                    base_path.push(format!("{}.png", frame_number));
                    base_path
                },
            },
        )
    } else {
        let (gpu, events_loop) = Gpu::with_window(output_width, output_height)?;
        let wait = Duration::from_secs_f64(1. / options.world.framerate as f64);
        let gpu_clone = gpu.clone();

        (
            gpu,
            RenderStrategy::Screen {
                events_loop,
                wait,
                get_frame: move || {
                    gpu_clone
                        .get_frame()
                        .expect("To get frame from windowed gpu")
                },
            },
        )
    };

    let mut current_seed = options.world.seed;
    loop {
        let mut rng = StdRng::seed_from_u64(current_seed);
        let mut painter = P::setup(&gpu, &options.world, &mut rng)?;

        let mut renderer = Renderer {
            strategy: &mut strategy,
            gpu: &gpu,
            options: options.clone(),
            rng: &mut rng,
            output_width: output_width,
            output_height: output_height,
        };

        if let Some(rebuild) = renderer.render_frames(|ctx, canvas| painter.paint(ctx, canvas))? {
            match rebuild {
                Rebuild::NewSeed(new_seed) => {
                    current_seed = new_seed;
                }
            }
        } else {
            break;
        }
    }

    Ok(())
}
