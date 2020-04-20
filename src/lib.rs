//! A brush for generative fine art.

mod gpu;
mod noise_traits;
mod raster;
mod render;

pub mod attributes;
pub mod canvas;
pub mod forms;
pub mod paint;
pub mod path;
pub mod shaders;
pub mod transforms;
pub mod uniforms;

/// Exhuastive set of imports for painting.
pub mod prelude {
    pub use self::{
        attributes::*, canvas::*, forms::*, paint::*, path::*, shaders::*, transforms::*,
        uniforms::*,
    };
    pub use super::*;
    pub use euclid;
    pub use noise::{self, *};
    pub use noise_traits::*;
    pub use rayon::{self, prelude::*};

    pub use palette::{
        self, encoding::Srgb, Alpha, Blend, ComponentWise, Hue, IntoColor, LinSrgb, LinSrgba,
        Saturate, *,
    };
    pub use rand::{self, rngs::StdRng, Rng, SeedableRng};
    pub use structopt::StructOpt;

    pub use std::f32::consts::PI;
}

pub use self::{
    gpu::{Gpu, Shader},
    render::Context,
    shaders::ShaderProgram,
};

use self::{gpu::*, prelude::*, raster::Method};
use euclid::{Point3D, Size2D, UnknownUnit, Vector2D, Vector3D};
use failure::Error;
use lyon_path::math::Point;
use render::*;
use std::{path::PathBuf, time::Duration};

/// A two dimensional point.
pub type P2 = Point;

/// A three dimensional point.
pub type P3 = Point3D<f32, UnknownUnit>;

/// A two dimensional vector.
pub type V2 = Vector2D<f32, UnknownUnit>;

/// A three dimensional vector.
pub type V3 = Vector3D<f32, UnknownUnit>;

/// A two dimensional size.
pub type S2 = Size2D<f32, UnknownUnit>;

/// An angle.
pub type Angle = euclid::Angle<f32>;

/// A compiled GLSL program.
pub type Program = glium::program::Program;

/// A value or an error.
pub type Result<T> = std::result::Result<T, Error>;

/// Command line options for a painting run.
///
/// Construct with `Options::from_args()` to run the CLI.
#[derive(StructOpt, Debug, Clone)]
#[structopt(name = "valora")]
pub struct Options {
    #[structopt(flatten)]
    pub world: World,

    /// In brainstorm mode:
    ///
    ///   * When rendering a limited number of frames to screen, the preview will not close.
    ///
    ///   * When rendering to file, every frame will be rendered with a different seed.
    #[structopt(short = "b", long = "brainstorm")]
    pub brainstorm: bool,

    /// The number of frames to delay saving to file. For example, if delay=100,
    /// 100 frames will be rendered silently and then the 101st and those after it
    /// will be saved to file.
    #[structopt(short = "d", long = "delay", default_value = "0")]
    pub delay: usize,

    /// Prefix of output path. Output is <prefix>/<seed>/<frame_number>.png
    #[structopt(short = "o", long = "output", parse(from_os_str))]
    pub output: Option<PathBuf>,

    /// Sample depth is the number of times of subdivide and sample pixels at
    /// the boundaries of paths. This controls the antialiasing quality of the
    /// rasterizer. It recommended to leave this at 1 for quick iteration and
    /// turn it up for final output. The highest value is "super64".
    #[structopt(subcommand)]
    pub sample_depth: Option<amicola::SampleDepth>,
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
    #[structopt(short = "f", long = "frames")]
    pub frames: Option<usize>,

    /// The number of frames (to try) to render per second.
    #[structopt(short = "r", long = "frames_per_second", default_value = "24")]
    pub framerate: usize,
}

impl World {
    /// Normalizes coordinates into the range [0, 1] by dividing them by the coordinate space dimensions.
    pub fn normalize(&self, p: P2) -> P2 {
        P2::new(p.x / self.width, p.y / self.height)
    }

    /// Returns the center of the coordinate space.
    pub fn center(&self) -> P2 {
        P2::new(self.width / 2.0, self.height / 2.0)
    }

    /// Returns a `Rect` representing the entire frame.
    pub fn rect(&self) -> Rect {
        Rect {
            bottom_left: P2::new(0., 0.),
            width: self.width,
            height: self.height,
        }
    }
}

/// Draws a rectangle path covering the entire canvas.
impl Paint for World {
    fn paint(&self, comp: &mut Canvas) {
        comp.line_to(P2::new(0.0, 0.0));
        comp.line_to(P2::new(self.width, 0.0));
        comp.line_to(P2::new(self.width, self.height));
        comp.line_to(P2::new(0.0, self.height));
        comp.line_to(P2::new(0.0, 0.0));
    }
}

/// A trait for types which paint canvases.
pub trait Artist: Sized {
    /// Constructs the artist.
    ///
    /// This would be a place to compile any GLSL or construct any expensive
    /// resources needed across the whole composition.
    fn setup(gpu: Gpu, world: World, rng: &mut StdRng) -> Result<Self>;

    /// Paints a single frame.
    fn paint(&mut self, ctx: Context, canvas: &mut Canvas);
}

/// Run an artist defined by raw functions.
///
/// Takes a function that produces the function that should paint each frame.
pub fn run_fn<F>(options: Options, f: impl Fn(Gpu, World, &mut StdRng) -> Result<F>) -> Result<()>
where
    F: FnMut(Context, &mut Canvas),
{
    let (output_width, output_height) = (
        (options.world.width as f32 * options.world.scale) as u32,
        (options.world.height as f32 * options.world.scale) as u32,
    );

    let number_width = options
        .world
        .frames
        .unwrap_or(0)
        .to_string()
        .chars()
        .count();

    let (gpu, mut strategy) = if let Some(base_path) = options.output.clone() {
        let (gpu, _) = Gpu::new()?;
        let buffer = gpu.build_texture(output_width, output_height)?;

        std::fs::create_dir_all(&base_path)
            .expect(&format!("To create save directory {}", base_path.display()));

        (
            gpu,
            RenderStrategy::File {
                buffer,
                output_path: move |frame_number: usize, seed: u64| {
                    let mut base_path = base_path.clone();
                    base_path.push(format!(
                        "{}_{number:>0width$}.png",
                        seed,
                        number = frame_number,
                        width = number_width
                    ));
                    base_path
                },
            },
        )
    } else {
        let (gpu, events_loop, (screen_width, screen_height)) =
            Gpu::with_window(output_width, output_height)?;
        let buffer = gpu.build_texture(screen_width, screen_height)?;

        let wait = Duration::from_secs_f64(1. / options.world.framerate as f64);
        let gpu_clone = gpu.clone();

        let texture_glsl = include_str!("shaders/texture.frag");
        let texture_program = gpu.compile_glsl(texture_glsl)?;

        (
            gpu,
            RenderStrategy::Screen {
                events_loop,
                wait,
                buffer,
                texture_program,
                get_frame: move || {
                    gpu_clone
                        .get_frame()
                        .expect("To get frame from windowed gpu")
                },
            },
        )
    };

    let mut current_seed = options.world.seed;
    let mut render_count = 0;
    loop {
        let mut rng = StdRng::seed_from_u64(current_seed);
        let mut paint_fn = f(gpu.clone(), options.world, &mut rng)?;

        let mut renderer = Renderer {
            strategy: &mut strategy,
            gpu: &gpu,
            options: Options {
                world: World {
                    seed: current_seed,
                    frames: match (options.brainstorm, options.output.as_ref()) {
                        (true, Some(_)) => Some(1),
                        _ => options.world.frames,
                    },
                    ..options.world
                },
                ..options.clone()
            },
            rng: &mut rng,
            output_width: output_width,
            output_height: output_height,
        };

        let report = renderer.render_frames(|ctx, canvas| paint_fn(ctx, canvas))?;

        if let Some(rebuild) = report.rebuild {
            match rebuild {
                Rebuild::NewSeed(new_seed) => {
                    current_seed = new_seed;
                }
            }
        } else if options.brainstorm
            && options.output.is_some()
            && render_count < options.world.frames.unwrap_or(usize::max_value())
        {
            current_seed += 1;
        } else if report.explicit_quit || !options.brainstorm || options.output.is_some() {
            break;
        }

        render_count += 1;
    }

    Ok(())
}

/// Run an artist.
pub fn run<A: Artist>(options: Options) -> Result<()> {
    run_fn(options, |gpu, world, rng| {
        let mut artist = A::setup(gpu, world, rng)?;
        Ok(move |ctx: Context, canvas: &mut Canvas| artist.paint(ctx, canvas))
    })
}
