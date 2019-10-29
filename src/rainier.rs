//! Tools for composing generative fine art.

use crate::amicola::*;

pub use crate::amicola::{Polygon, Shader, UniformBuffer, V2, V4};
pub use glium::program::Program;
pub use rand::{self, rngs::StdRng, Rng, SeedableRng};

use failure::Error;
use image::{ImageBuffer, Rgba};
use std::{convert::TryFrom, path::PathBuf};
use structopt::StructOpt;

type Result<T> = std::result::Result<T, Error>;

#[derive(StructOpt, Debug, Clone)]
#[structopt(name = "valora")]
pub struct Options {
    /// Seed for the rng.
    #[structopt(short = "e", long = "seed", default_value = "0")]
    pub seed: u64,

    /// Width of view pane.
    #[structopt(short = "w", long = "width", default_value = "1024")]
    pub width: u32,

    /// Height of view pane.
    #[structopt(short = "h", long = "height", default_value = "512")]
    pub height: u32,

    /// Scale of view pane.
    #[structopt(short = "s", long = "scale", default_value = "1.0")]
    pub scale: f32,

    /// Frame range to render from the generate scene.
    #[structopt(short = "f", long = "frames", default_value = "1")]
    pub frames: usize,

    /// Prefix of output path. Output is <prefix>/<seed>/<frame_number>.png
    #[structopt(short = "o", long = "output", parse(from_os_str))]
    pub output: PathBuf,
}

#[derive(Debug, Copy, Clone)]
pub struct World {
    pub seed: u64,
    pub width: f32,
    pub height: f32,
    pub scale: f32,
    pub frames: usize,
}

#[derive(Debug, Copy, Clone)]
pub struct Context {
    pub world: World,
    pub frame: usize,
}

impl From<&Options> for Context {
    fn from(options: &Options) -> Self {
        Self {
            world: World {
                seed: options.seed,
                width: options.width as f32,
                height: options.height as f32,
                scale: options.scale,
                frames: options.frames,
            },
            frame: 0,
        }
    }
}

impl Context {
    pub fn normalize(&self, p: V2) -> V2 {
        V2::new(p.x / self.world.width, p.y / self.world.height)
    }

    pub fn center(&self) -> V2 { V2::new(self.world.width / 2.0, self.world.height / 2.0) }

    pub fn full_frame(&self) -> Polygon {
        Polygon::try_from(vec![
            V2::new(0.0, 0.0),
            V2::new(self.world.width, 0.0),
            V2::new(self.world.width, self.world.height),
            V2::new(0.0, self.world.height),
        ])
        .unwrap()
    }
}

fn save_path_for_frame(mut base_path: PathBuf, seed: u64, frame: usize) -> PathBuf {
    base_path.push(format!("{}", seed));
    std::fs::create_dir_all(&base_path)
        .expect(&format!("To create save directory {:?}", base_path));
    base_path.push(format!("{}.png", frame));
    base_path
}

pub struct Gpu {}

pub struct RenderGate {
    amicola: Amicola,
    default_shader: Shader,
    width: u32,
    height: u32,
    save_dir: PathBuf,
    frames: usize,
    context: Context,
}

impl RenderGate {
    pub fn render(&mut self, mut f: impl FnMut(&Context, &mut Composition)) -> Result<()> {
        for frame in 0..(self.frames) {
            let mut comp = Composition::new(self.default_shader.clone());
            self.context.frame = frame;
            f(&self.context, &mut comp);

            let buffer =
                self.amicola
                    .precompose(self.width, self.height, comp.elements.into_iter())?;
            let raw: glium::texture::RawImage2d<u8> = buffer.read();
            let image: ImageBuffer<Rgba<u8>, Vec<u8>> =
                ImageBuffer::from_raw(self.width, self.height, raw.data.into_owned()).unwrap();
            image.save(save_path_for_frame(
                self.save_dir.clone(),
                self.context.world.seed,
                frame,
            ))?;
        }
        Ok(())
    }
}

pub struct Rainier {}

impl Rainier {
    pub fn run(
        options: Options,
        mut f: impl FnMut(&Gpu, &World, &mut StdRng, RenderGate) -> Result<()>,
    ) -> Result<()> {
        let context = Context::from(&options);
        let amicola = Amicola::new()?;

        let (width, height) = (
            options.width as f32 * options.scale,
            options.height as f32 * options.scale,
        );
        let mut rng = StdRng::seed_from_u64(options.seed);
        let mut Composition = Composition::new(amicola.default_shader(width, height));

        let gpu = Gpu {};
        let gate = RenderGate {
            width: width as u32,
            height: height as u32,
            default_shader: amicola.default_shader(width, height),
            amicola,
            save_dir: options.output,
            frames: options.frames,
            context: context,
        };

        f(&gpu, &context.world, &mut rng, gate)
    }
}

pub struct Composition {
    current_path: Vec<V2>,
    current_shader: Shader,
    current_color: V4,
    scale: f32,
    elements: Vec<Element>,
}

impl Composition {
    fn new(default_shader: Shader) -> Self {
        Self {
            current_path: vec![],
            current_shader: default_shader,
            current_color: V4::new(1.0, 1.0, 1.0, 1.0),
            scale: 1.0,
            elements: vec![],
        }
    }

    fn set_scale(&mut self, scale: f32) { self.scale = scale; }

    fn move_to(&mut self, dest: V2) { self.current_path = vec![dest * self.scale]; }

    fn line_to(&mut self, dest: V2) { self.current_path.push(dest * self.scale); }

    fn set_color(&mut self, color: V4) { self.current_color = color; }

    fn set_shader(&mut self, shader: Shader) { self.current_shader = shader; }

    fn fill(&mut self) {
        let mut path = vec![];
        std::mem::swap(&mut self.current_path, &mut path);
        self.elements.push(Element {
            path,
            color: self.current_color,
            shader: self.current_shader.clone(),
            raster_method: RasterMethod::Fill,
        });
    }
}
