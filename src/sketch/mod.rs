pub mod animation;
pub mod skins;
pub mod shaded;

pub use self::animation::*;
pub use self::skins::*;
pub use self::shaded::*;

use errors::Result;
use glium;
use pipeline::Pipeline;
use rand::{SeedableRng, StdRng, random};
use raster::{Tessellate, Tessellation};
use shaders::Shader;
use std::{fs, thread, time, rc::Rc};
use itertools::Itertools;

pub struct SketchCfg {
    pub size: u32,
    pub root_frame_filename: Option<String>,
    pub seed: Option<usize>,
}

pub struct SketchContext {
    pub cfg: SketchCfg,
    pub frame: usize,
    pub current_seed: usize,
}

pub struct Canvas {
    queue: Vec<(Rc<Shader>, Tessellation)>,
}

impl Canvas {
    pub fn compose<I: Iterator<Item=Result<Canvas>>>(mut i: I) -> Result<Canvas> {
        i.fold_results(Canvas::new(), |bg, fg| fg.atop(bg))
    }

    pub fn new() -> Self { Self { queue: Vec::new() } }

    pub fn draw<T: Tessellate>(mut self, shader: Rc<Shader>, t: &T) -> Result<Self> {
        let tessellation = t.tessellate(shader.as_ref())?;
        self.queue.push((shader, tessellation));
        Ok(self)
    }

    pub fn atop(mut self, mut bg: Self) -> Self {
        bg.queue.append(&mut self.queue);
        bg
    }

    pub fn drain(self) -> Vec<(Rc<Shader>, Tessellation)> { self.queue }
}

pub trait Draw: Sized {
    fn draw(&self, ctx: &SketchContext) -> Result<Canvas>;
}

pub trait Step: Sized {
    fn step(self,
            ctx: &SketchContext,
            rng: &mut StdRng,
            events: Vec<glium::glutin::WindowEvent>)
            -> Result<Option<Self>>;
}

impl<T> Step for T {
    default fn step(self,
                    _ctx: &SketchContext,
                    _rng: &mut StdRng,
                    _events: Vec<glium::glutin::WindowEvent>)
                    -> Result<Option<Self>> {
        Ok(Some(self))
    }
}

pub trait Seed: Sized {
    fn seed(ctx: &SketchContext) -> Result<Self>;
}

pub fn sketch<S: Draw + Step + Seed>(cfg: SketchCfg) -> Result<()> {
    let pipeline = Pipeline::new(cfg.size)?;
    let current_seed = cfg.seed.unwrap_or(random());
    let mut rng = StdRng::from_seed(&[current_seed]);
    let mut context = SketchContext { cfg, frame: 0, current_seed };
    let mut sketch_bin = Some(S::seed(&context)?);

    let mut cycle = pipeline.events();
    while let Ok(Some((mut pipeline, events))) = cycle {
        if events
               .iter()
               .find(|event| match **event {
                         glium::glutin::WindowEvent::ReceivedCharacter('r') => true,
                         _ => false,
                     })
               .is_some() {
            context.current_seed = random();
            rng = StdRng::from_seed(&[context.current_seed]);
            context.frame = 0;
            sketch_bin = Some(S::seed(&context)?);
        }
        pipeline
            .draw(sketch_bin.as_ref().unwrap().draw(&context)?.drain())?;
        sketch_bin = sketch_bin.unwrap().step(&context, &mut rng, events)?;
        if let Some(ref root_frame_filename) = context.cfg.root_frame_filename {
            let saves_dir = format!("{}/{:14}/", root_frame_filename, context.current_seed);
            fs::create_dir_all(&saves_dir)?;
            pipeline.save_frame(&saves_dir, context.frame)?;
        }
        cycle = match sketch_bin {
            Some(_) => pipeline.events(),
            None => Ok(None),
        };
        thread::sleep(time::Duration::from_millis(16));
        context.frame += 1;
    }
    cycle.map(|_| ())
}
