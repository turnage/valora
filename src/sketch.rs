use element::Element;
use errors::Result;
use glium;
use pipeline::Pipeline;
use rand::{SeedableRng, StdRng, random};
use std::{fs, thread, time};

pub struct SketchCfg {
    pub size: u32,
    pub root_frame_filename: Option<String>,
    pub seed: Option<usize>,
}

pub struct SketchContext {
    pub cfg: SketchCfg,
    pub frame: usize,
    pub rng: StdRng,
    pub current_seed: usize,
}

pub trait Sketch: Seed {
    fn draw<'a>(&'a self, ctx: &SketchContext) -> Result<Box<Iterator<Item = Element<'a>> + 'a>>;
    fn step(self,
            _ctx: &SketchContext,
            _events: Vec<glium::glutin::WindowEvent>)
            -> Result<Option<Self>> {
        Ok(Some(self))
    }
}

pub trait Seed: Sized {
    fn seed(self, ctx: &SketchContext) -> Result<Self>;
}

impl<T: Default> Seed for T {
    fn seed(self, _ctx: &SketchContext) -> Result<Self> { Ok(Self::default()) }
}

pub fn sketch<S: Sketch>(cfg: SketchCfg, sketch: S) -> Result<()> {
    let pipeline = Pipeline::new(cfg.size)?;
    let current_seed = cfg.seed.unwrap_or(random());
    let rng = StdRng::from_seed(&[current_seed]);
    let mut context = SketchContext { cfg, frame: 0, rng, current_seed };
    let mut sketch_bin = Some(sketch);

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
            context.rng = StdRng::from_seed(&[context.current_seed]);
            context.frame = 0;
            sketch_bin = Some(sketch_bin.unwrap().seed(&context)?);
        }
        pipeline
            .draw(sketch_bin.as_ref().unwrap().draw(&context)?)?;
        sketch_bin = sketch_bin.unwrap().step(&context, events)?;
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
