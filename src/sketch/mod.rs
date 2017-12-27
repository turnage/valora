pub mod animation;

pub use self::animation::*;

use errors::Result;
use rand::{SeedableRng, StdRng, random};
use std::{fs, thread, time, rc::Rc};
use composition::Composition;
use gpu::{Factory,Gpu};
use glium::glutin::WindowEvent;

pub struct SketchCfg {
    pub size: u32,
    pub root_frame_filename: Option<String>,
    pub seed: Option<usize>,
}

pub struct SketchContext {
    pub cfg: SketchCfg,
    pub gpu: Rc<Gpu>,
    pub frame: usize,
    pub current_seed: usize,
}

impl SketchContext {
    pub fn produce<Spec, F: Factory<Spec>>(&self, spec: &Spec) -> Result<F> {
        F::produce(&spec, self.gpu.clone())
    }
}

pub trait Sketch {
    fn sketch(&self, ctx: &SketchContext, rng: StdRng) -> Result<Composition>;
}

pub fn sketch<S: Sketch>(cfg: SketchCfg, sketch: S) -> Result<()> {
    let (gpu, events_loop) = Gpu::new(cfg.size)?;
    let current_seed = cfg.seed.unwrap_or(random());
    let mut context = SketchContext { cfg, gpu: Rc::new(gpu), frame: 0, current_seed };
    let mut composition = sketch.sketch(&context, StdRng::from_seed(&[current_seed]))?;

    let mut cycle = Gpu::events(events_loop);
    while let Some((events_loop, events)) = cycle {
        if events
               .iter()
               .find(|event| match **event {
                         WindowEvent::ReceivedCharacter('r') => true,
                         _ => false,
                     })
               .is_some() {
            context.current_seed = random();
            context.frame = 0;
            composition = sketch.sketch(&context, StdRng::from_seed(&[context.current_seed]))?;
        }
        composition.render(&context)?;
        if let Some(ref root_frame_filename) = context.cfg.root_frame_filename {
            let saves_dir = format!("{}/{:14}/", root_frame_filename, context.current_seed);
            fs::create_dir_all(&saves_dir)?;
            context.gpu.save_frame(&format!("{}{}", saves_dir, context.frame))?;
        }
        cycle = Gpu::events(events_loop);
        thread::sleep(time::Duration::from_millis(16));
        context.frame += 1;
    }
    Ok(())
}
