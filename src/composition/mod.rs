mod assembly;

use errors::Result;
use glium::glutin::WindowEvent;
use gpu::{Factory, Gpu};
use gpu::render::{Render, RenderSpec};
use rand::{random, SeedableRng, StdRng};
use std::{fs, thread, time};
use std::rc::Rc;

pub use self::assembly::*;

#[derive(StructOpt, Debug)]
#[structopt(name = "compose_config", about = "compose configuration.")]
pub struct CompositionCfg {
    #[structopt(short = "r", long = "resolution", help = "compose resolution.",
                default_value = "400")]
    pub resolution: u32,

    #[structopt(short = "o", long = "output",
                help = "Filename for compose output; directories for animations.")]
    pub output: Option<String>,

    #[structopt(short = "l", long = "limit", help = "Max frames to write to disk for animation.",
                default_value = "0")]
    pub frame_limit: usize,

    #[structopt(short = "s", long = "seed", help = "Seed for the compose rng.")]
    pub seed: Option<usize>,

    #[structopt(short = "t", long = "still",
                help = "This compose is still; do not animate; brainstorm seeds.")]
    pub still: bool,
    /// quality sets the factor above the output resolution that textures and
    /// polygons will render with and be super sampled at for output. Should
    /// be a power of 2 but hey, you do you.

    #[structopt(short = "q", long = "quality",
                help = "Factor above resolution to sample down at; should be power of 2.",
                default_value = "1")]
    pub quality: u32,
}

pub struct CompositionCtx {
    pub cfg: CompositionCfg,
    pub gpu: Rc<Gpu>,
    pub current_seed: usize,
}

pub fn compose<F: Fn(&CompositionCtx, StdRng) -> Result<Composition>>(
    cfg: CompositionCfg,
    compose: F,
) -> Result<()> {
    let (gpu, events_loop) = Gpu::new(cfg.resolution)?;
    let current_seed = cfg.seed.unwrap_or(random());
    println!("Using seed: {}", current_seed);
    let mut context = CompositionCtx {
        cfg,
        gpu: Rc::new(gpu),
        current_seed,
    };
    let render_spec = RenderSpec {
        width: context.cfg.resolution * context.cfg.quality,
        height: context.cfg.resolution * context.cfg.quality,
        composition: Composition::new(),
    };

    let mut frame = 0;
    let mut render = Render::produce(
        RenderSpec {
            composition: compose(&context, StdRng::from_seed(&[context.current_seed]))?,
            ..render_spec
        },
        context.gpu.clone(),
    )?;

    let mut cycle = Gpu::events(events_loop);
    while let Some((events_loop, events)) = cycle {
        if let Some(_) = events.iter().find(|event| match **event {
            WindowEvent::ReceivedCharacter('r') => true,
            _ => false,
        }) {
            context.current_seed = random();
            println!("Using seed: {}", context.current_seed);
            frame = 0;
            render = Render::produce(
                RenderSpec {
                    composition: compose(&context, StdRng::from_seed(&[context.current_seed]))?,
                    ..render_spec
                },
                context.gpu.clone(),
            )?;
        }
        if !(context.cfg.still && frame > 0) {
            render = render.step(frame);
            let mut screen = context.gpu.display.draw();
            for cmd in render.cmds() {
                cmd.exec(&mut screen)?;
            }
            screen.finish()?;
            if let Some(ref root_frame_filename) = context.cfg.output {
                if frame < context.cfg.frame_limit {
                    let save_path = match context.cfg.still || context.cfg.frame_limit == 1 {
                        true => {
                            fs::create_dir_all(&root_frame_filename)?;
                            format!("{}/{}", root_frame_filename, context.current_seed)
                        }
                        false => {
                            let dir = format!("{}/{}/", root_frame_filename, context.current_seed);
                            fs::create_dir_all(&dir)?;
                            format!("{}{:08}", dir, frame)
                        }
                    };
                    context.gpu.save_frame(&save_path)?;
                }
            }
        }
        cycle = Gpu::events(events_loop);
        thread::sleep(time::Duration::from_millis(32));
        frame += 1;
    }
    Ok(())
}
