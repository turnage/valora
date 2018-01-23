use composition::Composition;
use errors::Result;
use glium::glutin::WindowEvent;
use gpu::{Factory, Gpu, Shader};
use gpu::render::{Render, RenderSpec};
use rand::{random, SeedableRng, StdRng};
use std::{fs, thread, time};
use std::rc::Rc;
use glium::{Program, Surface};

pub struct SketchCfg {
    pub size: u32,
    pub root_frame_filename: Option<String>,
    pub frame_limit: usize,
    pub seed: Option<usize>,
    pub still: bool,
    /// quality sets the factor above the output resolution that textures and
    /// polygons will render with and be super sampled at for output. Should
    /// be a power of 2 but hey, you do you.
    pub quality: u32,
}

impl Default for SketchCfg {
    fn default() -> Self {
        Self {
            size: 400,
            root_frame_filename: None,
            frame_limit: 400,
            seed: None,
            still: false,
            quality: 1,
        }
    }
}

pub struct SketchContext {
    pub cfg: SketchCfg,
    pub gpu: Rc<Gpu>,
    pub current_seed: usize,
}

impl SketchContext {
    pub fn load_shader(
        &self,
        vert_src: Option<&'static str>,
        frag_src: &'static str,
    ) -> Result<Shader> {
        Program::from_source(
            self.gpu.as_ref(),
            vert_src.unwrap_or(shader!("default.vert")),
            frag_src,
            None,
        ).map(Into::into)
            .map_err(Into::into)
    }
}
pub fn sketch<F: Fn(&SketchContext, StdRng) -> Result<Composition>>(
    cfg: SketchCfg,
    sketch: F,
) -> Result<()> {
    let (gpu, events_loop) = Gpu::new(cfg.size)?;
    let current_seed = cfg.seed.unwrap_or(random());
    let mut context = SketchContext {
        cfg,
        gpu: Rc::new(gpu),
        current_seed,
    };
    let render_spec = RenderSpec {
        width: context.cfg.size * context.cfg.quality,
        height: context.cfg.size * context.cfg.quality,
        composition: Composition::new(),
    };

    let mut frame = 0;
    let mut render = Render::produce(
        RenderSpec {
            composition: sketch(&context, StdRng::from_seed(&[context.current_seed]))?,
            ..render_spec
        },
        context.gpu.clone(),
    )?;

    let mut cycle = Gpu::events(events_loop);
    while let Some((events_loop, events)) = cycle {
        if events
            .iter()
            .find(|event| match **event {
                WindowEvent::ReceivedCharacter('r') => true,
                _ => false,
            })
            .is_some() || (context.cfg.still && frame > 30)
        {
            context.current_seed = random();
            frame = 0;
            render = Render::produce(
                RenderSpec {
                    composition: sketch(&context, StdRng::from_seed(&[context.current_seed]))?,
                    ..render_spec
                },
                context.gpu.clone(),
            )?;
        }
        if !(context.cfg.still && frame > 0) {
            render = render.step(frame)?;
            context
                .gpu
                .draw(frame, render.render(&context.gpu.library, frame)?)?;
            if let Some(ref root_frame_filename) = context.cfg.root_frame_filename {
                if frame < context.cfg.frame_limit {
                    let save_path = match context.cfg.still {
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
                    context
                        .gpu
                        .save_frame(render.buffer().as_ref(), &save_path)?;
                }
            }
        }
        cycle = Gpu::events(events_loop);
        thread::sleep(time::Duration::from_millis(32));
        frame += 1;
    }
    Ok(())
}
