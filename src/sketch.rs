use element::Element;
use errors::Result;
use glium;
use pipeline::Pipeline;
use raster::{Tessellate, Tessellation};
use shaders::Shader;
use std::{thread, time};
use std::result::Result as StdResult;

pub struct SketchCfg {
    pub size: u32,
    pub root_frame_filename: Option<String>,
}

pub struct SketchContext {
    pub cfg: SketchCfg,
    pub frame: usize,
}

pub trait Sketch: Sized {
    fn draw(&self, ctx: &SketchContext) -> StdResult<Vec<(Shader, Element)>, String>;
    fn step(self,
            _ctx: &SketchContext,
            _events: Vec<glium::glutin::WindowEvent>)
            -> StdResult<Self, String> {
        Ok(self)
    }
}

pub fn sketch<S: Sketch>(cfg: SketchCfg, mut sketch: S) -> Result<()> {
    let pipeline = Pipeline::new(cfg.size, cfg.root_frame_filename.clone())?;
    let mut context = SketchContext { cfg, frame: 0 };

    let mut cycle = pipeline.events();
    while let Ok(Some((mut pipeline, events))) = cycle {
        pipeline
            .draw(sketch
                      .draw(&context)?
                      .into_iter()
                      .map(|(shader, element)| element.tessellate(shader))
                      .collect::<Result<Vec<Tessellation>>>()?,
                  context.frame)?;
        sketch = sketch.step(&context, events)?;
        cycle = pipeline.events();
        context.frame += 1;

        thread::sleep(time::Duration::from_millis(500));
    }
    cycle.map(|_| ())
}
