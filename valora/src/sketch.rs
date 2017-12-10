use raster::{Tessellate, Tessellation};
use pipeline::Pipeline;
use errors::Result;
use std::result::Result as StdResult;
use glutin;
use geom::poly::Poly;
use std::{thread, time};

pub enum Element {
    Poly(Poly),
}

impl Tessellate for Element {
    fn tessellate(self) -> Result<Tessellation> {
        match self {
            Element::Poly(poly) => poly.tessellate(),
        }
    }
}

pub struct SketchCfg {
    pub size: u32,
}

pub struct SketchContext {
    pub cfg: SketchCfg,
}

pub trait Sketch: Sized {
    fn draw(&self, ctx: &SketchContext) -> StdResult<Vec<Element>, String>;
    fn step(
        self,
        _ctx: &SketchContext,
        _events: Vec<glutin::WindowEvent>,
    ) -> StdResult<Self, String> {
        Ok(self)
    }
}

pub fn sketch<S: Sketch>(cfg: SketchCfg, mut sketch: S) -> Result<()> {
    let pipeline = Pipeline::new(cfg.size)?;
    let context = SketchContext { cfg };

    let mut cycle = pipeline.events();
    while let Ok(Some((mut pipeline, events))) = cycle {
        pipeline.draw(sketch
            .draw(&context)?
            .into_iter()
            .map(|t| t.tessellate())
            .collect::<Result<Vec<Tessellation>>>()?)?;
        sketch = sketch.step(&context, events)?;
        cycle = pipeline.events();
        thread::sleep(time::Duration::from_secs(1));
    }
    cycle.map(|_| ())
}
