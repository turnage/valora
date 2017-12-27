use errors::Result;
use gpu::{DefaultShader, Factory, GpuMesh, Shader};
use mesh::Mesh;
use sketch::SketchContext;
use std::ops::DerefMut;
use std::rc::Rc;
use tessellation::Tessellate;
use tween::Tween;

#[derive(Default)]
pub struct Composition {
    layers: Vec<Box<Layer>>,
}

pub trait Compose<L: Layer> {
    fn push(self, layer: L) -> Self;
}

impl Composition {
    pub fn new() -> Self { Self::default() }

    pub fn add<L: 'static + Layer>(mut self, layer: L) -> Self {
        self.layers.push(Box::new(layer));
        self
    }

    pub fn render(&mut self, ctx: &SketchContext) -> Result<()> {
        ctx.gpu
            .screen()
            .draw_all(self.layers
                          .iter_mut()
                          .flat_map(|l| match (l.deref_mut().shader(ctx),
                                               l.deref_mut().render(ctx)) {
                                        (Ok(shader), Ok(meshes)) => {
                                            meshes
                                                .into_iter()
                                                .map(|mesh| Ok((shader.clone(), mesh)))
                                                .collect()
                                        }
                                        (Err(e1), Err(e2)) => vec![Err(e1), Err(e2)],
                                        (Err(e), _) => vec![Err(e)],
                                        (_, Err(e)) => vec![Err(e)],
                                    })
                          .collect::<Result<Vec<(Rc<Shader>, GpuMesh)>>>()?)
    }
}

pub enum BlendMode {
    Normal,
}

pub trait Layer {
    fn render(&mut self, ctx: &SketchContext) -> Result<Vec<GpuMesh>>;
    fn shader(&mut self, ctx: &SketchContext) -> Result<Rc<Shader>> {
        let shader: DefaultShader = DefaultShader::produce(&(), ctx.gpu.clone())?;
        Ok(Rc::new(shader))
    }
    fn blend_mode(&self) -> BlendMode { BlendMode::Normal }
}

impl<T: Tessellate + Clone> Layer for Mesh<T> {
    fn render(&mut self, ctx: &SketchContext) -> Result<Vec<GpuMesh>> {
        Ok(vec![GpuMesh::produce(&self, ctx.gpu.clone())?])
    }
}

impl<L: Layer + Clone> Layer for Tween<L> {
    fn render(&mut self, ctx: &SketchContext) -> Result<Vec<GpuMesh>> {
        self.tween(ctx.frame).render(ctx)
    }
}

impl<L: Layer> Layer for Vec<L> {
    fn render(&mut self, ctx: &SketchContext) -> Result<Vec<GpuMesh>> {
        self.iter_mut()
            .flat_map(|l| match l.render(ctx) {
                          Ok(meshes) => meshes.into_iter().map(|m| Ok(m)).collect(),
                          Err(e) => vec![Err(e)],
                      })
            .collect()
    }
}