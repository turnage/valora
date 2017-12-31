use color::{BlendMode, Colorer};
use errors::Result;
use geom::Rect;
use glium::texture::Texture2d;
use gpu::{DefaultShader, Factory, Gpu, GpuMesh, Shader, Tessellate, TextureShader};
use mesh::{DrawMode, Mesh};
use sketch::SketchContext;
use std::rc::Rc;
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

    pub fn solid_layer(self, colorer: Colorer) -> Self {
        self.add(Mesh {
                     src: Rect::frame(),
                     colorer,
                     blend_mode: BlendMode::Normal,
                     draw_mode: DrawMode::Fill,
                 })
    }

    pub fn add<L: 'static + Layer>(mut self, layer: L) -> Self {
        self.layers.push(Box::new(layer));
        self
    }

    pub fn render(&mut self, ctx: &SketchContext) -> Result<()> {
        let mut cmds = Vec::new();
        for mut layer in self.layers.iter_mut() {
            let shader = layer.shader(ctx)?;
            for mesh in layer.render(ctx)? {
                cmds.push((shader.clone(), mesh))
            }
        }
        ctx.gpu.screen().draw_all(cmds)
    }
}

pub trait Layer {
    fn render(&mut self, ctx: &SketchContext) -> Result<Vec<GpuMesh>>;
    fn shader(&mut self, ctx: &SketchContext) -> Result<Rc<Shader>> {
        let shader: DefaultShader = DefaultShader::produce((), ctx.gpu.clone())?;
        Ok(Rc::new(shader))
    }
}

impl<T: Tessellate + Clone> Layer for Mesh<T> {
    fn render(&mut self, ctx: &SketchContext) -> Result<Vec<GpuMesh>> {
        Ok(vec![GpuMesh::produce(self.clone(), ctx.gpu.clone())?])
    }
}

impl<L: 'static + Layer + Clone> Layer for Tween<L> {
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