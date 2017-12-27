use color::BlendMode;
use errors::Result;
use geom::Rect;
use glium::texture::Texture2d;
use gpu::{BlendShader, BlendShaderSpec, DefaultShader, Factory, Gpu, GpuMesh, Shader,
          TextureShader};
use mesh::Mesh;
use sketch::SketchContext;
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
        let mut composition = ctx.gpu.canvas()?;
        for mut layer in self.layers.iter_mut().rev() {
            let mut cmds = Vec::new();
            let shader = layer.shader(ctx)?;
            for mesh in layer.render(ctx)? {
                cmds.push((shader.clone(), mesh))
            }
            let fg = ctx.gpu.render_to_texture(cmds)?;
            composition = Self::blend(ctx.gpu.clone(), fg, composition, layer.blend_mode())?;
        }
        let mut screen = ctx.gpu.screen();
        TextureShader::produce(composition, ctx.gpu.clone())?
            .draw(&mut screen, Self::frame_mesh(ctx.gpu.clone())?)?;
        screen.finish()
    }

    fn blend(gpu: Rc<Gpu>, fg: Texture2d, bg: Texture2d, mode: BlendMode) -> Result<Texture2d> {
        let shader = Rc::new(BlendShader::produce(BlendShaderSpec { bg, fg, mode }, gpu.clone())?);
        gpu.render_to_texture(vec![(shader, Self::frame_mesh(gpu.clone())?)])
    }

    fn frame_mesh(gpu: Rc<Gpu>) -> Result<GpuMesh> {
        GpuMesh::produce(Mesh { src: Rect::frame(), colorer: Default::default() }, gpu)
    }
}

pub trait Layer {
    fn render(&mut self, ctx: &SketchContext) -> Result<Vec<GpuMesh>>;
    fn shader(&mut self, ctx: &SketchContext) -> Result<Rc<Shader>> {
        let shader: DefaultShader = DefaultShader::produce((), ctx.gpu.clone())?;
        Ok(Rc::new(shader))
    }
    fn blend_mode(&self) -> BlendMode { BlendMode::Normal }
}

impl<T: Tessellate + Clone> Layer for Mesh<T> {
    fn render(&mut self, ctx: &SketchContext) -> Result<Vec<GpuMesh>> {
        Ok(vec![GpuMesh::produce(self.clone(), ctx.gpu.clone())?])
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