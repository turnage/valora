use gpu::{Factory, Gpu, GpuMesh};
use std::rc::Rc;
use composition::{Composition, Layer};
use gpu::shaders::{GpuShader, Shader};
use mesh::Mesh;
use errors::Result;

pub struct GpuLayer {
    src: Mesh,
    shader: GpuShader,
    cached_mesh: GpuMesh,
    gpu: Rc<Gpu>,
}

impl Factory<Layer> for GpuLayer {
    fn produce(spec: Layer, gpu: Rc<Gpu>) -> Result<GpuLayer> {
        let (shader, mesh) = match spec {
            Layer::Mesh(mesh) => (Shader::Default, mesh),
            Layer::ShadedMesh { shader, mesh } => (shader, mesh),
        };
        Ok(GpuLayer {
            shader: GpuShader::produce(shader, gpu.clone())?,
            cached_mesh: GpuMesh::produce(mesh.clone(), gpu.clone())?,
            src: mesh,
            gpu,
        })
    }
}

impl GpuLayer {
    pub fn step(mut self, frame: usize) -> Result<Self> {
        self.cached_mesh.scale = self.src.scale.tween(frame);
        Ok(self)
    }
    pub fn render<'a>(&'a self) -> (&'a GpuShader, &'a GpuMesh) {
        (&self.shader, &self.cached_mesh)
    }
}

pub struct Render {
    layers: Vec<GpuLayer>,
}

impl Factory<Composition> for Render {
    fn produce(spec: Composition, gpu: Rc<Gpu>) -> Result<Render> {
        Ok(Render {
            layers: spec.layers()
                .into_iter()
                .map(|l| GpuLayer::produce(l, gpu.clone()))
                .collect::<Result<Vec<GpuLayer>>>()?,
        })
    }
}

impl Render {
    pub fn step(self, frame: usize) -> Result<Self> {
        Ok(Self {
            layers: self.layers
                .into_iter()
                .map(|l| l.step(frame))
                .collect::<Result<Vec<GpuLayer>>>()?,
            ..self
        })
    }

    pub fn render<'a>(&'a self) -> Vec<(&'a GpuShader, &'a GpuMesh)> {
        self.layers.iter().map(|l| l.render()).collect()
    }
}
