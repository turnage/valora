use gpu::{self, Factory, Gpu, GpuBareVertex, GpuMesh};
use std::rc::Rc;
use composition::{Composition, Layer};
use gpu::shaders::{GpuShader, GpuUniforms, Shader, UniformFacade};
use mesh::{Mesh, MeshTransforms};
use glium::draw_parameters::DrawParameters;
use glium::{Surface, VertexBuffer};
use glium::texture::{MipmapsOption, Texture2d, UncompressedFloatFormat};
use errors::Result;
use rayon::prelude::*;
use palette::{Blend, Colora};
use color::BlendMode;
use glium::uniforms::{AsUniformValue, EmptyUniforms, UniformBuffer, UniformValue, Uniforms};

pub const MAX_MESHES: usize = 1024;

#[derive(Clone, Copy, Debug, PartialEq, Default)]
#[repr(C)]
pub struct GpuMeshTransforms {
    scale: f32,
    rotation: f32,
    center: (f32, f32),
    root_center: (f32, f32),
    _pad1: [u32; 2],
    color: (f32, f32, f32, f32),
}

impl GpuMeshTransforms {
    pub fn batch(src: Vec<Self>) -> [Self; MAX_MESHES] {
        let mut dest = [GpuMeshTransforms::default(); MAX_MESHES];
        for (i, tx) in src.into_iter().enumerate() {
            dest[i] = tx;
        }
        dest
    }
}

implement_vertex!(
    GpuMeshTransforms,
    scale,
    rotation,
    center,
    root_center,
    color
);
implement_uniform_block!(
    GpuMeshTransforms,
    scale,
    rotation,
    center,
    root_center,
    color
);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct GpuBatchVertex {
    pub vertex_position: (f32, f32),
    pub mesh_index: u32,
}

implement_vertex!(GpuBatchVertex, vertex_position, mesh_index);

impl GpuMeshTransforms {
    pub fn update(self, frame: usize, tx: &MeshTransforms) -> Self {
        Self {
            scale: tx.scale.tween(frame),
            rotation: tx.rotation.tween(frame),
            center: (tx.origin_x.tween(frame), tx.origin_y.tween(frame)),
            color: format_color(tx.color),
            ..self
        }
    }
}

fn format_color(color: Colora) -> (f32, f32, f32, f32) {
    let cp = color.into_premultiplied();
    (cp.red, cp.green, cp.blue, color.alpha)
}

impl<'a> From<&'a Mesh> for GpuMeshTransforms {
    fn from(src: &Mesh) -> GpuMeshTransforms {
        let center = src.src.center();
        GpuMeshTransforms {
            scale: 1.0,
            rotation: 0.0,
            center: (center.x, center.y),
            root_center: (center.x, center.y),
            _pad1: [0; 2],
            color: format_color(src.transforms.color),
        }
    }
}

pub struct DrawCmd<'a> {
    blend: gpu::Blend,
    shader: &'a GpuShader,
    src: DrawCmdSource<'a>,
}

pub enum DrawCmdSource<'a> {
    Instances {
        mesh: &'a GpuMesh<GpuBareVertex>,
        instance_data: &'a VertexBuffer<GpuMeshTransforms>,
    },
    Batch {
        mesh: &'a GpuMesh<GpuBatchVertex>,
        batch_data: &'a BatchCache,
    },
}

impl<'a> DrawCmd<'a> {
    pub fn exec<S: Surface>(&self, surface: &mut S) -> Result<()> {
        let parameters = DrawParameters {
            smooth: None,
            blend: self.blend,
            ..Default::default()
        };
        match self.src {
            DrawCmdSource::Instances {
                mesh,
                instance_data,
            } => {
                surface.draw(
                    (
                        mesh.vertices.as_ref(),
                        instance_data.per_instance().unwrap(),
                    ),
                    mesh.indices.as_ref(),
                    &self.shader.program,
                    &EmptyUniforms,
                    &parameters,
                )?;
            }
            DrawCmdSource::Batch { mesh, batch_data } => {
                surface.draw(
                    mesh.vertices.as_ref(),
                    mesh.indices.as_ref(),
                    &self.shader.program,
                    &uniform! {
                        Transforms: &batch_data.data
                    },
                    &parameters,
                )?;
            }
        };
        Ok(())
    }
}

struct InstanceCache {
    sources: Vec<MeshTransforms>,
    data: VertexBuffer<GpuMeshTransforms>,
}

struct BatchCache {
    sources: Vec<MeshTransforms>,
    data: UniformBuffer<[GpuMeshTransforms; MAX_MESHES]>,
}

struct GpuLayer {
    shader: GpuShader,
    blend: gpu::Blend,
    src: GpuLayerSource,
}

enum GpuLayerSource {
    Instances {
        src: GpuMesh<GpuBareVertex>,
        cache: InstanceCache,
    },
    Batch {
        mesh: GpuMesh<GpuBatchVertex>,
        cache: BatchCache,
    },
}

impl Factory<Layer> for GpuLayer {
    fn produce(spec: Layer, gpu: Rc<Gpu>) -> Result<GpuLayer> {
        match spec {
            Layer::Mesh { shader, mesh } => Ok(GpuLayer {
                shader: GpuShader::produce(shader, gpu.clone())?,
                blend: gpu::Blend::from(BlendMode::Normal),
                src: GpuLayerSource::Batch {
                    cache: BatchCache {
                        sources: vec![mesh.transforms.clone()],
                        data: UniformBuffer::dynamic(
                            gpu.as_ref(),
                            GpuMeshTransforms::batch(vec![GpuMeshTransforms::from(&mesh)]),
                        )?,
                    },
                    mesh: GpuMesh::produce(vec![mesh].as_ref(), gpu.clone())?,
                },
            }),
            Layer::MeshInstances { src, meshes } => Ok(GpuLayer {
                shader: GpuShader::produce(Shader::Instance, gpu.clone())?,
                blend: gpu::Blend::from(BlendMode::Normal),
                src: GpuLayerSource::Instances {
                    cache: InstanceCache {
                        data: VertexBuffer::dynamic(
                            gpu.as_ref(),
                            &meshes
                                .iter()
                                .map(|_| GpuMeshTransforms::from(&src))
                                .collect::<Vec<GpuMeshTransforms>>(),
                        )?,
                        sources: meshes,
                    },
                    src: GpuMesh::produce(src, gpu.clone())?,
                },
            }),
            Layer::MeshGroup { shader, meshes } => Ok(GpuLayer {
                shader: GpuShader::produce(shader, gpu.clone())?,
                blend: gpu::Blend::from(BlendMode::Normal),
                src: GpuLayerSource::Batch {
                    mesh: GpuMesh::produce(meshes.as_ref(), gpu.clone())?,
                    cache: BatchCache {
                        data: UniformBuffer::dynamic(
                            gpu.as_ref(),
                            GpuMeshTransforms::batch(
                                meshes.iter().map(|m| GpuMeshTransforms::from(m)).collect(),
                            ),
                        )?,
                        sources: meshes.into_iter().map(|m| m.transforms).collect(),
                    },
                },
            }),
        }
    }
}

impl GpuLayer {
    pub fn step(self, frame: usize) -> Self {
        match self.src {
            GpuLayerSource::Instances { src, mut cache } => {
                let sources = cache.sources;
                for (i, mut cached) in cache.data.map().iter_mut().enumerate() {
                    *cached = cached.update(frame, &sources[i]);
                }
                cache.sources = sources;
                Self {
                    src: GpuLayerSource::Instances { src, cache },
                    ..self
                }
            }
            GpuLayerSource::Batch { mesh, mut cache } => Self {
                src: GpuLayerSource::Batch {
                    mesh,
                    cache: {
                        let sources = cache.sources;
                        for (i, mut cached) in
                            cache.data.map().iter_mut().enumerate().take(sources.len())
                        {
                            *cached = cached.update(frame, &sources[i]);
                        }
                        BatchCache { sources, ..cache }
                    },
                },
                ..self
            },
        }
    }

    pub fn render<'a>(&'a self) -> DrawCmd<'a> {
        DrawCmd {
            shader: &self.shader,
            blend: self.blend,
            src: match self.src {
                GpuLayerSource::Instances { ref src, ref cache } => DrawCmdSource::Instances {
                    mesh: src,
                    instance_data: &cache.data,
                },
                GpuLayerSource::Batch {
                    ref mesh,
                    ref cache,
                } => DrawCmdSource::Batch {
                    mesh,
                    batch_data: cache,
                },
            },
        }
    }
}

struct BufferSpec {
    pub width: u32,
    pub height: u32,
}

pub struct RenderSpec {
    pub width: u32,
    pub height: u32,
    pub composition: Composition,
}

pub struct Render {
    layers: Vec<GpuLayer>,
}

impl Factory<RenderSpec> for Render {
    fn produce(spec: RenderSpec, gpu: Rc<Gpu>) -> Result<Render> {
        Ok(Render {
            layers: spec.composition
                .layers()
                .into_iter()
                .map(|l| -> Result<GpuLayer> { Factory::produce(l, gpu.clone()) })
                .collect::<Result<Vec<GpuLayer>>>()?,
        })
    }
}

impl Render {
    pub fn step(self, frame: usize) -> Self {
        Self {
            layers: self.layers.into_iter().map(|l| l.step(frame)).collect(),
            ..self
        }
    }

    pub fn cmds<'a>(&'a self) -> Vec<DrawCmd<'a>> {
        self.layers.iter().map(|l| l.render()).collect()
    }
}
