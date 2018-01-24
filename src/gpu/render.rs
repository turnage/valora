use gpu::{Factory, Gpu, GpuMesh, GpuVertex};
use std::rc::Rc;
use composition::{Composition, Layer};
use gpu::shaders::{GpuShader, GpuUniforms, UniformFacade};
use mesh::Mesh;
use glium::draw_parameters::{DrawParameters};
use glium::{Surface, VertexBuffer};
use glium::texture::{MipmapsOption, Texture2d, UncompressedFloatFormat};
use glium::uniforms::MagnifySamplerFilter;
use errors::Result;
use poly::{Point, Rect};
use itertools::Itertools;

#[derive(Clone, Copy, Debug)]
pub struct MeshTransforms {
    scale: f32,
    rotation: f32,
    center: (f32, f32),
    root_center: (f32, f32)
}

implement_vertex!(MeshTransforms, scale, rotation, center, root_center);

impl From<Point> for MeshTransforms {
    fn from(root_center: Point) -> MeshTransforms {
        let fixed_root_center = GpuVertex::fix_point(root_center);
        MeshTransforms {
            root_center: (fixed_root_center.x, fixed_root_center.y),
            scale: 1.0,
            rotation: 0.0,
            center: (0.0, 0.0),
        }
    }
}

pub struct DrawCmd<'a, 'b, 'c> {
    pub shader: &'a GpuShader,
    pub mesh: &'b GpuMesh,
    pub instance_data: &'c VertexBuffer<MeshTransforms>,
}

pub struct InstanceCache {
    sources: Vec<Mesh>,
    cache: VertexBuffer<MeshTransforms>
}

impl InstanceCache {
    pub fn step(mut self, frame: usize) -> Self {
        let sources = self.sources;
        for (i, mut cached) in self.cache.map().iter_mut().enumerate() {
            cached.scale = sources[i].scale.tween(frame);
            cached.rotation = sources[i].rotation.tween(frame);
            cached.center = (
                GpuVertex::fix_coord(sources[i].origin_x.tween(frame)),
                GpuVertex::fix_coord(sources[i].origin_y.tween(frame))
            );
        };
        Self {
            cache: self.cache,
            sources,
        }
    }
}

pub struct GpuLayer {
    src: Mesh,
    shader: GpuShader,
    cached_mesh: GpuMesh,
    instances: InstanceCache,
}

impl Factory<Layer> for GpuLayer {
    fn produce(spec: Layer, gpu: Rc<Gpu>) -> Result<GpuLayer> {
        let (shader, mesh, instances) = match spec {
            Layer::Mesh { shader, mesh } => (shader, mesh.clone(), vec![mesh]),
            Layer::MeshGroup { shader, src, meshes } => (shader, src, meshes)
        };
        Ok(GpuLayer {
            shader: GpuShader::produce(shader, gpu.clone())?,
            cached_mesh: GpuMesh::produce(mesh.clone(), gpu.clone())?,
            instances: InstanceCache {
                cache: VertexBuffer::dynamic(
                        gpu.as_ref(), 
                        &instances.iter()
                            .map(|_| {
                                MeshTransforms::from(mesh.src.center())
                            })
                            .collect::<Vec<MeshTransforms>>())?,
                sources: instances,
            },
            src: mesh,
        })
    }
}

impl GpuLayer {
    pub fn step(mut self, frame: usize) -> Result<Self> {
        self.cached_mesh.scale = self.src.scale.tween(frame);
        self.cached_mesh.center = [
            GpuVertex::fix_coord(self.src.origin_x.tween(frame)),
            GpuVertex::fix_coord(self.src.origin_y.tween(frame)),
        ];
        self.cached_mesh.rotation = self.src.rotation.tween(frame);
        self.instances = self.instances.step(frame);
        Ok(self)
    }

    pub fn render<'a>(&'a self) -> DrawCmd<'a, 'a, 'a> {
        DrawCmd {
            shader: &self.shader,
            mesh: &self.cached_mesh,
            instance_data: &self.instances.cache,
        }
    }
}

struct BufferSpec {
    pub width: u32,
    pub height: u32,
}

struct Buffer {
    targets: [Rc<Texture2d>; 2],
    blitter: (GpuShader, GpuMesh, InstanceCache),
}

impl Buffer {
    pub fn blitter<'a>(&'a self) -> DrawCmd<'a, 'a, 'a> {
        DrawCmd {
            shader: &self.blitter.0,
            mesh: &self.blitter.1,
            instance_data: &self.blitter.2.cache
        }
    }

    /// Draws commands to the buffer.
    pub fn draw(&self, frame: usize, cmds: Vec<DrawCmd>) -> Result<()> {
        let mut surfaces = [self.targets[0].as_surface(), self.targets[1].as_surface()];
        for cmd in cmds.into_iter() {
            let facade = UniformFacade {
                shader_uniforms: &cmd.shader.uniforms,
                last: &self.targets[1],
                frame: frame as u32,
            };
            surfaces[0].draw(
                    (cmd.mesh.vertices.as_ref(), cmd.instance_data.per_instance().unwrap()),
                    cmd.mesh.indices.as_ref(),
                    &cmd.shader.program,
                    &facade,
                    &DrawParameters {
                        smooth: None,
                        blend: cmd.mesh.blend,
                        ..Default::default()
                })?;

            surfaces[0].fill(&surfaces[1], MagnifySamplerFilter::Linear);
        }
        Ok(())
    }

    pub fn front(&self) -> Rc<Texture2d> {
        self.targets[0].clone()
    }
}

impl Factory<BufferSpec> for Buffer {
    fn produce(spec: BufferSpec, gpu: Rc<Gpu>) -> Result<Self> {
        let target = || -> Result<Texture2d> {
            Texture2d::empty_with_format(
                gpu.as_ref(),
                UncompressedFloatFormat::F32F32F32F32,
                MipmapsOption::AutoGeneratedMipmaps,
                spec.width,
                spec.height,
            ).map_err(Into::into)
        };

        let targets = [Rc::new(target()?), Rc::new(target()?)];
        for target in targets.iter() {
            target.as_ref().as_surface().clear_color(0.0, 0.0, 0.0, 1.0)
        }
        let blitter = (
            GpuShader {
                program: gpu.library.blit_shader.clone(),
                uniforms: GpuUniforms::Texture(targets[0].clone())
            },
            GpuMesh::produce(Mesh::from(Rect::frame()), gpu.clone())?,
            InstanceCache {
                sources: vec![Mesh::from(Rect::frame())],
                cache: VertexBuffer::dynamic(
                        gpu.as_ref(), 
                        &vec![MeshTransforms::from(Point::center())])?
            }
        );
        Ok(Self { targets, blitter })
    }
}

pub struct RenderSpec {
    pub width: u32,
    pub height: u32,
    pub composition: Composition,
}

pub struct Render {
    layers: Vec<GpuLayer>,
    buffer: Buffer,
}

impl Factory<RenderSpec> for Render {
    fn produce(spec: RenderSpec, gpu: Rc<Gpu>) -> Result<Render> {
        Ok(Render {
            layers: spec.composition
                .layers()
                .into_iter()
                .map(|l| -> Result<GpuLayer> {Factory::produce(l, gpu.clone())})
                .collect::<Result<Vec<GpuLayer>>>()?,
            buffer: Buffer::produce(
                BufferSpec {
                    width: spec.width,
                    height: spec.height,
                },
                gpu,
            )?,
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

    pub fn render<'a>(
        &'a self,
        frame: usize,
    ) -> Result<DrawCmd<'a, 'a, 'a>> {
        self.buffer.draw(frame, self.cmds())?;
        Ok(self.buffer.blitter())
    }

    pub fn buffer(&self) -> Rc<Texture2d> {
        self.buffer.front()
    }

    fn cmds<'a>(&'a self) -> Vec<DrawCmd<'a, 'a, 'a>> {
        self.layers.iter().map(|l| l.render()).collect()
    }
}
