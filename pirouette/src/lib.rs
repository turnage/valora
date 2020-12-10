use anyhow::Result;
use bytemuck::{Pod, Zeroable};
use byteorder::{BigEndian, ByteOrder};
use derivative::Derivative;
use float_ord::FloatOrd;
use image::{ImageBuffer, Rgba};
use shaderc::*;
use std::rc::Rc;
use std::{
    any::{Any, TypeId},
    collections::HashMap,
    path::Path,
};
use wgpu::{util::DeviceExt, *};

#[repr(C)]
#[derive(Copy, Clone, Debug, Pod, Zeroable)]
pub struct Scope {
    pub scale: f32,
    pub dimensions: [f32; 2],
    pub offset: [f32; 2],
}

impl Scope {
    fn width(self) -> u32 {
        self.dimensions[0] as u32
    }
    fn height(self) -> u32 {
        self.dimensions[1] as u32
    }

    fn texel_count(self) -> u32 {
        (self.width() * self.height()) as u32
    }

    fn bind_group_layout_entry() -> BindGroupLayoutEntry {
        BindGroupLayoutEntry {
            binding: 0,
            visibility: ShaderStage::VERTEX,
            ty: BindingType::UniformBuffer {
                dynamic: false,
                min_binding_size: None,
            },
            count: None,
        }
    }
}

pub trait Lay {
    fn layout(device: &Device) -> Option<BindGroupLayout>;
}

#[repr(C)]
#[derive(Debug, Clone, Copy, Pod, Zeroable)]
pub struct NullUniforms;

impl Lay for NullUniforms {
    fn layout(device: &Device) -> Option<BindGroupLayout> {
        None
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug, Pod, Zeroable)]
pub struct Vertex {
    pub position: [f32; 2],
    pub color: [f32; 4],
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum Format {
    /// RGBA32 Float. Use this for buffers you plan to mix and
    /// blend further.
    MixFidelity,
    /// BGRA8 Unorm Srgb. Use this for buffers to export.
    Final,
}

impl Format {
    fn texel_size(self) -> u32 {
        match self {
            Format::MixFidelity => 16, // 4 bytes per channel
            Format::Final => 4,        // 1 byte per channel
        }
    }
}

impl Format {
    fn process_rows<'a>(self, rows: impl Iterator<Item = &'a [u8]>) -> Vec<u8> {
        match self {
            Format::MixFidelity => rows
                .flat_map(|row| row.chunks_exact(4))
                .map(BigEndian::read_f32)
                .map(|f| (f * 255.).floor() as u8)
                .collect(),
            Format::Final => rows.flatten().copied().collect(),
        }
    }
}

impl From<Format> for TextureFormat {
    fn from(format: Format) -> TextureFormat {
        match format {
            Format::MixFidelity => TextureFormat::Rgba32Float,
            Format::Final => TextureFormat::Bgra8UnormSrgb,
        }
    }
}

#[derive(Debug)]
struct ReadyPipeline {
    render_pipeline: RenderPipeline,
    scope_layout: BindGroupLayout,
    uniforms_layout: Option<BindGroupLayout>,
}

#[derive(Debug)]
pub struct Renderer {
    pipelines: HashMap<RenderProfile, ReadyPipeline>,
    vertex_shader: ShaderModule,
    fragment_shader: ShaderModule,
}

#[derive(Copy, Clone, Hash, PartialEq, Eq, Derivative)]
#[derivative(Debug)]
struct RenderProfile {
    #[derivative(Debug = "ignore")]
    scale: FloatOrd<f32>,
    #[derivative(Debug = "ignore")]
    width: FloatOrd<f32>,
    #[derivative(Debug = "ignore")]
    height: FloatOrd<f32>,
    uniforms_type: TypeId,
    format: TextureFormat,
}

/// A draw instruction.
#[derive(Debug)]
pub struct Draw<'a> {
    device: &'a Device,
    queue: &'a Queue,
    target: &'a RenderTarget,
    command_encoder: CommandEncoder,
}

fn align_up_256(x: u32) -> u32 {
    ((x / 256) + 1) * 256
}

impl<'a> Draw<'a> {
    pub fn write_out(mut self, path: impl AsRef<Path>) -> Result<()> {
        let scope = self.target.scope;
        let format = self.target.format;

        let unpadded_row_length = scope.width() * format.texel_size();
        let padded_row_length = align_up_256(unpadded_row_length);
        let buffer = self.device.create_buffer_init(&util::BufferInitDescriptor {
            label: None,
            contents: vec![5; (scope.height() * padded_row_length) as usize].as_slice(),
            usage: BufferUsage::COPY_DST | BufferUsage::MAP_READ,
        });

        self.command_encoder.copy_texture_to_buffer(
            TextureCopyViewBase {
                texture: &self.target.texture,
                mip_level: 0,
                origin: Origin3d::ZERO,
            },
            BufferCopyViewBase {
                buffer: &buffer,
                layout: TextureDataLayout {
                    offset: 0,
                    bytes_per_row: padded_row_length,
                    rows_per_image: scope.height(),
                },
            },
            Extent3d {
                width: scope.width(),
                height: scope.height(),
                depth: 1,
            },
        );

        let command = self.command_encoder.finish();
        self.queue.submit(std::iter::once(command));
        self.device.poll(Maintain::Wait);

        let slice = buffer.slice(..);
        let map_request = slice.map_async(MapMode::Read);
        self.device.poll(Maintain::Wait);
        futures::executor::block_on(map_request)?;

        let image = read_gpu_buffer(
            format,
            padded_row_length,
            unpadded_row_length,
            scope.width(),
            scope.height(),
            &slice.get_mapped_range(),
        );

        Ok(image.save(path)?)
    }
}

#[derive(Debug)]
pub struct RenderTarget {
    scope: Scope,
    texture: Texture,
    format: Format,
}

fn read_gpu_buffer(
    format: Format,
    padded_row_length: u32,
    unpadded_row_length: u32,
    width: u32,
    height: u32,
    buffer: &[u8],
) -> ImageBuffer<Rgba<u8>, Vec<u8>> {
    use image::buffer::ConvertBuffer;

    let data = format.process_rows(
        buffer
            .chunks_exact(padded_row_length as _)
            .enumerate()
            .map(|(i, c)| {
                if i < 10 {
                    println!("c: {:?}", c);
                }
                c
            })
            .map(|chunk| (&chunk[..(unpadded_row_length as _)])),
    );

    ImageBuffer::from_raw(width, height, data).unwrap()
}

impl Renderer {
    pub fn new(device: &Device) -> Self {
        let vertex_source = include_spirv!("../shaders/default.vert.spv");
        let fragment_source = include_spirv!("../shaders/default.frag.spv");
        Self {
            pipelines: HashMap::default(),
            vertex_shader: device.create_shader_module(vertex_source),
            fragment_shader: device.create_shader_module(fragment_source),
        }
    }

    pub fn begin_draw<'a, U>(
        &mut self,
        device: &'a Device,
        queue: &'a Queue,
        target: &'a RenderTarget,
        lines: &[Vertex],
        uniforms: U,
        fragment_shader: Option<Rc<ShaderModule>>,
    ) -> Draw<'a>
    where
        U: Any + Lay + Pod,
    {
        let mut command_encoder = device.create_command_encoder(&CommandEncoderDescriptor {
            label: Some("Render pass encoder"),
        });

        let scope_buffer = device.create_buffer_init(&util::BufferInitDescriptor {
            label: Some("Scope uniform buffer"),
            contents: bytemuck::cast_slice(&[target.scope]),
            usage: BufferUsage::UNIFORM,
        });

        let vertex_buffer = device.create_buffer_init(&util::BufferInitDescriptor {
            label: Some("Lines buffer"),
            contents: bytemuck::cast_slice(lines),
            usage: BufferUsage::VERTEX,
        });

        let pipeline = self.load_pipeline(device, target.format.into(), target.scope, &uniforms);

        let scope_bind_group = device.create_bind_group(&BindGroupDescriptor {
            label: Some("Scope bind group"),
            layout: &pipeline.scope_layout,
            entries: &[BindGroupEntry {
                binding: 0,
                resource: BindingResource::Buffer(scope_buffer.slice(..)),
            }],
        });

        let target_view = target
            .texture
            .create_view(&TextureViewDescriptor::default());

        {
            let mut render_pass = command_encoder.begin_render_pass(&RenderPassDescriptor {
                depth_stencil_attachment: None,
                color_attachments: &[RenderPassColorAttachmentDescriptor {
                    attachment: &target_view,
                    resolve_target: None,
                    ops: Operations {
                        load: LoadOp::Clear(Color {
                            r: 1.,
                            g: 0.,
                            b: 0.,
                            a: 1.,
                        }),
                        store: true,
                    },
                }],
            });

            render_pass.set_pipeline(&pipeline.render_pipeline);
            render_pass.set_vertex_buffer(0, vertex_buffer.slice(..));
            render_pass.set_bind_group(0, &scope_bind_group, &[]);
            if let Some(uniforms_layout) = pipeline.uniforms_layout.as_ref() {
                let uniforms_buffer = device.create_buffer_init(&util::BufferInitDescriptor {
                    label: Some("Scope uniform buffer"),
                    contents: bytemuck::cast_slice(&[uniforms]),
                    usage: BufferUsage::UNIFORM,
                });

                let uniforms_bind_group = device.create_bind_group(&BindGroupDescriptor {
                    label: Some("uniforms bind group"),
                    layout: &uniforms_layout,
                    entries: &[BindGroupEntry {
                        binding: 0,
                        resource: BindingResource::Buffer(uniforms_buffer.slice(..)),
                    }],
                });
            }
            render_pass.draw(0..(lines.len() as u32), 0..1);
        }

        Draw {
            device,
            queue,
            command_encoder,
            target,
        }
    }

    fn load_pipeline<U>(
        &mut self,
        device: &Device,
        format: TextureFormat,
        scope: Scope,
        uniforms: &U,
    ) -> &ReadyPipeline
    where
        U: Any + Lay,
    {
        let key = RenderProfile {
            scale: FloatOrd(scope.scale),
            width: FloatOrd(scope.dimensions[0]),
            height: FloatOrd(scope.dimensions[1]),
            uniforms_type: uniforms.type_id(),
            format,
        };

        if self.pipelines.get(&key).is_none() {
            self.pipelines
                .insert(key, self.create_pipeline(device, format, scope, uniforms));
        }

        self.pipelines.get(&key).unwrap()
    }

    pub fn create_target(device: &Device, format: Format, scope: Scope) -> RenderTarget {
        let label = format!("{:?} - {:?}", format, scope);
        let texture = device.create_texture(&TextureDescriptor {
            label: Some(label.as_str()),
            size: Extent3d {
                width: scope.width(),
                height: scope.height(),
                depth: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: TextureDimension::D2,
            format: format.into(),
            usage: TextureUsage::COPY_SRC | TextureUsage::OUTPUT_ATTACHMENT,
        });

        RenderTarget {
            texture,
            format,
            scope,
        }
    }

    fn create_pipeline<U>(
        &self,
        device: &Device,
        format: TextureFormat,
        scope: Scope,
        uniforms: &U,
    ) -> ReadyPipeline
    where
        U: Any + Lay,
    {
        let uniform_id = format!("Uniform Type {:?}", uniforms.type_id());
        let uniform_id = uniform_id.as_str();

        let scope_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("Scope bind group"),
            entries: &[Scope::bind_group_layout_entry()],
        });
        let uniforms_layout = U::layout(device);

        let pipeline_layout = {
            let mut groups = vec![];
            groups.push(&scope_layout);
            uniforms_layout
                .iter()
                .for_each(|layout| groups.push(layout));

            device.create_pipeline_layout(&PipelineLayoutDescriptor {
                label: Some(uniform_id),
                bind_group_layouts: groups.as_slice(),
                push_constant_ranges: &[],
            })
        };

        let alpha_over = BlendDescriptor {
            src_factor: BlendFactor::One,
            dst_factor: BlendFactor::OneMinusSrcAlpha,
            operation: BlendOperation::Add,
        };

        let render_pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some(uniform_id),
            layout: Some(&pipeline_layout),
            primitive_topology: PrimitiveTopology::TriangleList,
            rasterization_state: None,
            depth_stencil_state: None,
            color_states: &[ColorStateDescriptor {
                format,
                alpha_blend: alpha_over.clone(),
                color_blend: alpha_over,
                write_mask: ColorWrite::ALL,
            }],
            vertex_state: VertexStateDescriptor {
                index_format: IndexFormat::Uint16,
                vertex_buffers: &[VertexBufferDescriptor {
                    stride: std::mem::size_of::<Vertex>() as BufferAddress,
                    step_mode: InputStepMode::Vertex,
                    attributes: &vertex_attr_array![
                        0 => Float2,
                        1 => Float4
                    ],
                }],
            },
            sample_count: 1,
            sample_mask: !0,
            alpha_to_coverage_enabled: false,
            vertex_stage: ProgrammableStageDescriptor {
                module: &self.vertex_shader,
                entry_point: "main",
            },
            fragment_stage: Some(ProgrammableStageDescriptor {
                module: &self.fragment_shader,
                entry_point: "main",
            }),
        });

        ReadyPipeline {
            render_pipeline,
            scope_layout,
            uniforms_layout,
        }
    }
}
