use anyhow::{Context, Result};
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
    pub offset: [f32; 2],
    pub dimensions: [u32; 2],
}

impl Scope {
    fn bind_group_layout_entry() -> BindGroupLayoutEntry {
        BindGroupLayoutEntry {
            binding: 0,
            visibility: ShaderStage::COMPUTE,
            ty: BindingType::StorageBuffer {
                dynamic: false,
                min_binding_size: None,
                readonly: true,
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

fn align_up(x: u32) -> u32 {
    (x / 256) * 256 + 256 - x % 256
}

#[derive(Debug)]
pub struct Renderer {
    lines_layout: BindGroupLayout,
    scope_layout: BindGroupLayout,
    surfaces_layout: BindGroupLayout,
    render_pipeline: ComputePipeline,
    export_pipeline: ComputePipeline,
}

#[derive(Debug)]
pub struct Job {
    scope: Scope,
    background: Buffer,
    foreground: Buffer,
    export: Buffer,
    cpu_stage: Buffer,
}

impl Renderer {
    pub fn new(device: &Device) -> Self {
        let render_shader =
            device.create_shader_module(include_spirv!("../shaders/render.comp.spv"));
        let export_shader =
            device.create_shader_module(include_spirv!("../shaders/export.comp.spv"));

        let compute_binding_entry = |i, readonly| BindGroupLayoutEntry {
            binding: i,
            visibility: ShaderStage::COMPUTE,
            ty: BindingType::StorageBuffer {
                dynamic: false,
                min_binding_size: None,
                readonly,
            },
            count: None,
        };
        let scope_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("Scope"),
            entries: &[compute_binding_entry(0, /*readonly=*/ true)],
        });

        let lines_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("Lines"),
            entries: &[compute_binding_entry(0, /*readonly=*/ true)],
        });

        let surfaces_layout = device.create_bind_group_layout(&BindGroupLayoutDescriptor {
            label: Some("Surfaces"),
            entries: &[
                compute_binding_entry(0, /*readonly=*/ false),
                compute_binding_entry(1, /*readonly=*/ false),
            ],
        });

        let render_pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("Render Layout"),
            bind_group_layouts: &[&scope_layout, &lines_layout, &surfaces_layout],
            push_constant_ranges: &[],
        });
        let render_pipeline = device.create_compute_pipeline(&ComputePipelineDescriptor {
            label: Some("Render"),
            layout: Some(&render_pipeline_layout),
            compute_stage: ProgrammableStageDescriptor {
                module: &render_shader,
                entry_point: "main",
            },
        });

        let export_pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("Export Layout"),
            bind_group_layouts: &[&scope_layout, &surfaces_layout],
            push_constant_ranges: &[],
        });
        let export_pipeline = device.create_compute_pipeline(&ComputePipelineDescriptor {
            label: Some("Export"),
            layout: Some(&export_pipeline_layout),
            compute_stage: ProgrammableStageDescriptor {
                module: &export_shader,
                entry_point: "main",
            },
        });

        Self {
            lines_layout,
            scope_layout,
            surfaces_layout,
            render_pipeline,
            export_pipeline,
        }
    }

    pub fn job(&self, scope: Scope, device: &Device) -> Job {
        let [width, height] = scope.dimensions;
        let buffer = |label, contents, usage| {
            device.create_buffer_init(&util::BufferInitDescriptor {
                label: Some(label),
                contents,
                usage,
            })
        };

        let mix_buffer_size = (16 * width * height) as usize;
        let export_buffer_size = (4 * width * height) as usize;
        let contents = vec![0u8; mix_buffer_size];

        Job {
            scope,
            background: buffer("background", &contents, BufferUsage::STORAGE),
            foreground: buffer("foreground", &contents, BufferUsage::STORAGE),
            export: buffer(
                "export",
                vec![50; export_buffer_size].as_slice(), //&contents[0..export_buffer_size],
                BufferUsage::STORAGE | BufferUsage::COPY_SRC,
            ),
            cpu_stage: buffer(
                "export",
                &contents[0..export_buffer_size],
                BufferUsage::COPY_DST | BufferUsage::MAP_READ,
            ),
        }
    }

    pub fn render(&self, job: &Job, lines: &[Vertex], device: &Device) -> CommandBuffer {
        let mut encoder = device.create_command_encoder(&CommandEncoderDescriptor {
            label: Some("render"),
        });

        let scope_buffer = device.create_buffer_init(&util::BufferInitDescriptor {
            label: Some("scope"),
            contents: bytemuck::cast_slice(&[job.scope]),
            usage: BufferUsage::STORAGE,
        });
        let scope_group = device.create_bind_group(&BindGroupDescriptor {
            label: Some("scope"),
            layout: &self.scope_layout,
            entries: &[BindGroupEntry {
                binding: 0,
                resource: BindingResource::Buffer(scope_buffer.slice(..)),
            }],
        });

        let lines_buffer = device.create_buffer_init(&util::BufferInitDescriptor {
            label: Some("lines"),
            contents: bytemuck::cast_slice(lines),
            usage: BufferUsage::STORAGE,
        });
        let lines_group = device.create_bind_group(&BindGroupDescriptor {
            label: Some("lines"),
            layout: &self.lines_layout,
            entries: &[BindGroupEntry {
                binding: 0,
                resource: BindingResource::Buffer(lines_buffer.slice(..)),
            }],
        });

        let surfaces_group = device.create_bind_group(&BindGroupDescriptor {
            label: Some("surfaces"),
            layout: &self.surfaces_layout,
            entries: &[
                BindGroupEntry {
                    binding: 0,
                    resource: BindingResource::Buffer(job.background.slice(..)),
                },
                BindGroupEntry {
                    binding: 1,
                    resource: BindingResource::Buffer(job.foreground.slice(..)),
                },
            ],
        });

        {
            let mut compute_pass = encoder.begin_compute_pass();
            compute_pass.set_pipeline(&self.render_pipeline);
            compute_pass.set_bind_group(0, &scope_group, &[]);
            compute_pass.set_bind_group(1, &lines_group, &[]);
            compute_pass.set_bind_group(2, &surfaces_group, &[]);
            //compute_pass.dispatch(job.scope.dimensions[0] as u32, (lines.len() / 2) as u32, 1);
            compute_pass.dispatch(64, 64, 1);
        }

        encoder.finish()
    }

    pub fn export(
        &self,
        job: &Job,
        device: &Device,
        queue: &Queue,
        f: impl Fn(ImageBuffer<Rgba<u8>, &[u8]>) -> Result<()>,
    ) -> Result<()> {
        let mut encoder = device.create_command_encoder(&CommandEncoderDescriptor {
            label: Some("export"),
        });

        let scope_buffer = device.create_buffer_init(&util::BufferInitDescriptor {
            label: Some("scope"),
            contents: bytemuck::cast_slice(&[job.scope]),
            usage: BufferUsage::STORAGE,
        });
        let scope_group = device.create_bind_group(&BindGroupDescriptor {
            label: Some("scope"),
            layout: &self.scope_layout,
            entries: &[BindGroupEntry {
                binding: 0,
                resource: BindingResource::Buffer(scope_buffer.slice(..)),
            }],
        });

        let surfaces_group = device.create_bind_group(&BindGroupDescriptor {
            label: Some("surfaces"),
            layout: &self.surfaces_layout,
            entries: &[
                BindGroupEntry {
                    binding: 0,
                    resource: BindingResource::Buffer(job.foreground.slice(..)),
                },
                BindGroupEntry {
                    binding: 1,
                    resource: BindingResource::Buffer(job.export.slice(..)),
                },
            ],
        });

        let [width, height] = job.scope.dimensions;
        {
            let mut compute_pass = encoder.begin_compute_pass();
            compute_pass.set_pipeline(&self.export_pipeline);
            compute_pass.set_bind_group(0, &scope_group, &[]);
            compute_pass.set_bind_group(1, &surfaces_group, &[]);
            compute_pass.dispatch(64, 64, 1);
        }
        encoder.copy_buffer_to_buffer(
            &job.export,
            0,
            &job.cpu_stage,
            0,
            (width * height * 4) as u64,
        );

        let result = {
            queue.submit(std::iter::once(encoder.finish()));
            let cpu_stage = job.cpu_stage.slice(..);
            let request = cpu_stage.map_async(MapMode::Read);
            device.poll(Maintain::Wait);
            futures::executor::block_on(request)?;

            let raw: &[u8] = &cpu_stage.get_mapped_range();
            let pixels: usize = (width * height) as usize;
            let image =
                ImageBuffer::from_raw(width as u32, height as u32, &raw[..pixels * 4]).unwrap();
            f(image)
        };

        job.cpu_stage.unmap();

        result.context("Exporting to u8 image")
    }
}
