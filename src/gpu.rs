//! GPU handle and types.

use crate::{
    raster::{raster_path, Method},
    uniforms::{uniform_bytes, UniformSet},
    Result,
};
use anyhow::{anyhow, ensure, Context as _};
use bytemuck::{Pod, Zeroable};
use itertools::Itertools;
use lyon_path::Builder;
use palette::LinSrgba;
use rand::random;
use rayon::prelude::*;
use std::{mem::size_of, sync::Arc, time::Duration};
use wgpu::util::DeviceExt;

const PRELUDE: &str = include_str!("shaders/prelude.wgsl");
const DEFAULT_SHADER: &str = include_str!("shaders/default.wgsl");
const PRESENT_SHADER: &str = include_str!("shaders/present.wgsl");

/// The format of rendered output: 8 bit sRGB encoded RGBA.
const OUTPUT_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Rgba8UnormSrgb;

/// A rectangle of pixels to shade, in output pixels from the top left.
#[repr(C)]
#[derive(Debug, Copy, Clone, Pod, Zeroable)]
pub(crate) struct Instance {
    pub rect: [f32; 4],
    pub color: [f32; 4],
    pub coverage: f32,
}

/// A shader which can be used to shade paths. See `ShaderProgram` for construction.
#[derive(Clone)]
pub struct Shader {
    id: u64,
    pipeline: Arc<wgpu::RenderPipeline>,
    uniforms: Arc<[u8]>,
}

/// A rasterable element in a composition.
pub struct Element {
    pub path: Builder,
    pub color: LinSrgba,
    pub raster_method: Method,
    pub shader: Shader,
}

/// Values every shader can read from the `valora` uniform.
pub(crate) struct FrameUniforms {
    pub scale: f32,
    pub time: Duration,
    pub sample_depth: amicola::SampleDepth,
}

/// Textures to render a composition into, and read it back from.
pub(crate) struct Target {
    pub width: u32,
    pub height: u32,
    /// Linear color, accumulated across frames.
    canvas: wgpu::TextureView,
    present_bind_group: wgpu::BindGroup,
    output: wgpu::Texture,
    output_view: wgpu::TextureView,
    readback: wgpu::Buffer,
    padded_bytes_per_row: u32,
}

/// A handle to the GPU for managing buffers and shaders.
#[derive(Clone)]
pub struct Gpu(Arc<GpuInner>);

struct GpuInner {
    device: wgpu::Device,
    queue: wgpu::Queue,
    canvas_format: wgpu::TextureFormat,
    frame_layout: wgpu::BindGroupLayout,
    uniforms_layout: wgpu::BindGroupLayout,
    pipeline_layout: wgpu::PipelineLayout,
    present_layout: wgpu::BindGroupLayout,
    present_pipeline: wgpu::RenderPipeline,
    default_pipeline: Arc<wgpu::RenderPipeline>,
}

impl Gpu {
    pub(crate) fn new() -> Result<Self> {
        pollster::block_on(Self::new_async())
    }

    async fn new_async() -> Result<Self> {
        let instance =
            wgpu::Instance::new(wgpu::InstanceDescriptor::new_without_display_handle_from_env());
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::HighPerformance,
                ..Default::default()
            })
            .await
            .context("finding a GPU adapter")?;

        // Blending into 32 bit float textures is an optional feature; 16 bit
        // float textures are blendable everywhere.
        let float32_blendable = adapter
            .features()
            .contains(wgpu::Features::FLOAT32_BLENDABLE);
        let (required_features, canvas_format) = match float32_blendable {
            true => (
                wgpu::Features::FLOAT32_BLENDABLE,
                wgpu::TextureFormat::Rgba32Float,
            ),
            false => (wgpu::Features::empty(), wgpu::TextureFormat::Rgba16Float),
        };
        let (device, queue) = adapter
            .request_device(&wgpu::DeviceDescriptor {
                label: Some("valora"),
                required_features,
                // Large compositions need large textures, and buffers to read
                // them back.
                required_limits: wgpu::Limits {
                    max_buffer_size: adapter.limits().max_buffer_size,
                    ..wgpu::Limits::default().using_resolution(adapter.limits())
                },
                ..Default::default()
            })
            .await
            .context("opening the GPU device")?;

        let uniform_entry = wgpu::BindGroupLayoutEntry {
            binding: 0,
            visibility: wgpu::ShaderStages::VERTEX_FRAGMENT,
            ty: wgpu::BindingType::Buffer {
                ty: wgpu::BufferBindingType::Uniform,
                has_dynamic_offset: false,
                min_binding_size: None,
            },
            count: None,
        };
        let frame_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("valora frame uniforms"),
            entries: &[uniform_entry],
        });
        let uniforms_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("valora shader uniforms"),
            entries: &[uniform_entry],
        });
        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("valora"),
            bind_group_layouts: &[Some(&frame_layout), Some(&uniforms_layout)],
            immediate_size: 0,
        });

        let present_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("valora present"),
            entries: &[wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStages::FRAGMENT,
                ty: wgpu::BindingType::Texture {
                    sample_type: wgpu::TextureSampleType::Float { filterable: false },
                    view_dimension: wgpu::TextureViewDimension::D2,
                    multisampled: false,
                },
                count: None,
            }],
        });
        let present_module = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("valora present"),
            source: wgpu::ShaderSource::Wgsl(PRESENT_SHADER.into()),
        });
        let present_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("valora present"),
            layout: Some(
                &device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                    label: Some("valora present"),
                    bind_group_layouts: &[Some(&present_layout)],
                    immediate_size: 0,
                }),
            ),
            vertex: wgpu::VertexState {
                module: &present_module,
                entry_point: Some("vertex"),
                compilation_options: Default::default(),
                buffers: &[],
            },
            primitive: wgpu::PrimitiveState::default(),
            depth_stencil: None,
            multisample: wgpu::MultisampleState::default(),
            fragment: Some(wgpu::FragmentState {
                module: &present_module,
                entry_point: Some("fragment"),
                compilation_options: Default::default(),
                targets: &[Some(OUTPUT_FORMAT.into())],
            }),
            multiview_mask: None,
            cache: None,
        });

        let default_pipeline =
            compile_pipeline(&device, &pipeline_layout, canvas_format, DEFAULT_SHADER)?;

        Ok(Gpu(Arc::new(GpuInner {
            device,
            queue,
            canvas_format,
            frame_layout,
            uniforms_layout,
            pipeline_layout,
            present_layout,
            present_pipeline,
            default_pipeline,
        })))
    }

    pub(crate) fn default_shader(&self) -> Shader {
        Shader {
            id: random(),
            pipeline: self.0.default_pipeline.clone(),
            uniforms: uniform_bytes(&()).into(),
        }
    }

    /// Compiles a WGSL shader which defines `fn shade(fragment: Fragment) -> vec4<f32>`.
    pub(crate) fn compile(&self, source: &str) -> Result<Arc<wgpu::RenderPipeline>> {
        compile_pipeline(
            &self.0.device,
            &self.0.pipeline_layout,
            self.0.canvas_format,
            source,
        )
    }

    pub(crate) fn build_shader(
        &self,
        pipeline: Arc<wgpu::RenderPipeline>,
        uniforms: &impl UniformSet,
    ) -> Shader {
        Shader {
            id: random(),
            pipeline,
            uniforms: uniform_bytes(uniforms).into(),
        }
    }

    /// The largest width or height of a target.
    pub(crate) fn max_dimension(&self) -> u32 {
        self.0.device.limits().max_texture_dimension_2d
    }

    /// Returns an error if a target of the given size cannot be created.
    pub(crate) fn check_target_size(&self, width: u32, height: u32) -> Result<()> {
        ensure!(
            width > 0 && height > 0,
            "the output must be at least one pixel wide and tall, but would be {}x{}",
            width,
            height
        );
        let max = self.max_dimension();
        ensure!(
            width <= max && height <= max,
            "the output may be at most {} pixels on each side, but would be {}x{}",
            max,
            width,
            height
        );
        let readback_size = padded_bytes_per_row(width) as u64 * height as u64;
        let max_buffer_size = self.0.device.limits().max_buffer_size;
        ensure!(
            readback_size <= max_buffer_size,
            "the output may be at most {} bytes, but {}x{} would be {} bytes",
            max_buffer_size,
            width,
            height,
            readback_size
        );
        Ok(())
    }

    /// Creates a cleared target to render into.
    pub(crate) fn create_target(&self, width: u32, height: u32) -> Target {
        let device = &self.0.device;
        let size = wgpu::Extent3d {
            width,
            height,
            depth_or_array_layers: 1,
        };
        let texture = |label, format, usage| {
            device.create_texture(&wgpu::TextureDescriptor {
                label: Some(label),
                size,
                mip_level_count: 1,
                sample_count: 1,
                dimension: wgpu::TextureDimension::D2,
                format,
                usage,
                view_formats: &[],
            })
        };

        let canvas = texture(
            "valora canvas",
            self.0.canvas_format,
            wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::TEXTURE_BINDING,
        )
        .create_view(&Default::default());
        let output = texture(
            "valora output",
            OUTPUT_FORMAT,
            wgpu::TextureUsages::RENDER_ATTACHMENT | wgpu::TextureUsages::COPY_SRC,
        );
        let output_view = output.create_view(&Default::default());
        let present_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("valora present"),
            layout: &self.0.present_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::TextureView(&canvas),
            }],
        });

        let padded_bytes_per_row = padded_bytes_per_row(width);
        let readback = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("valora readback"),
            size: padded_bytes_per_row as u64 * height as u64,
            usage: wgpu::BufferUsages::COPY_DST | wgpu::BufferUsages::MAP_READ,
            mapped_at_creation: false,
        });

        Target {
            width,
            height,
            canvas,
            present_bind_group,
            output,
            output_view,
            readback,
            padded_bytes_per_row,
        }
    }

    /// Paints elements over the target's canvas.
    pub(crate) fn render(
        &self,
        target: &Target,
        elements: Vec<Element>,
        frame: FrameUniforms,
    ) -> Result<()> {
        let GpuInner { device, queue, .. } = self.0.as_ref();
        let (width, height) = (target.width, target.height);

        let rastered: Vec<(Shader, Vec<Instance>)> = elements
            .into_par_iter()
            .map(|element| {
                let instances = raster_path(
                    element.path,
                    element.raster_method,
                    element.color,
                    width,
                    height,
                    frame.sample_depth,
                );
                (element.shader, instances)
            })
            .collect();

        // Draw consecutive elements with the same shader in one call.
        let mut instances = vec![];
        let mut batches = vec![];
        for (_, batch) in &rastered.into_iter().group_by(|(shader, _)| shader.id) {
            let start = instances.len() as u32;
            let mut shader = None;
            for (element_shader, element_instances) in batch {
                instances.extend(element_instances);
                shader = Some(element_shader);
            }
            let shader = shader.expect("groups are never empty");
            batches.push((shader, start..instances.len() as u32));
        }

        let errors = device.push_error_scope(wgpu::ErrorFilter::Validation);

        let frame_uniforms = [
            width as f32,
            height as f32,
            frame.scale,
            frame.time.as_secs_f32(),
        ];
        let frame_bind_group = self
            .0
            .uniform_bind_group(&self.0.frame_layout, bytemuck::cast_slice(&frame_uniforms));
        let batches: Vec<_> = batches
            .into_iter()
            .filter(|(_, range)| !range.is_empty())
            .map(|(shader, range)| {
                let bind_group = self
                    .0
                    .uniform_bind_group(&self.0.uniforms_layout, &shader.uniforms);
                (shader.pipeline, bind_group, range)
            })
            .collect();
        let instance_buffer = (!instances.is_empty()).then(|| {
            device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("valora instances"),
                contents: bytemuck::cast_slice(&instances),
                usage: wgpu::BufferUsages::VERTEX,
            })
        });

        let mut encoder = device.create_command_encoder(&Default::default());
        if let Some(instance_buffer) = &instance_buffer {
            let mut pass = begin_pass(&mut encoder, &target.canvas, wgpu::LoadOp::Load);
            pass.set_bind_group(0, &frame_bind_group, &[]);
            pass.set_vertex_buffer(0, instance_buffer.slice(..));
            for (pipeline, bind_group, range) in &batches {
                pass.set_pipeline(pipeline);
                pass.set_bind_group(1, bind_group, &[]);
                pass.draw(0..4, range.clone());
            }
        }
        {
            let mut pass = begin_pass(
                &mut encoder,
                &target.output_view,
                wgpu::LoadOp::Clear(wgpu::Color::TRANSPARENT),
            );
            pass.set_pipeline(&self.0.present_pipeline);
            pass.set_bind_group(0, &target.present_bind_group, &[]);
            pass.draw(0..3, 0..1);
        }
        queue.submit([encoder.finish()]);

        match pollster::block_on(errors.pop()) {
            Some(error) => Err(anyhow!("rendering a frame: {}", error)),
            None => Ok(()),
        }
    }

    /// Reads the target's output as sRGB encoded RGBA8, in rows from the top.
    pub(crate) fn read(&self, target: &Target) -> Result<Vec<u8>> {
        let GpuInner { device, queue, .. } = self.0.as_ref();

        let mut encoder = device.create_command_encoder(&Default::default());
        encoder.copy_texture_to_buffer(
            target.output.as_image_copy(),
            wgpu::TexelCopyBufferInfo {
                buffer: &target.readback,
                layout: wgpu::TexelCopyBufferLayout {
                    offset: 0,
                    bytes_per_row: Some(target.padded_bytes_per_row),
                    rows_per_image: None,
                },
            },
            target.output.size(),
        );
        queue.submit([encoder.finish()]);

        let (sender, receiver) = std::sync::mpsc::channel();
        target
            .readback
            .map_async(wgpu::MapMode::Read, .., move |result| {
                let _ = sender.send(result);
            });
        device.poll(wgpu::PollType::wait_indefinitely())?;
        receiver.recv()??;

        let row_len = target.width as usize * 4;
        let pixels = {
            let mapped = target.readback.get_mapped_range(..)?;
            mapped
                .chunks(target.padded_bytes_per_row as usize)
                .flat_map(|row| &row[..row_len])
                .copied()
                .collect()
        };
        target.readback.unmap();

        Ok(pixels)
    }
}

impl GpuInner {
    fn uniform_bind_group(&self, layout: &wgpu::BindGroupLayout, bytes: &[u8]) -> wgpu::BindGroup {
        let buffer = self
            .device
            .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("valora uniforms"),
                contents: bytes,
                usage: wgpu::BufferUsages::UNIFORM,
            });
        self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("valora uniforms"),
            layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: buffer.as_entire_binding(),
            }],
        })
    }
}

/// Compiles a WGSL shader which defines `fn shade(fragment: Fragment) -> vec4<f32>`.
fn compile_pipeline(
    device: &wgpu::Device,
    layout: &wgpu::PipelineLayout,
    canvas_format: wgpu::TextureFormat,
    source: &str,
) -> Result<Arc<wgpu::RenderPipeline>> {
    // The shader comes first so that error line numbers match its file.
    let source = format!("{}\n{}", source, PRELUDE);

    let errors = device.push_error_scope(wgpu::ErrorFilter::Validation);
    let module = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: Some("valora shader"),
        source: wgpu::ShaderSource::Wgsl(source.into()),
    });
    let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
        label: Some("valora shader"),
        layout: Some(layout),
        vertex: wgpu::VertexState {
            module: &module,
            entry_point: Some("valora_vertex"),
            compilation_options: Default::default(),
            buffers: &[Some(wgpu::VertexBufferLayout {
                array_stride: size_of::<Instance>() as u64,
                step_mode: wgpu::VertexStepMode::Instance,
                attributes: &wgpu::vertex_attr_array![
                    0 => Float32x4,
                    1 => Float32x4,
                    2 => Float32,
                ],
            })],
        },
        primitive: wgpu::PrimitiveState {
            topology: wgpu::PrimitiveTopology::TriangleStrip,
            ..Default::default()
        },
        depth_stencil: None,
        multisample: wgpu::MultisampleState::default(),
        fragment: Some(wgpu::FragmentState {
            module: &module,
            entry_point: Some("valora_fragment"),
            compilation_options: Default::default(),
            targets: &[Some(wgpu::ColorTargetState {
                format: canvas_format,
                blend: Some(wgpu::BlendState {
                    color: wgpu::BlendComponent {
                        src_factor: wgpu::BlendFactor::SrcAlpha,
                        dst_factor: wgpu::BlendFactor::OneMinusSrcAlpha,
                        operation: wgpu::BlendOperation::Add,
                    },
                    alpha: wgpu::BlendComponent {
                        src_factor: wgpu::BlendFactor::One,
                        dst_factor: wgpu::BlendFactor::OneMinusSrcAlpha,
                        operation: wgpu::BlendOperation::Add,
                    },
                }),
                write_mask: wgpu::ColorWrites::ALL,
            })],
        }),
        multiview_mask: None,
        cache: None,
    });

    match pollster::block_on(errors.pop()) {
        Some(error) => Err(anyhow!("{}", error)),
        None => Ok(Arc::new(pipeline)),
    }
}

/// The length of a row of RGBA8 pixels, padded for copying out of a texture.
fn padded_bytes_per_row(width: u32) -> u32 {
    (width * 4).next_multiple_of(wgpu::COPY_BYTES_PER_ROW_ALIGNMENT)
}

fn begin_pass<'e>(
    encoder: &'e mut wgpu::CommandEncoder,
    view: &wgpu::TextureView,
    load: wgpu::LoadOp<wgpu::Color>,
) -> wgpu::RenderPass<'e> {
    encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
        label: Some("valora"),
        color_attachments: &[Some(wgpu::RenderPassColorAttachment {
            view,
            depth_slice: None,
            resolve_target: None,
            ops: wgpu::Operations {
                load,
                store: wgpu::StoreOp::Store,
            },
        })],
        ..Default::default()
    })
}
