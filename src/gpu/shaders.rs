use errors::Result;
use glium::texture::{RawImage2d, Texture2d};
use gpu::{Factory, Gpu, GpuMesh};
use image::{ImageBuffer, Rgb};
use std::rc::Rc;
use glium::draw_parameters::{DrawParameters, Smooth};
use gpu::programs::Library;
use glium::uniforms::{MagnifySamplerFilter, MinifySamplerFilter, UniformBuffer};
use glium::{Program, Surface};
use palette::{Blend, Colora};
use poly::Point;
use tween::Tween;

pub const MAX_VORONOI_SITES: usize = 1024;

pub enum GpuShader {
    Default,
    Texture(Rc<Texture2d>),
    Voronoi(GpuVoronoi),
    Custom(Rc<Program>),
    Intermittent {
        src: Rc<GpuShader>,
        predicate: Rc<Fn(usize) -> bool>,
    },
    NearFilter(NearFilterCfg),
}

impl GpuShader {
    pub fn draw<S: Surface>(
        &self,
        lib: &Library,
        frame: usize,
        surface: &mut S,
        mesh: &GpuMesh,
        last: Option<&Texture2d>,
    ) -> Result<()> {
        match *self {
            GpuShader::Default => Ok(surface.draw(
                mesh.vertices.as_ref(),
                mesh.indices.as_ref(),
                &lib.default_shader,
                &uniform!{
                    scale: mesh.scale,
                    root_center: mesh.root_center,
                    center: mesh.center,
                    frame_number: frame as u32,
                },
                &DrawParameters {
                    smooth: None,
                    blend: mesh.blend,
                    ..Default::default()
                },
            )?),
            GpuShader::Texture(ref texture) => Ok(surface.draw(
                mesh.vertices.as_ref(),
                mesh.indices.as_ref(),
                &lib.texture_shader,
                &uniform!{
                    scale: mesh.scale,
                    root_center: mesh.root_center,
                    center: mesh.center,
                    tex: texture
                      .sampled()
                      .minify_filter(MinifySamplerFilter::Linear)
                      .magnify_filter(MagnifySamplerFilter::Linear)
                },
                &Default::default(),
            )?),
            GpuShader::Voronoi(ref gpu_voronoi) => {
                let mut strengths: [f32; MAX_VORONOI_SITES] = [0.0; MAX_VORONOI_SITES];
                for i in 0..(gpu_voronoi.site_count as usize) {
                    strengths[i] = gpu_voronoi.strengths[i].tween(frame);
                }

        use glium::{Blend, BlendingFunction, LinearBlendingFactor};
                gpu_voronoi.strengths_buffer.write(&strengths);
                Ok(surface.draw(
                    mesh.vertices.as_ref(),
                    mesh.indices.as_ref(),
                    &lib.voronoi_shader,
                    &uniform!{
                        center: mesh.center,
                        root_center: mesh.root_center,
                        scale: mesh.scale,
                        frame_number: frame as u32,
                        Colors: &gpu_voronoi.colors,
                        Positions: &gpu_voronoi.positions,
                        Strengths: &gpu_voronoi.strengths_buffer,
                        site_count: gpu_voronoi.site_count,
                        frame: last.unwrap()
                            .sampled()
                            .minify_filter(MinifySamplerFilter::Linear)
                            .magnify_filter(MagnifySamplerFilter::Linear),
                    },
                    &DrawParameters {
                        smooth: None,
                        blend: mesh.blend,
                        ..Default::default()
                    },
                )?)
            }
            GpuShader::Custom(ref program) => Ok(surface.draw(
                mesh.vertices.as_ref(),
                mesh.indices.as_ref(),
                program,
                &uniform!{
                    center: mesh.center,
                    root_center: mesh.root_center,
                    scale: mesh.scale,
                    frame_number: frame as u32,
                    frame: last.unwrap()
                        .sampled()
                        .minify_filter(MinifySamplerFilter::Linear)
                        .magnify_filter(MagnifySamplerFilter::Linear),
                },
                &DrawParameters {
                    smooth: None,
                    blend: mesh.blend,
                    ..Default::default()
                },
            )?),
            GpuShader::NearFilter(ref cfg) => Ok(surface.draw(
                mesh.vertices.as_ref(),
                mesh.indices.as_ref(),
                &lib.near_filter,
                &uniform!{
                    center: mesh.center,
                    root_center: mesh.root_center,
                    scale: mesh.scale,
                    frame_number: frame as u32,
                    start: cfg.start.tween(frame),
                    step: cfg.step.tween(frame),
                    steps: cfg.steps.tween(frame) as i32,
                    sign: cfg.sign.tween(frame),
                    frame: last.unwrap()
                        .sampled()
                        .minify_filter(MinifySamplerFilter::Linear)
                        .magnify_filter(MagnifySamplerFilter::Linear),
                },
                &DrawParameters {
                    smooth: None,
                    blend: mesh.blend,
                    ..Default::default()
                },
            )?),
            GpuShader::Intermittent {
                ref src,
                ref predicate,
            } => if predicate(frame) {
                src.draw(lib, frame, surface, mesh, last)
            } else {
                Ok(())
            },
        }
    }
}

pub struct GpuVoronoi {
    positions: UniformBuffer<[[f32; 2]; MAX_VORONOI_SITES]>,
    strengths_buffer: UniformBuffer<[f32; MAX_VORONOI_SITES]>,
    strengths: Vec<Tween>,
    colors: UniformBuffer<[[f32; 4]; MAX_VORONOI_SITES]>,
    site_count: u32,
}

#[derive(Clone)]
pub struct VoronoiSite {
    pub strength: Tween,
    pub color: Colora,
    pub site: Point,
}

#[derive(Clone)]
pub struct NearFilterCfg {
    pub start: Tween,
    pub step: Tween,
    pub steps: Tween,
    pub sign: Tween,
}

#[derive(Clone)]
pub enum Shader {
    Default,
    Texture(Rc<ImageBuffer<Rgb<u8>, Vec<u8>>>),
    Voronoi(Vec<VoronoiSite>),
    Custom(Rc<Program>),
    Intermittent {
        src: Rc<Shader>,
        predicate: Rc<Fn(usize) -> bool>,
    },
    NearFilter(NearFilterCfg),    
}

impl From<ImageBuffer<Rgb<u8>, Vec<u8>>> for Shader {
    fn from(img: ImageBuffer<Rgb<u8>, Vec<u8>>) -> Self {
        Shader::Texture(Rc::new(img))
    }
}

impl From<Program> for Shader {
    fn from(program: Program) -> Self {
        Shader::Custom(Rc::new(program))
    }
}

impl Factory<Shader> for GpuShader {
    fn produce(spec: Shader, gpu: Rc<Gpu>) -> Result<Self> {
        match spec {
            Shader::Default => Ok(GpuShader::Default),
            Shader::Texture(ref spec_tex) => {
                let dims = spec_tex.dimensions();
                let img = spec_tex.as_ref().clone();
                let raw = RawImage2d::from_raw_rgb(img.into_raw(), dims);
                let tex = Rc::new(Texture2d::new(gpu.as_ref(), raw)?);
                Ok(GpuShader::Texture(tex))
            }
            Shader::Voronoi(sites) => {
                if sites.len() > MAX_VORONOI_SITES {
                    return Err(String::from("at most 1024 sites are supported").into());
                }

                let mut colors = [[0f32, 0.0, 0.0, 0.0]; MAX_VORONOI_SITES];
                let mut positions = [[0f32, 0.0]; MAX_VORONOI_SITES];
                let mut strengths = Vec::new();
                let site_count = sites.len() as u32;
                let (height, width) = gpu.display.get_framebuffer_dimensions();
                for (
                    i,
                    VoronoiSite {
                        strength,
                        color,
                        site,
                    },
                ) in sites.into_iter().enumerate()
                {
                    let cp = Colora {
                        alpha: 1.0,
                        ..color
                    }.into_premultiplied();

                    colors[i] = [cp.red, cp.green, cp.blue, color.alpha];
                    positions[i] = [site.x, site.y];
                    strengths.push(strength)
                }

                Ok(GpuShader::Voronoi(GpuVoronoi {
                    positions: UniformBuffer::new(gpu.as_ref(), positions)?,
                    colors: UniformBuffer::new(gpu.as_ref(), colors)?,
                    strengths_buffer: UniformBuffer::new(gpu.as_ref(), [0.0; MAX_VORONOI_SITES])?,
                    strengths,
                    site_count,
                }))
            }
            Shader::Custom(program) => Ok(GpuShader::Custom(program)),
            Shader::Intermittent { src, predicate } => Ok(GpuShader::Intermittent {
                src: Rc::new(GpuShader::produce(src.as_ref().clone(), gpu.clone())?),
                predicate,
            }),
            Shader::NearFilter(cfg) => Ok(GpuShader::NearFilter(cfg))
        }
    }
}
