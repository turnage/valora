use errors::Result;
use glium::texture::{RawImage2d, Texture2d};
use gpu::{Factory, Gpu};
use image::{ImageBuffer, Rgb};
use std::rc::Rc;
use glium::uniforms::{MagnifySamplerFilter, MinifySamplerFilter, UniformBuffer, Uniforms, UniformValue, AsUniformValue, SamplerBehavior, SamplerWrapFunction};
use glium::{Program};
use tween::Tween;

pub const MAX_VORONOI_SITES: usize = 1024;


#[derive(Clone)]
pub struct GpuShader {
    pub program: Rc<Program>,
    pub uniforms: GpuUniforms,
}

#[derive(Clone)]
pub enum GpuUniforms {
    Default,
    Texture(Rc<Texture2d>),
    NearFilter(NearFilterCfg),
}

pub struct UniformFacade<'a, 'b> {
    pub shader_uniforms: &'a GpuUniforms,
    pub frame: u32,
    pub last: &'b Texture2d,
}

impl<'a, 'b> Uniforms for UniformFacade<'a, 'b> {
    fn visit_values<'c, F: FnMut(&str, UniformValue<'c>)>(&'c self, mut f: F) {
        f("last", UniformValue::Texture2d(self.last, Some(SamplerBehavior{
                    wrap_function: (SamplerWrapFunction::Mirror, SamplerWrapFunction::Mirror, SamplerWrapFunction::Mirror),
                    magnify_filter: MagnifySamplerFilter::Linear,
                    minify_filter: MinifySamplerFilter::LinearMipmapLinear,
                    max_anisotropy: 4,
                })));
        f("frame", self.frame.as_uniform_value());
        self.shader_uniforms.visit_values(f);
    }
}

impl Uniforms for GpuUniforms {
    fn visit_values<'a, F: FnMut(&str, UniformValue<'a>)>(&'a self, mut f: F) {
        match *self {
            GpuUniforms::Texture(ref tex) => {
                f("tex", UniformValue::Texture2d(tex.as_ref(), Some(SamplerBehavior{
                    wrap_function: (SamplerWrapFunction::Mirror, SamplerWrapFunction::Mirror, SamplerWrapFunction::Mirror),
                    magnify_filter: MagnifySamplerFilter::Linear,
                    minify_filter: MinifySamplerFilter::Linear,
                    max_anisotropy: 4,
                })));
            }
            _ => {}
        }
    }
}

#[derive(Clone, Debug)]
pub struct NearFilterCfg {
    pub start: Tween,
    pub step: Tween,
    pub steps: Tween,
    pub sign: Tween,
}

#[derive(Clone, Debug)]
pub enum Shader {
    Default,
    Texture(Rc<ImageBuffer<Rgb<u8>, Vec<u8>>>),
    NearFilter(NearFilterCfg),
}

impl From<ImageBuffer<Rgb<u8>, Vec<u8>>> for Shader {
    fn from(img: ImageBuffer<Rgb<u8>, Vec<u8>>) -> Self {
        Shader::Texture(Rc::new(img))
    }
}

impl Factory<Shader> for GpuShader {
    fn produce(spec: Shader, gpu: Rc<Gpu>) -> Result<Self> {
        match spec {
            Shader::Default => Ok(GpuShader {
                program: gpu.library.default_shader.clone(),
                uniforms: GpuUniforms::Default,
            }),
            Shader::Texture(ref spec_tex) => {
                let dims = spec_tex.dimensions();
                let img = spec_tex.as_ref().clone();
                let raw = RawImage2d::from_raw_rgb(img.into_raw(), dims);
                let tex = Rc::new(Texture2d::new(gpu.as_ref(), raw)?);
                Ok(GpuShader {
                    program: gpu.library.texture_shader.clone(),
                    uniforms: GpuUniforms::Texture(tex)
                })
            }
            Shader::NearFilter(cfg) => Ok(GpuShader{
                program: gpu.library.near_filter.clone(),
                uniforms: GpuUniforms::NearFilter(cfg)
            }),
        }
    }
}
