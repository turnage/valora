use errors::Result;
use glium::texture::{RawImage2d, Texture2d};
use gpu::{Factory, Gpu, GpuMesh};
use image::{ImageBuffer, Rgb};
use std::rc::Rc;
use glium::draw_parameters::{DrawParameters, Smooth};
use gpu::programs::Library;
use glium::uniforms::{EmptyUniforms, MagnifySamplerFilter, MinifySamplerFilter};
use glium::{Frame, Surface};

pub enum GpuShader {
    Default,
    Texture(Rc<Texture2d>),
}

impl GpuShader {
    pub fn draw(&self, lib: &Library, surface: &mut Frame, mesh: &GpuMesh) -> Result<()> {
        match *self {
            GpuShader::Default => Ok(surface.draw(
                mesh.vertices.as_ref(),
                mesh.indices.as_ref(),
                &lib.default_shader,
                &EmptyUniforms,
                &DrawParameters {
                    smooth: Some(Smooth::Nicest),
                    blend: mesh.blend,
                    ..Default::default()
                },
            )?),
            GpuShader::Texture(ref texture) => Ok(surface.draw(
                mesh.vertices.as_ref(),
                mesh.indices.as_ref(),
                &lib.texture_shader,
                &uniform!{
                   matrix: [
                       [1.0, 0.0, 0.0, 0.0],
                       [0.0, 1.0, 0.0, 0.0],
                       [0.0, 0.0, 1.0, 0.0],
                       [0.0 , 0.0, 0.0, 1.0f32],
                   ],
                   tex: texture
                      .sampled()
                      .minify_filter(MinifySamplerFilter::Linear)
                      .magnify_filter(MagnifySamplerFilter::Linear)
                },
                &Default::default(),
            )?),
        }
    }
}

#[derive(Clone)]
pub enum Shader {
    Default,
    Texture(Rc<ImageBuffer<Rgb<u8>, Vec<u8>>>),
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
        }
    }
}
