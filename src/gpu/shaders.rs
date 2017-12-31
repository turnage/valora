use color::BlendMode;
use errors::Result;
use glium::Program;
use glium::texture::{RawImage2d, Texture2d};
use gpu::{Factory, Gpu, GpuMesh, Target};
use image::{ImageBuffer, Rgb};
use std::rc::Rc;

pub trait Shader {
    fn draw(&self, surface: &mut Target, mesh: GpuMesh) -> Result<()>;
}

pub struct DefaultShader {
    program: Rc<Program>,
}

impl Factory<()> for DefaultShader {
    fn produce(_: (), gpu: Rc<Gpu>) -> Result<Self> {
        Ok(Self { program: gpu.program(Gpu::PROGRAM_DEFAULT).unwrap() })
    }
}

impl Shader for DefaultShader {
    fn draw(&self, surface: &mut Target, mesh: GpuMesh) -> Result<()> {
        use glium::uniforms::EmptyUniforms;
        use glium::draw_parameters::{DrawParameters, Smooth};

        Ok(surface
               .draw((mesh.vertices.as_ref(), mesh.normals.as_ref()),
                     mesh.indices.as_ref(),
                     self.program.as_ref(),
                     &EmptyUniforms,
                     &DrawParameters {
                          smooth: Some(Smooth::Nicest),
                          blend: mesh.blend,
                          ..Default::default()
                      })?)
    }
}

#[derive(Clone)]
pub struct TextureShader {
    tex: Rc<Texture2d>,
    program: Rc<Program>,
}

#[derive(Clone)]
pub struct TextureShaderSpec {
    tex: Rc<ImageBuffer<Rgb<u8>, Vec<u8>>>,
}

impl Factory<TextureShaderSpec> for TextureShader {
    fn produce(spec: TextureShaderSpec, gpu: Rc<Gpu>) -> Result<Self> {
        let dims = spec.tex.dimensions();
        let img = spec.tex.as_ref().clone();
        let raw = RawImage2d::from_raw_rgb(img.into_raw(), dims);
        let tex = Rc::new(Texture2d::new(gpu.as_ref(), raw)?);
        Ok(TextureShader { tex, program: gpu.program(Gpu::PROGRAM_TEXTURE).unwrap() })
    }
}

impl Factory<Texture2d> for TextureShader {
    fn produce(tex: Texture2d, gpu: Rc<Gpu>) -> Result<Self> {
        Ok(TextureShader { tex: Rc::new(tex), program: gpu.program(Gpu::PROGRAM_TEXTURE).unwrap() })
    }
}

impl Shader for TextureShader {
    fn draw(&self, surface: &mut Target, mesh: GpuMesh) -> Result<()> {
        use glium::uniforms::{MagnifySamplerFilter, MinifySamplerFilter};
        Ok(surface
               .draw(mesh.vertices.as_ref(),
                     mesh.indices.as_ref(),
                     self.program.as_ref(),
                     &uniform!{
                        matrix: [
                            [1.0, 0.0, 0.0, 0.0],
                            [0.0, 1.0, 0.0, 0.0],
                            [0.0, 0.0, 1.0, 0.0],
                            [0.0 , 0.0, 0.0, 1.0f32],
                        ],
                        tex: self.tex
                           .sampled()
                           .minify_filter(MinifySamplerFilter::Linear)
                           .magnify_filter(MagnifySamplerFilter::Linear)
                     },
                     &Default::default())?)
    }
}