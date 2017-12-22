mod glsl;

use self::glsl::GLSLShader;
use errors::Result;
use geom::Point;
use glium::Display;
use glium::program::Program;
use glium::texture::{RawImage2d, Texture2d};
use glium::uniforms::EmptyUniforms;
use image::{ImageBuffer, Rgb};
use palette::Colora;
use pipeline::DrawCmd;
use std::rc::Rc;

#[derive(Clone)]
pub enum Shader {
    Constant(Colora),
    Linear(Rc<Fn(Point) -> Colora>),
    Texture(ImageBuffer<Rgb<u8>, Vec<u8>>),
    Voronoi { sites: Vec<(Colora, Point)>, frame: u32 },
    Empty,
}

impl Shader {
    const MAX_VORONOI_SITES: usize = 1024;

    pub fn constant(color: Colora) -> Self { Shader::Constant(color) }

    pub fn linear<F: 'static + Fn(Point) -> Colora>(f: F) -> Self { Shader::Linear(Rc::new(f)) }

    pub fn empty() -> Self { Shader::Empty }

    pub fn texture(img: ImageBuffer<Rgb<u8>, Vec<u8>>) -> Self { Shader::Texture(img) }

    pub fn voronoi(sites: Vec<(Colora, Point)>, frame: u32) -> Self {
        Shader::Voronoi { sites, frame }
    }

    pub fn color_vertex(&self, point: Point) -> Colora {
        match *self {
            Shader::Constant(color) => color.clone(),
            Shader::Linear(ref f) => f(point),
            _ => Colora::rgb(0.0, 0.0, 0.0, 0.0),
        }
    }

    pub fn program(&self, display: &Display) -> Result<Program> {
        match *self { 
            Shader::Texture(_) => GLSLShader::texture().program(display),
            Shader::Voronoi { .. } => GLSLShader::voronoi().program(display),
            _ => GLSLShader::default().program(display),
        }
    }

    pub fn shade(&self, mut cmd: DrawCmd) -> Result<()> {
        use glium::uniforms::UniformBuffer;

        match *self {
            Shader::Texture(ref img) => {
                use glium::uniforms::MagnifySamplerFilter;
                use glium::uniforms::MinifySamplerFilter;

                let dims = img.dimensions();
                let raw = RawImage2d::from_raw_rgb(img.clone().into_raw(), dims);
                let tex = Texture2d::new(cmd.display, raw)?;
                let uniforms = uniform! {
                    matrix: [
                        [1.0, 0.0, 0.0, 0.0],
                        [0.0, 1.0, 0.0, 0.0],
                        [0.0, 0.0, 1.0, 0.0],
                        [0.0 , 0.0, 0.0, 1.0f32],
                    ],
                    tex: tex.sampled()
                        .magnify_filter(MagnifySamplerFilter::Linear)
                        .minify_filter(MinifySamplerFilter::Linear),
                };
                cmd.draw(&self.program(cmd.display)?, &uniforms)
            }
            Shader::Voronoi { ref sites, frame } => {
                use palette::Blend;

                let mut colors = [[0f32, 0.0, 0.0, 0.0]; Shader::MAX_VORONOI_SITES];
                let mut positions = [[0f32, 0.0]; Shader::MAX_VORONOI_SITES];
                for (i, &(c, p)) in sites.iter().enumerate() {
                    let cp = Colora { alpha: 1.0, ..c }.into_premultiplied();

                    colors[i] = [cp.red, cp.green, cp.blue, c.alpha];
                    positions[i] = [p.x * frame as f32, p.y * frame as f32];
                }

                let colors_buffer = UniformBuffer::new(cmd.display, colors)?;
                let positions_buffer = UniformBuffer::new(cmd.display, positions)?;
                cmd.draw(&self.program(cmd.display)?,
                         &uniform!(
                             Colors: &colors_buffer,
                             Positions: &positions_buffer,
                             site_count: sites.len() as u32,
                         ))
            }
            _ => cmd.draw(&self.program(cmd.display)?, &EmptyUniforms),
        }
    }
}
