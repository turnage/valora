use errors::Result;
use glium::Display;
use glium::program::Program;

const VORONOI_VERTEX_SHADER: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
                                                         "/shaders/default.glslv"));

const VORONOI_FRAGMENT_SHADER: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
                                                           "/shaders/voronoi.frag"));

pub struct GLSLShader {
    vertex_shader: &'static str,
    fragment_shader: &'static str,
}

impl GLSLShader {
    pub fn voronoi() -> Self {
        Self { vertex_shader: VORONOI_VERTEX_SHADER, fragment_shader: VORONOI_FRAGMENT_SHADER }
    }

    pub fn texture() -> Self {
        Self {
            vertex_shader: include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
                                                "/shaders/texture.glslv")),
            fragment_shader: include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
                                                  "/shaders/texture.glslf")),
        }
    }

    pub fn program(&self, display: &Display) -> Result<Program> {
        program!(display,
            150 => {
                vertex: self.vertex_shader,
                fragment: self.fragment_shader,
            }
        )
                .map_err(Into::into)
    }
}

impl Default for GLSLShader {
    fn default() -> Self {
        Self {
            vertex_shader: include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
                                                "/shaders/default.glslv")),
            fragment_shader: include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
                                                  "/shaders/default.glslf")),
        }
    }
}