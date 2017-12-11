use errors::Result;
use pipeline::Vertex;
use shaders::Shader;

#[derive(Default)]
pub struct Tessellation {
    pub vertices: Vec<Vertex>,
    pub indices: Vec<u16>,
}

pub trait Tessellate {
    fn tessellate(self, Shader) -> Result<Tessellation>;
}
