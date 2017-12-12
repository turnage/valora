use errors::Result;
use pipeline::GpuVertex;
use shaders::Shader;

#[derive(Default)]
pub struct Tessellation {
    pub vertices: Vec<GpuVertex>,
    pub indices: Vec<u16>,
}

pub trait Tessellate {
    fn tessellate(self, Shader) -> Result<Tessellation>;
}
