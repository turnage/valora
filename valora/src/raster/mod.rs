use errors::Result;
use pipeline::Vertex;

const WORLD_OFFSET: f32 = 1.0;
const WORLD_FACTOR: f32 = 2.0;

// OpenGL places the origin in the center of the screen. We rescale
// and offset vertices one world unit so the origin is in the bottom
// left, and y and x point up and right respectively. If you think
// it should be done differently, you are wrong.
pub fn fix_coord(coord: f32) -> f32 {
    (coord * WORLD_FACTOR) - WORLD_OFFSET
}

#[derive(Default)]
pub struct Tessellation {
    pub vertices: Vec<Vertex>,
    pub indices: Vec<u16>,
}

pub trait Tessellate {
    fn tessellate(self) -> Result<Tessellation>;
}
