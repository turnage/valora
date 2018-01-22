use errors::Result;
use glium::{Display, Program};

pub struct Library {
    pub default_shader: Program,
    pub texture_shader: Program,
    pub voronoi_shader: Program,
    pub near_filter: Program,
}

pub fn load_library(display: &Display) -> Result<Library> {
    Ok(Library {
        default_shader: Program::from_source(
            display,
            shader!("default.vert"),
            shader!("default.frag"),
            None,
        )?,
        texture_shader: Program::from_source(
            display,
            shader!("texture.vert"),
            shader!("texture.frag"),
            None,
        )?,
        voronoi_shader: Program::from_source(
            display,
            shader!("default.vert"),
            shader!("voronoi.frag"),
            None,
        )?,
        near_filter: Program::from_source(
            display,
            shader!("default.vert"),
            shader!("near_filter.frag"),
            None,
        )?,
    })
}
