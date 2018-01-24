use errors::Result;
use glium::{Display, Program};
use std::rc::Rc;

pub struct Library {
    pub default_shader: Rc<Program>,
    pub texture_shader: Rc<Program>,
    pub voronoi_shader: Rc<Program>,
    pub near_filter: Rc<Program>,
    pub blit_shader: Rc<Program>,
}

pub fn load_library(display: &Display) -> Result<Library> {
    Ok(Library {
        default_shader: Rc::new(Program::from_source(
            display,
            shader!("default.vert"),
            shader!("default.frag"),
            None,
        )?),
        texture_shader: Rc::new(Program::from_source(
            display,
            shader!("texture.vert"),
            shader!("texture.frag"),
            None,
        )?),
        voronoi_shader: Rc::new(Program::from_source(
            display,
            shader!("default.vert"),
            shader!("voronoi.frag"),
            None,
        )?),
        near_filter: Rc::new(Program::from_source(
            display,
            shader!("default.vert"),
            shader!("near_filter.frag"),
            None,
        )?),
        blit_shader: Rc::new(Program::from_source(
            display,
            shader!("blit.vert"),
            shader!("texture.frag"),
            None,
        )?),
    })
}
