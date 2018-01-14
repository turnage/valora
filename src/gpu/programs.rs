use errors::Result;
use glium::{Display, Program};

lazy_static! {
    static ref PROGRAM_SPEC_DEFAULT: ProgramSpec  = ProgramSpec {
        vertex: include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
                                          "/shaders/default.vert")),
        fragment: include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
                                            "/shaders/default.glslf")),
    };
    static ref PROGRAM_SPEC_TEXTURE: ProgramSpec  = ProgramSpec {
        vertex: include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
                                          "/shaders/texture.vert")),
        fragment: include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
                                            "/shaders/texture.glslf")),
    };
}

struct ProgramSpec {
    vertex: &'static str,
    fragment: &'static str,
}

pub struct Library {
    pub default_shader: Program,
    pub texture_shader: Program,
}

fn load_program(spec: &ProgramSpec, display: &Display) -> Result<Program> {
    let bind_result = program!(
        display,
        150 => {
            vertex: spec.vertex,
            fragment: spec.fragment,
        }
    );
    bind_result.map_err(Into::into)
}

pub fn load_library(display: &Display) -> Result<Library> {
    Ok(Library {
        default_shader: load_program(&PROGRAM_SPEC_DEFAULT, display)?,
        texture_shader: load_program(&PROGRAM_SPEC_DEFAULT, display)?,
    })
}
