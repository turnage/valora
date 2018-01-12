use glium::{index, vertex, DrawError, SwapBuffersError, backend::glutin::DisplayCreationError,
            buffer::BufferCreationError, framebuffer::ValidationError,
            glutin::{ContextError, CreationError}, program::ProgramChooserCreationError,
            texture::TextureCreationError};
use lyon::tessellation::FillError;
use num::traits::ParseFloatError;
use std::io;

error_chain!{
    foreign_links {
        FrameValidation(ValidationError);
        GlutinContext(ContextError);
        SwapBuffer(SwapBuffersError);
        ProgramChooser(ProgramChooserCreationError);
        Draw(DrawError);
        Creation(CreationError);
        IndexCreation(index::BufferCreationError);
        VertexCreation(vertex::BufferCreationError);
        DisplayCreation(DisplayCreationError);
        TextureCreation(TextureCreationError);
        Io(io::Error);
        BufferCreation(BufferCreationError);
    }
}

impl From<FillError> for Error {
    fn from(_: FillError) -> Error {
        "Fill error".into()
    }
}

impl From<ParseFloatError> for Error {
    fn from(e: ParseFloatError) -> Error {
        format!("parse float error: {:?}", e).into()
    }
}
