use glium::{SwapBuffersError, glutin::{CreationError, ContextError},
            program::ProgramChooserCreationError, DrawError, index, vertex,
            backend::glutin::DisplayCreationError, texture::TextureCreationError,
            buffer::BufferCreationError, framebuffer::ValidationError};
use lyon::tessellation::FillError;
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
    fn from(_: FillError) -> Error { "Fill error".into() }
}
