use glium;
use lyon;
use std;

error_chain!{
    foreign_links {
        GlutinContext(glium::glutin::ContextError);
        SwapBuffer(glium::SwapBuffersError);
        ProgramChooser(glium::program::ProgramChooserCreationError);
        Draw(glium::DrawError);
        Creation(glium::glutin::CreationError);
        IndexCreation(glium::index::BufferCreationError);
        VertexCreation(glium::vertex::BufferCreationError);
        DisplayCreation(glium::backend::glutin::DisplayCreationError);
        TextureCreation(glium::texture::TextureCreationError);
        Io(std::io::Error);
    }
}

impl From<lyon::tessellation::FillError> for Error {
    fn from(_: lyon::tessellation::FillError) -> Error { "Fill error".into() }
}
