use gfx;
use glutin::ContextError;
use lyon;
use std::fmt::{Debug, Display};

error_chain!{
    foreign_links {
        GlutinContext(ContextError);
    }
}

impl From<lyon::tessellation::FillError> for Error {
    fn from(_: lyon::tessellation::FillError) -> Error {
        "Fill error".into()
    }
}

impl<T: Display + Debug> From<gfx::PipelineStateError<T>> for Error {
    fn from(pe: gfx::PipelineStateError<T>) -> Error {
        format!("{}", pe).into()
    }
}
