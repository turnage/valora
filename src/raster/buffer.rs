//! Intermediate and output buffers for raster graphics.

use image::{ImageBuffer, Rgba};
use rayon::prelude::*;

pub type Buffer = ImageBuffer<Rgba<f64>, Vec<f64>>;

pub type FinalBuffer = ImageBuffer<Rgba<u8>, Vec<u8>>;

pub fn finalize_buffer(buffer: Buffer) -> FinalBuffer {
    let (width, height) = buffer.dimensions();
    FinalBuffer::from_vec(
        width,
        height,
        buffer
            .into_vec()
            .into_par_iter()
            .map(|v| (v * 255.0) as u8)
            .collect(),
    )
    .expect("final buffer")
}
