//! Intermediate and output buffers for raster graphics.

use image::{ImageBuffer, Rgba};
use rayon::prelude::*;
use std::convert::TryFrom;

pub type Buffer = ImageBuffer<Rgba<f64>, Vec<f64>>;

pub type FinalBuffer = ImageBuffer<Rgba<u8>, Vec<u8>>;

#[derive(Clone, Debug)]
pub struct Surface {
    // surface data stored row-major where each pixel is 4 f64s in RGBA order.
    data: Vec<f64>,
    width: u32,
    height: u32,
}

impl Surface {
    pub fn with_dimensions(width: u32, height: u32) -> Self {
        Self {
            data: vec![0.0; (width * height * 4) as usize],
            width,
            height,
        }
    }

    pub fn pixel(&mut self, x: isize, y: isize) -> Option<&mut [f64]> {
        let i = self.normalize(x, y)?;
        let i = usize::try_from(i).ok()?;
        self.data.get_mut(i..(i + 4))
    }

    fn normalize(&self, x: isize, y: isize) -> Option<isize> {
        if x >= self.width as isize || x < 0 {
            return None;
        }

        Some(y * (self.width as isize) * 4 + x * 4)
    }
}

impl Into<FinalBuffer> for Surface {
    fn into(self) -> FinalBuffer {
        finalize_buffer(Buffer::from_vec(self.width as u32, self.height as u32, self.data).unwrap())
    }
}

fn finalize_buffer(buffer: Buffer) -> FinalBuffer {
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn data_allocation() {
        let surface = Surface::with_dimensions(2, 2);
        assert_eq!(
            surface.data,
            vec![0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        );
    }

    #[test]
    fn index_normalization() {
        let surface = Surface::with_dimensions(2, 3);
        assert_eq!(surface.normalize(1, 2), Some(20));
    }

    #[test]
    fn out_of_bounds_index_normalization() {
        let surface = Surface::with_dimensions(2, 3);
        assert_eq!(surface.normalize(2, 2), None);
    }

    #[test]
    fn coordinates() {
        let mut surface = Surface::with_dimensions(2, 3);
        surface
            .pixel(1, 2)
            .map(|p| p.iter_mut().for_each(|sp| *sp = 1.0));

        let expected_data = vec![
            0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
            0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0,
        ];

        assert_eq!(surface.data.len(), expected_data.len());

        assert_eq!(surface.data, expected_data);
    }
}
