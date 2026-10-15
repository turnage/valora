//! Uniform interface for shaders.

pub use valora_derive::UniformSet;

use palette::{LinSrgb, LinSrgba};

/// A set of values for a shader's uniforms.
///
/// Shaders receive the values as a struct bound at `@group(1) @binding(0)` in
/// the uniform address space. Derive this trait on a struct whose fields match
/// the WGSL struct's fields in order:
///
/// ```ignore
/// #[derive(UniformSet)]
/// struct Uniforms {
///     color: (f32, f32, f32),
///     scale: f32,
/// }
/// ```
///
/// ```wgsl
/// struct Uniforms {
///     color: vec3<f32>,
///     scale: f32,
/// }
/// @group(1) @binding(0) var<uniform> uniforms: Uniforms;
/// ```
pub trait UniformSet {
    fn write_uniforms(&self, writer: &mut UniformWriter);
}

impl UniformSet for () {
    fn write_uniforms(&self, _writer: &mut UniformWriter) {}
}

/// Lays out values according to WGSL's alignment rules for the uniform address
/// space.
#[derive(Debug, Default)]
pub struct UniformWriter {
    bytes: Vec<u8>,
}

impl UniformWriter {
    /// Writes the next field of the uniform struct.
    pub fn write<V: UniformValue + ?Sized>(&mut self, value: &V) {
        self.pad_to(V::ALIGN);
        value.write_bytes(&mut self.bytes);
    }

    /// The struct's bytes, padded to a multiple of 16 bytes.
    pub(crate) fn finish(mut self) -> Vec<u8> {
        self.pad_to(16);
        if self.bytes.is_empty() {
            self.bytes.resize(16, 0);
        }
        self.bytes
    }

    fn pad_to(&mut self, align: usize) {
        let len = self.bytes.len().next_multiple_of(align);
        self.bytes.resize(len, 0);
    }
}

pub(crate) fn uniform_bytes(uniforms: &impl UniformSet) -> Vec<u8> {
    let mut writer = UniformWriter::default();
    uniforms.write_uniforms(&mut writer);
    writer.finish()
}

/// A type which can be a field of a WGSL uniform struct.
pub trait UniformValue {
    /// The alignment of the WGSL type in bytes.
    const ALIGN: usize;

    /// Appends the value's bytes, including any padding within it.
    fn write_bytes(&self, bytes: &mut Vec<u8>);
}

macro_rules! scalar_uniform_value {
    ($scalar:ty) => {
        impl UniformValue for $scalar {
            const ALIGN: usize = 4;

            fn write_bytes(&self, bytes: &mut Vec<u8>) {
                bytes.extend(self.to_le_bytes());
            }
        }

        impl UniformValue for [$scalar; 2] {
            const ALIGN: usize = 8;

            fn write_bytes(&self, bytes: &mut Vec<u8>) {
                self.iter().for_each(|v| bytes.extend(v.to_le_bytes()));
            }
        }

        impl UniformValue for [$scalar; 3] {
            const ALIGN: usize = 16;

            fn write_bytes(&self, bytes: &mut Vec<u8>) {
                self.iter().for_each(|v| bytes.extend(v.to_le_bytes()));
            }
        }

        impl UniformValue for [$scalar; 4] {
            const ALIGN: usize = 16;

            fn write_bytes(&self, bytes: &mut Vec<u8>) {
                self.iter().for_each(|v| bytes.extend(v.to_le_bytes()));
            }
        }

        impl UniformValue for ($scalar, $scalar) {
            const ALIGN: usize = 8;

            fn write_bytes(&self, bytes: &mut Vec<u8>) {
                [self.0, self.1].write_bytes(bytes)
            }
        }

        impl UniformValue for ($scalar, $scalar, $scalar) {
            const ALIGN: usize = 16;

            fn write_bytes(&self, bytes: &mut Vec<u8>) {
                [self.0, self.1, self.2].write_bytes(bytes)
            }
        }

        impl UniformValue for ($scalar, $scalar, $scalar, $scalar) {
            const ALIGN: usize = 16;

            fn write_bytes(&self, bytes: &mut Vec<u8>) {
                [self.0, self.1, self.2, self.3].write_bytes(bytes)
            }
        }
    };
}

scalar_uniform_value!(f32);
scalar_uniform_value!(i32);
scalar_uniform_value!(u32);

/// `mat2x2<f32>`, as columns.
impl UniformValue for [[f32; 2]; 2] {
    const ALIGN: usize = 8;

    fn write_bytes(&self, bytes: &mut Vec<u8>) {
        self.iter().for_each(|column| column.write_bytes(bytes));
    }
}

/// `mat3x3<f32>`, as columns.
impl UniformValue for [[f32; 3]; 3] {
    const ALIGN: usize = 16;

    fn write_bytes(&self, bytes: &mut Vec<u8>) {
        for column in self {
            column.write_bytes(bytes);
            bytes.extend([0; 4]);
        }
    }
}

/// `mat4x4<f32>`, as columns.
impl UniformValue for [[f32; 4]; 4] {
    const ALIGN: usize = 16;

    fn write_bytes(&self, bytes: &mut Vec<u8>) {
        self.iter().for_each(|column| column.write_bytes(bytes));
    }
}

/// `vec3<f32>`.
impl UniformValue for LinSrgb {
    const ALIGN: usize = 16;

    fn write_bytes(&self, bytes: &mut Vec<u8>) {
        self.into_components().write_bytes(bytes)
    }
}

/// `vec4<f32>`.
impl UniformValue for LinSrgba {
    const ALIGN: usize = 16;

    fn write_bytes(&self, bytes: &mut Vec<u8>) {
        self.into_components().write_bytes(bytes)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn floats(bytes: &[u8]) -> Vec<f32> {
        bytes
            .chunks(4)
            .map(|c| f32::from_le_bytes([c[0], c[1], c[2], c[3]]))
            .collect()
    }

    #[test]
    fn scalars_pack_after_vec3() {
        let mut writer = UniformWriter::default();
        writer.write(&(1f32, 2f32, 3f32));
        writer.write(&4f32);
        writer.write(&5f32);
        assert_eq!(
            floats(&writer.finish()),
            vec![1., 2., 3., 4., 5., 0., 0., 0.]
        );
    }

    #[test]
    fn vectors_are_aligned() {
        let mut writer = UniformWriter::default();
        writer.write(&1f32);
        writer.write(&[2f32, 3.]);
        writer.write(&4f32);
        writer.write(&[5f32, 6., 7., 8.]);
        assert_eq!(
            floats(&writer.finish()),
            vec![1., 0., 2., 3., 4., 0., 0., 0., 5., 6., 7., 8.]
        );
    }

    #[test]
    fn mat3_columns_are_padded() {
        let mut writer = UniformWriter::default();
        writer.write(&[[1f32, 2., 3.], [4., 5., 6.], [7., 8., 9.]]);
        assert_eq!(
            floats(&writer.finish()),
            vec![1., 2., 3., 0., 4., 5., 6., 0., 7., 8., 9., 0.]
        );
    }

    #[test]
    fn empty_set_is_nonempty_buffer() {
        assert_eq!(uniform_bytes(&()), vec![0; 16]);
    }
}
