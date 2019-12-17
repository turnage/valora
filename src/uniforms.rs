//! Uniform interface for shaders.

pub use valora_derive::UniformSet;

use glium::{
    texture::texture2d::Texture2d,
    uniforms::{SamplerBehavior, UniformValue},
};

/// A trait proxying `glium::Uniforms` for types which own their uniforms.
pub trait OwnedUniforms {
    fn visit_owned_values(&self, f: &mut dyn FnMut(&str, &dyn IntoUniformValue));
}

/// A trait for types which can represent themselves as `glium::UniformValue`.
pub trait IntoUniformValue {
    fn into_uniform_value<'a>(&'a self) -> UniformValue<'a>;
}

macro_rules! primitive_uniform_value {
    ($primitive:ty, $wrapper:expr) => {
        impl IntoUniformValue for $primitive {
            fn into_uniform_value<'a>(&'a self) -> UniformValue<'a> { $wrapper(*self) }
        }
    };
}

primitive_uniform_value!(f32, |v| UniformValue::Float(v));
primitive_uniform_value!((f32, f32), |v: (f32, f32)| UniformValue::Vec2([v.0, v.1]));
primitive_uniform_value!((f32, f32, f32), |v: (f32, f32, f32)| UniformValue::Vec3([
    v.0, v.1, v.2
]));
primitive_uniform_value!((f32, f32, f32, f32), |v: (f32, f32, f32, f32)| {
    UniformValue::Vec4([v.0, v.1, v.2, v.3])
});
primitive_uniform_value!([f32; 2], |v| UniformValue::Vec2(v));
primitive_uniform_value!([f32; 3], |v| UniformValue::Vec3(v));
primitive_uniform_value!([f32; 4], |v| UniformValue::Vec4(v));
primitive_uniform_value!([[f32; 2]; 2], |v| UniformValue::Mat2(v));
primitive_uniform_value!([[f32; 3]; 3], |v| UniformValue::Mat3(v));
primitive_uniform_value!([[f32; 4]; 4], |v| UniformValue::Mat4(v));

primitive_uniform_value!(f64, |v| UniformValue::Double(v));
primitive_uniform_value!((f64, f64), |v: (f64, f64)| UniformValue::DoubleVec2([
    v.0, v.1
]));
primitive_uniform_value!((f64, f64, f64), |v: (f64, f64, f64)| {
    UniformValue::DoubleVec3([v.0, v.1, v.2])
});
primitive_uniform_value!((f64, f64, f64, f64), |v: (f64, f64, f64, f64)| {
    UniformValue::DoubleVec4([v.0, v.1, v.2, v.3])
});
primitive_uniform_value!([f64; 2], |v| UniformValue::DoubleVec2(v));
primitive_uniform_value!([f64; 3], |v| UniformValue::DoubleVec3(v));
primitive_uniform_value!([f64; 4], |v| UniformValue::DoubleVec4(v));
primitive_uniform_value!([[f64; 2]; 2], |v| UniformValue::DoubleMat2(v));
primitive_uniform_value!([[f64; 3]; 3], |v| UniformValue::DoubleMat3(v));
primitive_uniform_value!([[f64; 4]; 4], |v| UniformValue::DoubleMat4(v));

primitive_uniform_value!(i32, |v| UniformValue::SignedInt(v));
primitive_uniform_value!((i32, i32), |v: (i32, i32)| UniformValue::IntVec2([
    v.0, v.1
]));
primitive_uniform_value!((i32, i32, i32), |v: (i32, i32, i32)| UniformValue::IntVec3(
    [v.0, v.1, v.2]
));
primitive_uniform_value!((i32, i32, i32, i32), |v: (i32, i32, i32, i32)| {
    UniformValue::IntVec4([v.0, v.1, v.2, v.3])
});
primitive_uniform_value!([i32; 2], |v| UniformValue::IntVec2(v));
primitive_uniform_value!([i32; 3], |v| UniformValue::IntVec3(v));
primitive_uniform_value!([i32; 4], |v| UniformValue::IntVec4(v));

primitive_uniform_value!(i64, |v| UniformValue::Int64(v));
primitive_uniform_value!((i64, i64), |v: (i64, i64)| UniformValue::Int64Vec2([
    v.0, v.1
]));
primitive_uniform_value!((i64, i64, i64), |v: (i64, i64, i64)| {
    UniformValue::Int64Vec3([v.0, v.1, v.2])
});
primitive_uniform_value!((i64, i64, i64, i64), |v: (i64, i64, i64, i64)| {
    UniformValue::Int64Vec4([v.0, v.1, v.2, v.3])
});
primitive_uniform_value!([i64; 2], |v| UniformValue::Int64Vec2(v));
primitive_uniform_value!([i64; 3], |v| UniformValue::Int64Vec3(v));
primitive_uniform_value!([i64; 4], |v| UniformValue::Int64Vec4(v));

primitive_uniform_value!(u64, |v| UniformValue::UnsignedInt64(v));
primitive_uniform_value!((u64, u64), |v: (u64, u64)| UniformValue::UnsignedInt64Vec2(
    [v.0, v.1]
));
primitive_uniform_value!((u64, u64, u64), |v: (u64, u64, u64)| {
    UniformValue::UnsignedInt64Vec3([v.0, v.1, v.2])
});
primitive_uniform_value!((u64, u64, u64, u64), |v: (u64, u64, u64, u64)| {
    UniformValue::UnsignedInt64Vec4([v.0, v.1, v.2, v.3])
});
primitive_uniform_value!([u64; 2], |v| UniformValue::UnsignedInt64Vec2(v));
primitive_uniform_value!([u64; 3], |v| UniformValue::UnsignedInt64Vec3(v));
primitive_uniform_value!([u64; 4], |v| UniformValue::UnsignedInt64Vec4(v));

primitive_uniform_value!(u32, |v| UniformValue::UnsignedInt(v));
primitive_uniform_value!((u32, u32), |v: (u32, u32)| UniformValue::UnsignedIntVec2([
    v.0, v.1
]));
primitive_uniform_value!((u32, u32, u32), |v: (u32, u32, u32)| {
    UniformValue::UnsignedIntVec3([v.0, v.1, v.2])
});
primitive_uniform_value!((u32, u32, u32, u32), |v: (u32, u32, u32, u32)| {
    UniformValue::UnsignedIntVec4([v.0, v.1, v.2, v.3])
});
primitive_uniform_value!([u32; 2], |v| UniformValue::UnsignedIntVec2(v));
primitive_uniform_value!([u32; 3], |v| UniformValue::UnsignedIntVec3(v));
primitive_uniform_value!([u32; 4], |v| UniformValue::UnsignedIntVec4(v));

primitive_uniform_value!(bool, |v| UniformValue::Bool(v));
primitive_uniform_value!((bool, bool), |v: (bool, bool)| UniformValue::BoolVec2([
    v.0, v.1
]));
primitive_uniform_value!((bool, bool, bool), |v: (bool, bool, bool)| {
    UniformValue::BoolVec3([v.0, v.1, v.2])
});
primitive_uniform_value!((bool, bool, bool, bool), |v: (bool, bool, bool, bool)| {
    UniformValue::BoolVec4([v.0, v.1, v.2, v.3])
});
primitive_uniform_value!([bool; 2], |v| UniformValue::BoolVec2(v));
primitive_uniform_value!([bool; 3], |v| UniformValue::BoolVec3(v));
primitive_uniform_value!([bool; 4], |v| UniformValue::BoolVec4(v));

/// A texture that can be accessed from a shader.
pub struct UniformTexture {
    /// The texture.
    pub texture: Texture2d,
    /// The behavior of texture sampling.
    pub sampler_behavior: Option<SamplerBehavior>,
}

impl IntoUniformValue for UniformTexture {
    fn into_uniform_value<'a>(&'a self) -> UniformValue<'a> {
        UniformValue::Texture2d(&self.texture, self.sampler_behavior)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use valora_derive::UniformSet;

    #[derive(UniformSet)]
    pub struct MyUniforms {
        camera: [f32; 3],
        flipped: bool,
    }

    #[test]
    fn test() {
        let uniforms = MyUniforms {
            camera: [0., 0., 0.],
            flipped: true,
        };

        let mut i = 0;
        OwnedUniforms::visit_owned_values(&uniforms, &mut |name: &str, value| {
            let value = value.into_uniform_value();
            match i {
                0 => match (name, value) {
                    ("camera", UniformValue::Vec3(pos)) if pos == [0., 0., 0.] => {}
                    _ => panic!("Wrong camera values"),
                },
                1 => match (name, value) {
                    ("flipped", UniformValue::Bool(true)) => {}
                    _ => panic!("Wrong flipped values"),
                },
                _ => panic!("unexpected uniform: {:?}", name),
            }
            i += 1;
        });
    }
}
