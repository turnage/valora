//! Uniform interface for shaders.

pub use valora_derive::UniformSet;

use glium::{
    texture::{texture2d::Texture2d, texture2d_multisample::Texture2dMultisample},
    uniforms::{SamplerBehavior, UniformValue},
};

/// A trait proxying `glium::Uniforms` for types which own their uniforms.
pub trait OwnedUniforms {
    fn visit_owned_values(&self, f: &mut dyn FnMut(&str, &dyn IntoUniformValue));
}

/// A trait for types which can represent themselves as `glium::UniformValue`.
pub trait IntoUniformValue {
    fn as_uniform_value(&self) -> UniformValue;
}

macro_rules! primitive_uniform_value {
    ($primitive:ty, $wrapper:expr) => {
        impl IntoUniformValue for $primitive {
            fn as_uniform_value<'a>(&'a self) -> UniformValue<'a> {
                $wrapper(*self)
            }
        }
    };
}

macro_rules! referenced_uniform_value {
    ($base:ty, $wrapper:expr) => {
        impl IntoUniformValue for $base {
            fn as_uniform_value<'a>(&'a self) -> UniformValue<'a> {
                $wrapper(self)
            }
        }
    };
}

primitive_uniform_value!(f32, UniformValue::Float);
primitive_uniform_value!((f32, f32), |v: (f32, f32)| UniformValue::Vec2([v.0, v.1]));
primitive_uniform_value!((f32, f32, f32), |v: (f32, f32, f32)| UniformValue::Vec3([
    v.0, v.1, v.2
]));
primitive_uniform_value!((f32, f32, f32, f32), |v: (f32, f32, f32, f32)| {
    UniformValue::Vec4([v.0, v.1, v.2, v.3])
});
primitive_uniform_value!([f32; 2], UniformValue::Vec2);
primitive_uniform_value!([f32; 3], UniformValue::Vec3);
primitive_uniform_value!([f32; 4], UniformValue::Vec4);
primitive_uniform_value!([[f32; 2]; 2], UniformValue::Mat2);
primitive_uniform_value!([[f32; 3]; 3], UniformValue::Mat3);
primitive_uniform_value!([[f32; 4]; 4], UniformValue::Mat4);

primitive_uniform_value!(f64, UniformValue::Double);
primitive_uniform_value!((f64, f64), |v: (f64, f64)| UniformValue::DoubleVec2([
    v.0, v.1
]));
primitive_uniform_value!((f64, f64, f64), |v: (f64, f64, f64)| {
    UniformValue::DoubleVec3([v.0, v.1, v.2])
});
primitive_uniform_value!((f64, f64, f64, f64), |v: (f64, f64, f64, f64)| {
    UniformValue::DoubleVec4([v.0, v.1, v.2, v.3])
});
primitive_uniform_value!([f64; 2], UniformValue::DoubleVec2);
primitive_uniform_value!([f64; 3], UniformValue::DoubleVec3);
primitive_uniform_value!([f64; 4], UniformValue::DoubleVec4);
primitive_uniform_value!([[f64; 2]; 2], UniformValue::DoubleMat2);
primitive_uniform_value!([[f64; 3]; 3], UniformValue::DoubleMat3);
primitive_uniform_value!([[f64; 4]; 4], UniformValue::DoubleMat4);

primitive_uniform_value!(i32, UniformValue::SignedInt);
primitive_uniform_value!((i32, i32), |v: (i32, i32)| UniformValue::IntVec2([
    v.0, v.1
]));
primitive_uniform_value!((i32, i32, i32), |v: (i32, i32, i32)| UniformValue::IntVec3(
    [v.0, v.1, v.2]
));
primitive_uniform_value!((i32, i32, i32, i32), |v: (i32, i32, i32, i32)| {
    UniformValue::IntVec4([v.0, v.1, v.2, v.3])
});
primitive_uniform_value!([i32; 2], UniformValue::IntVec2);
primitive_uniform_value!([i32; 3], UniformValue::IntVec3);
primitive_uniform_value!([i32; 4], UniformValue::IntVec4);

primitive_uniform_value!(i64, UniformValue::Int64);
primitive_uniform_value!((i64, i64), |v: (i64, i64)| UniformValue::Int64Vec2([
    v.0, v.1
]));
primitive_uniform_value!((i64, i64, i64), |v: (i64, i64, i64)| {
    UniformValue::Int64Vec3([v.0, v.1, v.2])
});
primitive_uniform_value!((i64, i64, i64, i64), |v: (i64, i64, i64, i64)| {
    UniformValue::Int64Vec4([v.0, v.1, v.2, v.3])
});
primitive_uniform_value!([i64; 2], UniformValue::Int64Vec2);
primitive_uniform_value!([i64; 3], UniformValue::Int64Vec3);
primitive_uniform_value!([i64; 4], UniformValue::Int64Vec4);

primitive_uniform_value!(u64, UniformValue::UnsignedInt64);
primitive_uniform_value!((u64, u64), |v: (u64, u64)| UniformValue::UnsignedInt64Vec2(
    [v.0, v.1]
));
primitive_uniform_value!((u64, u64, u64), |v: (u64, u64, u64)| {
    UniformValue::UnsignedInt64Vec3([v.0, v.1, v.2])
});
primitive_uniform_value!((u64, u64, u64, u64), |v: (u64, u64, u64, u64)| {
    UniformValue::UnsignedInt64Vec4([v.0, v.1, v.2, v.3])
});
primitive_uniform_value!([u64; 2], UniformValue::UnsignedInt64Vec2);
primitive_uniform_value!([u64; 3], UniformValue::UnsignedInt64Vec3);
primitive_uniform_value!([u64; 4], UniformValue::UnsignedInt64Vec4);

primitive_uniform_value!(u32, UniformValue::UnsignedInt);
primitive_uniform_value!((u32, u32), |v: (u32, u32)| UniformValue::UnsignedIntVec2([
    v.0, v.1
]));
primitive_uniform_value!((u32, u32, u32), |v: (u32, u32, u32)| {
    UniformValue::UnsignedIntVec3([v.0, v.1, v.2])
});
primitive_uniform_value!((u32, u32, u32, u32), |v: (u32, u32, u32, u32)| {
    UniformValue::UnsignedIntVec4([v.0, v.1, v.2, v.3])
});
primitive_uniform_value!([u32; 2], UniformValue::UnsignedIntVec2);
primitive_uniform_value!([u32; 3], UniformValue::UnsignedIntVec3);
primitive_uniform_value!([u32; 4], UniformValue::UnsignedIntVec4);

primitive_uniform_value!(bool, UniformValue::Bool);
primitive_uniform_value!((bool, bool), |v: (bool, bool)| UniformValue::BoolVec2([
    v.0, v.1
]));
primitive_uniform_value!((bool, bool, bool), |v: (bool, bool, bool)| {
    UniformValue::BoolVec3([v.0, v.1, v.2])
});
primitive_uniform_value!((bool, bool, bool, bool), |v: (bool, bool, bool, bool)| {
    UniformValue::BoolVec4([v.0, v.1, v.2, v.3])
});
primitive_uniform_value!([bool; 2], UniformValue::BoolVec2);
primitive_uniform_value!([bool; 3], UniformValue::BoolVec3);
primitive_uniform_value!([bool; 4], UniformValue::BoolVec4);

referenced_uniform_value!(Texture2d, |t| UniformValue::Texture2d(t, None));
referenced_uniform_value!(
    Texture2dMultisample,
    |t| UniformValue::Texture2dMultisample(t, Some(SamplerBehavior::default()))
);

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
            let value = value.as_uniform_value();
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
