//! Uniform interface for shaders.

use glium::uniforms::UniformValue;

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
primitive_uniform_value!([f32; 2], |v| UniformValue::Vec2(v));
primitive_uniform_value!([f32; 3], |v| UniformValue::Vec3(v));
primitive_uniform_value!([f32; 4], |v| UniformValue::Vec4(v));
primitive_uniform_value!([[f32; 2]; 2], |v| UniformValue::Mat2(v));
primitive_uniform_value!([[f32; 3]; 3], |v| UniformValue::Mat3(v));
primitive_uniform_value!([[f32; 4]; 4], |v| UniformValue::Mat4(v));

primitive_uniform_value!(f64, |v| UniformValue::Double(v));
primitive_uniform_value!([f64; 2], |v| UniformValue::DoubleVec2(v));
primitive_uniform_value!([f64; 3], |v| UniformValue::DoubleVec3(v));
primitive_uniform_value!([f64; 4], |v| UniformValue::DoubleVec4(v));
primitive_uniform_value!([[f64; 2]; 2], |v| UniformValue::DoubleMat2(v));
primitive_uniform_value!([[f64; 3]; 3], |v| UniformValue::DoubleMat3(v));
primitive_uniform_value!([[f64; 4]; 4], |v| UniformValue::DoubleMat4(v));

primitive_uniform_value!(i32, |v| UniformValue::SignedInt(v));
primitive_uniform_value!([i32; 2], |v| UniformValue::IntVec2(v));
primitive_uniform_value!([i32; 3], |v| UniformValue::IntVec3(v));
primitive_uniform_value!([i32; 4], |v| UniformValue::IntVec4(v));

primitive_uniform_value!(i64, |v| UniformValue::Int64(v));
primitive_uniform_value!([i64; 2], |v| UniformValue::Int64Vec2(v));
primitive_uniform_value!([i64; 3], |v| UniformValue::Int64Vec3(v));
primitive_uniform_value!([i64; 4], |v| UniformValue::Int64Vec4(v));

primitive_uniform_value!(u64, |v| UniformValue::UnsignedInt64(v));
primitive_uniform_value!([u64; 2], |v| UniformValue::UnsignedInt64Vec2(v));
primitive_uniform_value!([u64; 3], |v| UniformValue::UnsignedInt64Vec3(v));
primitive_uniform_value!([u64; 4], |v| UniformValue::UnsignedInt64Vec4(v));

primitive_uniform_value!(u32, |v| UniformValue::UnsignedInt(v));
primitive_uniform_value!([u32; 2], |v| UniformValue::UnsignedIntVec2(v));
primitive_uniform_value!([u32; 3], |v| UniformValue::UnsignedIntVec3(v));
primitive_uniform_value!([u32; 4], |v| UniformValue::UnsignedIntVec4(v));

primitive_uniform_value!(bool, |v| UniformValue::Bool(v));
primitive_uniform_value!([bool; 2], |v| UniformValue::BoolVec2(v));
primitive_uniform_value!([bool; 3], |v| UniformValue::BoolVec3(v));
primitive_uniform_value!([bool; 4], |v| UniformValue::BoolVec4(v));

#[cfg(test)]
mod test {
    use super::*;
    use glium::uniforms::Uniforms;
    use uniform_derive::UniformSet;

    #[derive(UniformSet)]
    pub struct MyUniforms {
        camera: [f32; 3],
    }

    #[test]
    fn test() {
        let mut visited = std::collections::VecDeque::new();
        let uniforms = MyUniforms {
            camera: [0., 0., 0.],
        };

        Uniforms::visit_values(&uniforms, |name, value| {
            visited.push_back((name.to_string(), value))
        });

        let (camera, camera_value) = visited.pop_front().expect("A visited value");
        assert_eq!(camera, String::from("camera"));
    }
}
