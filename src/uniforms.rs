//! Uniform interface for shaders.

use glium::uniforms::UniformValue;

pub trait IntoUniformValue {
    fn into_uniform_value<'a>(&'a self) -> UniformValue<'a>;
}

impl IntoUniformValue for [f32; 3] {
    fn into_uniform_value<'a>(&'a self) -> UniformValue<'a> { UniformValue::Vec3(*self) }
}

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
