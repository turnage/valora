struct Uniforms {
    color: vec3<f32>,
}

@group(1) @binding(0) var<uniform> uniforms: Uniforms;

fn shade(fragment: Fragment) -> vec4<f32> {
    return vec4<f32>(uniforms.color, 1.);
}
