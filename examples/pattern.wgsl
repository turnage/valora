struct Uniforms {
    color: vec3<f32>,
    scale: f32,
    width: f32,
    height: f32,
}

@group(1) @binding(0) var<uniform> uniforms: Uniforms;

fn shade(fragment: Fragment) -> vec4<f32> {
    let pos = world_position(fragment);
    let white = vec3<f32>(1., 1., 1.);
    return vec4<f32>(0., mix(white, uniforms.color, pos.x / uniforms.width).xy, 1.);
}
