// Copies the linear float canvas into an sRGB encoded texture.

@group(0) @binding(0) var canvas: texture_2d<f32>;

@vertex
fn vertex(@builtin(vertex_index) index: u32) -> @builtin(position) vec4<f32> {
    let corner = vec2<f32>(f32((index << 1u) & 2u), f32(index & 2u));
    return vec4<f32>(corner * 2. - 1., 0., 1.);
}

@fragment
fn fragment(@builtin(position) position: vec4<f32>) -> @location(0) vec4<f32> {
    return textureLoad(canvas, vec2<i32>(position.xy), 0);
}
