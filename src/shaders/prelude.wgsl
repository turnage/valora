// Declarations valora appends to every shader. A shader provides
//
//     fn shade(fragment: Fragment) -> vec4<f32>
//
// which returns the linear RGBA color of a fragment. Valora multiplies the
// returned alpha by the fragment's coverage.

struct Valora {
    // The size of the output in pixels.
    size: vec2<f32>,
    // The scale of the output relative to the world.
    scale: f32,
    // Seconds since the composition began.
    time: f32,
}

@group(0) @binding(0) var<uniform> valora: Valora;

struct Fragment {
    // The fragment's position in output pixels, from the top left.
    @builtin(position) position: vec4<f32>,
    // The color the path was painted with.
    @location(0) @interpolate(flat) color: vec4<f32>,
    // The fraction of the pixel covered by the path.
    @location(1) @interpolate(flat) coverage: f32,
}

// The position of a fragment in world coordinates, from the bottom left.
fn world_position(fragment: Fragment) -> vec2<f32> {
    return vec2<f32>(fragment.position.x, valora.size.y - fragment.position.y) / valora.scale;
}

@vertex
fn valora_vertex(
    @builtin(vertex_index) index: u32,
    @location(0) rect: vec4<f32>,
    @location(1) color: vec4<f32>,
    @location(2) coverage: f32,
) -> Fragment {
    let corner = vec2<f32>(f32(index & 1u), f32(index >> 1u));
    let pixel = mix(rect.xy, rect.zw, corner);

    var fragment: Fragment;
    fragment.position = vec4<f32>(pixel / valora.size * vec2<f32>(2., -2.) + vec2<f32>(-1., 1.), 0., 1.);
    fragment.color = color;
    fragment.coverage = coverage;
    return fragment;
}

@fragment
fn valora_fragment(fragment: Fragment) -> @location(0) vec4<f32> {
    let color = shade(fragment);
    return vec4<f32>(color.rgb, color.a * fragment.coverage);
}
