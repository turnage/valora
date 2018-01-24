#version 150

uniform float scale;
uniform vec2 center;
uniform vec2 root_center;
uniform float rotation;

vec4 tweened_position(vec2 position) {
    vec2 offset = (position - root_center) * scale;
    float phase = atan(offset[1], offset[0]);
    float r = length(offset);
    vec2 rotated_offset = vec2(
        r * cos(rotation + phase),
        r * sin(rotation + phase));
    return vec4(rotated_offset + center, 0.0, 1.0);
}

vec2 tex_position(vec4 position) {
    return (vec2(position[0], position[1]) + 1.0) / 2.0;
}