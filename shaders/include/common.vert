#version 150

in float scale;
in vec2 center;
in vec2 root_center;
in float rotation;
in uint applied;

vec4 tweened_position(vec2 position) {
    vec2 offset = (position - root_center) * scale;
    float phase = atan(offset[1], offset[0]);
    float r = length(offset);
    vec2 rotated_offset = vec2(
        r * cos(rotation + phase),
        r * sin(rotation + phase));
    vec2 positions[2];
    positions[0] = rotated_offset + center;
    positions[1] = position;
    return vec4(positions[applied], 0.0, 1.0);
}

vec2 tex_position(vec4 position) {
    return (vec2(position[0], position[1]) + 1.0) / 2.0;
}