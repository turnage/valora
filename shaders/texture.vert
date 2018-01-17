#version 150

in vec2 position;
out vec2 v_tex_coords;

uniform mat4 matrix;

uniform float scale;
uniform vec2 center;
uniform vec2 root_center;

void main() {
    vec2 center_offset = (position - root_center) * scale;
    gl_Position = vec4(center_offset + center, 0.0, 1.0);
    v_tex_coords = (position + 1) / 2;
}
