#version 150

in vec2 position;
out vec2 v_tex_coords;

uniform mat4 matrix;

void main() {
    v_tex_coords = (position + 1) / 2;
    gl_Position = matrix * vec4(position, 0.0, 1.0);
}