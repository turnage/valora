#version 150

in vec2 vertex_position;

out vec4 v_Color;
out vec2 v_tex_coords;

void main() {
    vec2 fixed_position = vertex_position * 2.0 - 1.0;
    v_Color = vec4(1, 0, 0, 1);
    gl_Position = vec4(fixed_position, 0.0, 1.0);
    v_tex_coords = vertex_position;
}
