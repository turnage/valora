#version 150

in vec2 position;
in vec4 color;

out vec4 v_Color;
out vec2 v_tex_coords;

void main() {
    v_Color = color;
    gl_Position = vec4(position, 0.0, 1.0);
    v_tex_coords = (vec2(gl_Position[0], gl_Position[1]) + 1) / 2;
}
