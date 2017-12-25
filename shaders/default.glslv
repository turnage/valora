#version 150

in vec2 position;
in vec4 color;

out vec4 v_Color;

void main() {
    v_Color = color;
    gl_Position = vec4(position, 0.0, 1.0);
}