#version 150

in vec2 position;
in vec3 color;

out vec4 v_Color;

void main() {
    v_Color = vec4(color, 1.0);
    gl_Position = vec4(position, 0.0, 1.0);
}