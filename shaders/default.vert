#version 150

in vec2 position;
in vec3 normal;
in vec4 color;

out vec4 v_Color;

uniform float scale;
uniform vec2 center;
uniform vec2 root_center;

void main() {
    v_Color = color;
    vec2 center_offset = (position - root_center) * scale;
    gl_Position = vec4(center_offset + center, 0.0, 1.0);
}
