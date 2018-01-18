#version 150
#include "common.vert"

in vec2 position;
in vec4 color;

out vec2 pixel_position;
out vec4 v_Color;

void main() {
    v_Color = color;
    gl_Position = tweened_position(position);
    pixel_position = tex_position(gl_Position);
}
