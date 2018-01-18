#version 150
#include "common.vert"

in vec2 position;
in vec4 color;

out vec4 v_Color;

void main() {
    v_Color = color;
    gl_Position = tweened_position(position);
}
