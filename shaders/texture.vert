#version 150
#include "common.vert"

in vec2 position;
out vec2 v_tex_coords;

void main() {
    gl_Position = tweened_position(position);
    v_tex_coords = (vec2(gl_Position[0], gl_Position[1]) + 1) / 2;
}
