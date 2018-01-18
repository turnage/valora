#version 150
#include "common.frag"

in vec2 pixel_position;

out vec4 color;

void main() {
    vec4 pixcol = pixel(pixel_position);
	color = vec4(0.0, pixcol[1], pixcol[2], 1.0);
}
