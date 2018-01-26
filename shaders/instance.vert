#version 150
#include "common.vert"

in float scale;
in vec2 center;
in vec2 root_center;
in float rotation;
in vec4 color;

in vec2 vertex_position;
in uint mesh_index;

out vec2 pixel_position;
out vec4 v_Color;

void main() {
    v_Color = color;
    gl_Position = tweened_position(vertex_position, center, root_center, rotation, scale);
    pixel_position = tex_position(gl_Position);
}
