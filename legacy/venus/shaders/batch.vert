#version 150
#include "common.vert"

layout(std140) uniform;

struct Transform {
    float scale;
    float rotation;
    vec2 center;
    vec2 root_center;
    vec4 color;
};

uniform Transforms {
    Transform transforms[1024];
};

in vec2 vertex_position;
in uint mesh_index;

out vec2 pixel_position;
out vec4 v_Color;

void main() {
    Transform tx = transforms[mesh_index];
    v_Color = tx.color;
    gl_Position = tweened_position(vertex_position, tx.center, tx.root_center, tx.rotation, tx.scale);
    pixel_position = tex_position(gl_Position);
}
