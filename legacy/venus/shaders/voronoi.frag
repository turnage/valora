#version 150
#include "common.frag"

in vec4 v_Color;
in vec4 gl_FragCoord;
out vec4 Target0;

uniform Colors {
    vec4 colors[1024];
};

uniform Positions {
    vec2 positions[1024];
};

uniform Strengths {
    float strengths[1024];
};

uniform uint site_count;

void main() {
    vec4 closest_color = vec4(1.0, 1.0, 0.0, 1.0);
    float closest_distance = 10000.0;
    ivec2 idims = dimensions();
    vec2 dims = vec2(float(idims[0]), float(idims[1]));
    for (uint i = uint(0); i < site_count; i++) {
        vec2 fixed_position = vec2(positions[i][0] * dims[1], positions[i][1] * dims[1]);
        float distance = sqrt(pow(abs(fixed_position[0] - gl_FragCoord[0]) * strengths[i], 2) +
                              pow(abs(fixed_position[1] - gl_FragCoord[1]) * strengths[i], 2));
        if (distance < closest_distance) {
            closest_distance = distance;
            closest_color = colors[i];
        }
    }
    Target0 = closest_color;
}
