#version 150

in vec4 v_Color;
in vec4 gl_FragCoord;
out vec4 Target0;

uniform Colors {
    vec4 colors[1024];
};

uniform Positions {
    vec2 positions[1024];
};

uniform uint site_count;

void main() {
    vec4 closest_color = vec4(1.0, 1.0, 0.0, 1.0);
    float closest_distance = 10000.0;
    bool debug = false;
    for (uint i = uint(0); i < site_count; i++) {
        float distance = sqrt(pow(abs(positions[i][0] - gl_FragCoord[0]), 2) +
                              pow(abs(positions[i][1] - gl_FragCoord[1]), 2));
        if (distance < 0.05) {
            debug = true;
        }
        if (distance < closest_distance) {
            closest_distance = distance;
            closest_color = colors[i];
        }
    }
    if (debug) {
        Target0 = vec4(1.0, 0.0, 0.0, 1.0);
    } else {
        Target0 = closest_color;
    }
}