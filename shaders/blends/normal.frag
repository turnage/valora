#version 150

in vec2 v_tex_coords;
out vec4 color;

uniform sampler2D bg;
uniform sampler2D fg;

void main() {
    vec4 bgc = texture(bg, v_tex_coords);
    vec4 fgc = texture(fg, v_tex_coords);
    color = vec4(
        fgc[0] * fgc[3] + bgc[0] * (1 - fgc[3]),
        fgc[1] * fgc[3] + bgc[1] * (1 - fgc[3]),
        fgc[2] * fgc[3] + bgc[2] * (1 - fgc[3]),
        fgc[3] + bgc[3] * (1 - fgc[3])
    );
}