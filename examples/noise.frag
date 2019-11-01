#version 440

uniform float width;
uniform float height;

in vec4 v_color;

out vec4 frag;

float rand(vec2 p) {
    float sample_point = dot(p, vec2(129.83838, 83.838383));
    return fract(sin(sample_point) * 39399.43872382);
}

void main() {
  float r = rand(gl_FragCoord.xy);
  float g = rand(gl_FragCoord.yx);
  float b = rand(vec2(r, g));
  frag = vec4(r, g, b, v_color.w);
}