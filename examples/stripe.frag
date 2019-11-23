#version 440

uniform float width;

in vec4 v_color;

out vec4 frag;

void main() {
  frag = clamp(vec4(mod(v_color.xyz * gl_FragCoord.x / width, 0.1), v_color.z), 0., 1.);
}