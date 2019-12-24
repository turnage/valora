#version 400

in vec4 v_color;

out vec4 frag;

void main() {
  frag = clamp(v_color, 0., 1.);
}