#version 400

uniform vec3 color;

in vec4 v_color;

out vec4 frag;

void main() {
  frag = vec4(color, 1.);
}