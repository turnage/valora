#version 440

uniform float red;

in vec4 v_color;

out vec4 frag;

void main() {
  frag = vec4(red, v_color.yzw);
}