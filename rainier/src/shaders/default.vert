#version 440

uniform float width;
uniform float height;

in vec2 vpos;
in vec4 vcol;

out vec4 v_color;

vec2 _project_coordinates() {
  vec2 tmp = vpos;
  tmp.y = height - tmp.y;
  return vec2(tmp.x / width * 2. - 1., tmp.y / height * 2. - 1.);
}

void main() {
  gl_Position = vec4(_project_coordinates(), 0., 1.);
  v_color = vcol;
}