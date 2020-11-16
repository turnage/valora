#version 400

uniform float _valora_width;
uniform float _valora_height;
uniform float _valora_height_sign;

in vec2 vpos;
in vec4 vcol;

out vec4 v_color;

vec2 _project_coordinates() {
  vec2 tmp = vpos;
  tmp.y = _valora_height - tmp.y;
  return vec2(tmp.x / _valora_width * 2. - 1., _valora_height_sign * (tmp.y / _valora_height * 2. - 1.));
}

void main() {
  gl_Position = vec4(_project_coordinates(), 0., 1.);
  v_color = vcol;
}
