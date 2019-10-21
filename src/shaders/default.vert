uniform float width;
uniform float height;

in vec2 position;
in vec4 color;

out vec4 v_color;

vec2 _project_coordinates() {
  return vec2(position.x / width * 2 - 1, position.y / height * 2 - 1);
}

void main() {
  gl_Position = vec4(_project_coordinates(), 0., 1.);
  v_color = color;
}