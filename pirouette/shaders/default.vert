#version 450

layout(location=0) in vec2 position;
layout(location=1) in vec4 color;

layout(location=0) out vec4 vertex_color;

layout(set=1, binding=0)
uniform Scope {
  vec2 dimensions;
  vec2 offset;
  float scale;
};


vec2 _project_coordinates() {
  vec2 tmp = position;
  tmp.y = dimensions.y - tmp.y;
  return vec2(
    tmp.x / dimensions.x * 2. - 1.,
    tmp.y / dimensions.y * 2. - 1.
  );
}

void main() {
  gl_Position = vec4(_project_coordinates(), 0., 1.);
  vertex_color = color;
}
