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

void main2() {
  gl_Position = vec4(position, 0., 1.);//_project_coordinates(), 0., 1.);
  vertex_color = color;
}

const vec2 positions[3] = vec2[3](
    vec2(0.0, 0.5),
    vec2(-0.5, -0.5),
    vec2(0.5, -0.5)
);

void main() {
    gl_Position = vec4(positions[gl_VertexIndex], 0.0, 1.0);
}
