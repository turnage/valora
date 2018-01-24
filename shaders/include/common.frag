uniform sampler2D last;
uniform uint frame;

vec4 pixel(vec2 pos) {
	return texture(last, pos);
}

ivec2 dimensions() {
  return textureSize(last, 0);
}

float signof(float x) {
  if (x < 0) {
    return -1;
  }

  return 1;
}
