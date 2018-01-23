uniform sampler2D frame;
uniform uint frame_number;

vec4 pixel(vec2 pos) {
	return texture(frame, pos);
}

ivec2 dimensions() {
  return textureSize(frame, 0);
}

float signof(float x) {
  if (x < 0) {
    return -1;
  }

  return 1;
}
