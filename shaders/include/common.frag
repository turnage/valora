uniform sampler2D frame;
uniform uint frame_number;

vec4 pixel(vec2 pos) {
	return texture(frame, pos);
}

ivec2 dimensions() {
  return textureSize(frame, 0);
}
