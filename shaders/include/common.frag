uniform sampler2D frame;

vec4 pixel(vec2 pos) {
	return texture(frame, pos);
}
