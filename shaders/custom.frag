#version 150
//#include "common.frag"

uniform sampler2D frame;

vec4 pixel(vec2 pos) {
  return texture(frame, pos);
}

in vec2 pixel_position;

out vec4 color;

bool is_pixel(vec2 pos) {
return true;
//	return pos[0] < 1.0 && pos[0] >= 0.0 && pos[1] < 1.0 && pos[0] >= 0.0;
}

vec4 maybe_pixel(vec2 pos) {
	if (is_pixel(pos)) {
		return pixel(pos);
	} else {
		return vec4(-10, -10, -10, -10);
	}
}

struct Candidate {
	vec4 color;
	float distance;
};

Candidate consider(vec2 pos, Candidate candidate, vec4 us) {
	vec4 color = maybe_pixel(pos);
	if (length(us - color) < candidate.distance) {
		candidate.color = color;
		candidate.distance = length(color - us);
	}
	return candidate;
}

vec4 closest_neighbor(vec2 pos, float distance) {
	ivec2 dims = textureSize(frame, 0);
	vec2 unit = vec2(1.0 / float(dims[0]), 1.0 / float(dims[1])) * distance;
	vec4 us = pixel(pos);

	Candidate candidate;
	candidate.color = us;
	candidate.distance = 10000.0;

	candidate = consider(pos - unit, candidate, us);
	candidate = consider(pos + unit, candidate, us);

	candidate = consider(pos + vec2(unit[0], 0), candidate, us);
	candidate = consider(pos + vec2(0, unit[1]), candidate, us);

	candidate = consider(pos - vec2(unit[0], 0), candidate, us);
	candidate = consider(pos - vec2(0, unit[1]), candidate, us);

	candidate = consider(pos + vec2(-unit[0], unit[1]), candidate, us);
	candidate = consider(pos + vec2(unit[0], -unit[1]), candidate, us);

	return candidate.color;
}

vec4 closest_neighbor_n(vec2 pos, int start, int step, int n) {
	vec4 us = pixel(pos);
	Candidate candidate;
	candidate.color = us;
	candidate.distance = 10000.0;

	for (int i = 0; i < n; i++) {
		float distance = float(i * step + 1 + start);
		vec4 color = closest_neighbor(pos, distance);
		if (length(color - us) < candidate.distance) {
			candidate.color = color;
			candidate.distance = length(color - us);
		}
	}

	return candidate.color;
}

void main() {
	color = vec4(closest_neighbor_n(pixel_position, 40, 40, 2));
}
