#version 150
#include "common.frag"
#define NO_DISTANCE 100000

in vec2 pixel_position;
out vec4 color;

uniform float start;
uniform float step;
uniform float sign;
uniform int steps;

bool is_pixel(vec2 pos) {
	return pos[0] < 1.0 && pos[0] >= 0.0 && pos[1] < 1.0 && pos[0] >= 0.0;
}

vec4 maybe_pixel(vec2 pos) {
	ivec2 dims = dimensions();
	return pixel(ivec2(pos) % dims);
}

struct Candidate {
	vec4 color;
	float distance;
};

bool candidate_passes(Candidate candidate, vec4 color, vec4 us) {
	if (candidate.distance == NO_DISTANCE) {
		return true;
	}
	float delta = candidate.distance - length(us - color);
	return delta * sign > 0;
}

Candidate consider(vec2 pos, Candidate candidate, vec4 us) {
	vec4 color = pixel(pos);
	if (candidate_passes(candidate, color, us)) {
		candidate.color = color;
		candidate.distance = length(color - us);
	}
	return candidate;
}

vec4 closest_neighbor(vec2 pos, float distance) {
	ivec2 dims = textureSize(last, 0);
	vec2 unit = vec2(1.0 / float(dims[0]), 1.0 / float(dims[1])) * distance;
	vec4 us = pixel(pos);

	Candidate candidate;
	candidate.color = us;
	candidate.distance = NO_DISTANCE;

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

vec4 closest_neighbor_n(vec2 pos, float start, float step, int n) {
	vec4 us = pixel(pos);
	Candidate candidate;
	candidate.color = us;
	candidate.distance = NO_DISTANCE;

	for (int i = 0; i < n; i++) {
		float distance = float(i + 1) * step + signof(step) * start;
		vec4 color = closest_neighbor(pos, distance);
		if (candidate_passes(candidate, color, us)) {
			candidate.color = color;
			candidate.distance = length(color - us);
		}
	}

	return candidate.color;
}

void main() {
    ivec2 dims = dimensions();
	color = vec4(closest_neighbor_n(pixel_position, float(dims[0]) / 1080.0 * start, float(dims[0]) / 1080.0 * step, steps));
}
