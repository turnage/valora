#version 400

out vec4 frag;

uniform sampler2DMS texture_in;

void main() {
  ivec2 texel = ivec2(floor(gl_FragCoord.x), floor(gl_FragCoord.y)); 
  frag = texelFetch(texture_in, texel, gl_SampleID);
}