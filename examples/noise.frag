#version 440

uniform vec2 iResolution;
uniform float time;

#define PI 3.14
#define RGB(r, g, b, a) vec4(vec3(float(r)/255., float(g)/255., float(b)/255.), a)
#define NO_DISTANCE 10000.

// NOISE IMPL FROM: https://gist.github.com/patriciogonzalezvivo/670c22f3966e662d2f83#perlin-noise

float rand(vec2 c){
	return fract(sin(dot(c.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

float noise(vec2 p, float freq ){
	float unit = iResolution.x/freq;
	vec2 ij = floor(p/unit);
	vec2 xy = 0.5*(1.-cos(PI*mod(p,unit)/unit));
	float a = rand((ij+vec2(0.,0.)));
	float b = rand((ij+vec2(1.,0.)));
	float c = rand((ij+vec2(0.,1.)));
	float d = rand((ij+vec2(1.,1.)));
	float x1 = mix(a, b, xy.x);
	float x2 = mix(c, d, xy.x);
	return mix(x1, x2, xy.y);
}

float pNoise(vec2 p, int res, float scale, float lacunarity){
	float persistance = .5;
	float n = 0.;
	float normK = 0.;
	float f = scale;
	float amp = 1.;
	int iCount = 0;
	for (int i = 0; i<50; i++){
		n+=amp*noise(p + time, f);
		f*=lacunarity;
		normK+=amp;
		amp*=persistance;
		if (iCount == res) break;
		iCount++;
	}
	float nf = n/normK;
	return nf*nf*nf*nf*3.0;
}

float noiseTextureScalar(vec2 p, float distortion, float scale, int detail, float lower) {
    float distortionTheta = pNoise(p, detail, scale, 2.) * 2. * PI;
    vec2 distortionOffset = distortion*vec2(cos(distortionTheta), sin(distortionTheta));
    return abs(pNoise(p + distortionOffset, detail, scale, 2.));
}

vec4 noiseTexture(vec2 p, float distortion, float scale, int detail, float lower) {
    return vec4(
        noiseTextureScalar(p+10000., distortion, scale, detail, lower),
        noiseTextureScalar(p+20000., distortion, scale, detail, lower),
        noiseTextureScalar(p, distortion, scale, detail, lower),
        1.0
    );
}

// 4th param should be point on ramp
#define RAMP_STEPS 6
vec3 colorRamp(float p, vec4 steps[RAMP_STEPS]) {
    vec3 color = mix(steps[0].xyz, steps[1].xyz, smoothstep(steps[0].w, steps[1].w, p));
    color = mix(color, steps[2].xyz, smoothstep(steps[1].w, steps[2].w, p));
    color = mix(color, steps[3].xyz, smoothstep(steps[2].w, steps[3].w, p));
    color = mix(color, steps[4].xyz, smoothstep(steps[3].w, steps[4].w, p));
    return color;
}

vec4 christmasNoise(vec2 p) {
    vec4 c1 = RGB(23, 39, 44, 1.0);
    vec4 c2 = RGB(27, 85, 82, 1.0);
    vec4 c3 = RGB(111, 177, 128, 1.0);
    vec4 c4 = RGB(231, 204, 129, 1.0);
    vec4 c5 = RGB(228, 98, 65, 1.0);

    vec4 rampColors[RAMP_STEPS];
    
    rampColors[0] = RGB(140, 70, 12, 0.1);
    rampColors[1] = RGB(100, 0, 0, 0.0);
    rampColors[2] = RGB(198, 73, 69, 0.2);
    rampColors[3] = RGB(231, 204, 129, 0.7);
    rampColors[4] = RGB(180, 60, 65, 1.0);
    rampColors[5] = RGB(0, 0, 0, 2.0);
  

    vec4 n1 = noiseTexture(p, 0., 0.1 + 0.0001*time, 16, 0.0);
    
    vec2 samplePoint = n1.xy*iResolution.xy;
    
    vec4 n2 = noiseTexture(samplePoint, 10., 8., 16, 0.0);
    
    vec4 n3 = noiseTexture(n2.xy*iResolution.xy, 3., 4., 16, 0.0);

    return vec4(colorRamp(n3.x, rampColors), 1.0);
}

vec4 layer1(vec2 pos) {
    return christmasNoise(pos*iResolution.xy);
}

vec2 domainWarp(vec2 fragCoord) {
    float scale = abs(pNoise(fragCoord, 4, 4., 2.))*1000.;
    vec2 off = vec2(pNoise(fragCoord, 3, 4., 2.) * scale, pNoise(-1.0 * fragCoord, 3, 4., 2.)*scale);
    return fragCoord + off;
}

out vec4 frag;

void main() {
  frag = layer1(domainWarp(gl_FragCoord.xy/iResolution.xy));
}