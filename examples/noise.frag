#version 440

uniform float width;
uniform float height;

in vec4 v_color;

out vec4 frag;
#define TWO_PI 6.28318530718
#define NO_DISTANCE 100000.
#define iTime 5.

vec3 random3(vec3 st)
{
    st = vec3( dot(st,vec3(127.1,311.7,211.2)/20.),
            	dot(st,vec3(269.5,183.3, 157.1)), dot(st,vec3(269.5,183.3, 17.1))  );
   	return -1.0 + 2.0*fract(sin(st)*43758.5453123);
}
float noise3D(vec3 st) 
{
	vec3 i = floor(st) ;
  	vec3 f = fract(st);
		
    vec3 u = smoothstep(0.,1.,f);
    
	float valueNowxy01 =mix( mix( dot( random3(i + vec3(0.0,0.0,0.0) ), f - vec3(0.0,0.0,0.0) ),
                    		 	 dot( random3(i + vec3(1.0,0.0,0.0) ), f - vec3(1.0,0.0,0.0) ), u.x),
                		mix( dot( random3(i + vec3(0.0,1.0,0.0) ), f - vec3(0.0,1.0,0.0) ),
                     		 	 dot( random3(i + vec3(1.0,1.0,0.0) ), f - vec3(1.0,1.0,0.0) ), u.x), u.y);
	float valueNowxy02 =mix( mix( dot( random3(i + vec3(0.0,0.0,1.0) ), f - vec3(0.0,0.0,1.0) ),
                    		 	 dot( random3(i + vec3(1.0,0.0,1.0) ), f - vec3(1.0,0.0,1.0) ), u.x),
                		mix( dot( random3(i + vec3(0.0,1.0,1.0) ), f - vec3(0.0,1.0,1.0) ),
                     		 	 dot( random3(i + vec3(1.0,1.0,1.0) ), f - vec3(1.0,1.0,1.0) ), u.x), u.y);

    float toReturn = abs(mix(valueNowxy01, valueNowxy02, u.z));
    return pow(.2, toReturn) -0.4;;

}
mat2 rotate2d(float _angle){
    return mat2(cos(_angle),-sin(_angle),
                sin(_angle),cos(_angle));
}


float fbm(vec2 st){
    
    int n = 5;
    float toReturn = 0.;
    float frequencyIncrease = 2.9;
    float amplitudeDecrese = 0.5;
    float amplitude = 1.;
    float frequency = 1.;
    
    for(int i = 0; i < n; i++){
        
        float det =  float(mod(float(i),2.)==0.);
        int signMul = (int(det)*2)-1;
        toReturn += amplitude*noise3D(vec3(st.xy *rotate2d(float(i/n))*frequency, iTime*.1*amplitude));
        
        amplitude *= amplitudeDecrese ;
        
        frequency *= frequencyIncrease;
    }
    
    return toReturn;
}


vec3 fbmN(vec2 st) {
    float q = 0.4;
    float order = 8.;
    vec2 sample_r = st;
    vec2 sample_g = st;
    vec2 sample_b = st;
    for (float i = 0.2; i < order; i += 1.) {
        vec2 sample_r_ = sample_r + vec2(fbm(vec2(i, 0.002)), fbm(vec2(i, 0.002))); 
        vec2 sample_g_ = sample_g + vec2(fbm(vec2(0.001, i)), fbm(vec2(0.003, i))); 
        vec2 sample_b_ = sample_b + vec2(fbm(vec2(i, -0.003)), fbm(vec2(i, 0.001))); 
        
        sample_r = sample_r * (1. - 1.) + q * sample_r_;
        sample_g = sample_g * (1. - 1.) + q * sample_g_;
        sample_b = sample_b * (1. - 1.) + q * sample_b_;
    }
    
    return vec3(fbm(sample_r), fbm(sample_g), fbm(sample_b));
}

vec4 layer1(vec2 uv) {
    vec2 toCenter = vec2(0.5) - uv ;
    float dis = length(toCenter);
    float angle = (acos(toCenter.x*1.0/dis)/TWO_PI)*2. ;
    //vec2 st = vec2(angle , dis );
    vec2 st = uv;
    // Time varying pixel color
    float r1 = fbm( st);
    float r2 = fbm( st*rotate2d(1.14) + vec2(1412., 124.)+r1);
	float colt = fbm( st*rotate2d(0.213) +vec2(14122., 14.)+r2);
    // Output to screen
    vec3 finColor = mix(vec3(r1, r2, colt), vec3(0.121, pow(min(r1,r2),2.), 0.2), colt);
    //finColor = fbmN(vec2(r1, r2));
    finColor = mix(finColor, 
                   vec3(dot(finColor, vec3(0.91,abs(sin(iTime*0.2)),0.2)), dot(finColor, vec3(colt)), colt*r1), r2); 
    return vec4(finColor, 1.);
}


vec4 layer2(vec2 uv) {
    return vec4(max(layer1(uv+vec2(1.5)).y, layer1(uv+vec2(1.5)).x),
                max(layer1(uv+vec2(2.5)).y, layer1(uv+vec2(2.5)).x),
                max(layer1(uv+vec2(3.5)).y, layer1(uv+vec2(3.5)).x),
                1);
}

struct Candidate {
	vec4 color;
	float distance;
};

bool candidate_passes(Candidate candidate, vec4 color, vec4 us, float dir) {
	if (candidate.distance == NO_DISTANCE) {
		return true;
	}
	float delta = candidate.distance - length(us - color);
	return delta * dir > 0.;
}

Candidate consider(vec2 pos, Candidate candidate, vec4 us, float dir) {
	vec4 color = layer2(pos);
	if (candidate_passes(candidate, color, us, dir)) {
		candidate.color = color;
		candidate.distance = length(color - us);
	}
	return candidate;
}

vec4 closest_neighbor(vec2 pos, float distance, float dir) {
	vec2 unit = vec2(1.0 / width, 1.0 / height) * distance;
	vec4 us = layer2(pos);

	Candidate candidate;
	candidate.color = us;
	candidate.distance = NO_DISTANCE;

	candidate = consider(pos - unit, candidate, us, dir);
	candidate = consider(pos + unit, candidate, us, dir);

	candidate = consider(pos + vec2(unit[0], 0), candidate, us, dir);
	candidate = consider(pos + vec2(0, unit[1]), candidate, us, dir);

	candidate = consider(pos - vec2(unit[0], 0), candidate, us, dir);
	candidate = consider(pos - vec2(0, unit[1]), candidate, us, dir);

	candidate = consider(pos + vec2(-unit[0], unit[1]), candidate, us, dir);
	candidate = consider(pos + vec2(unit[0], -unit[1]), candidate, us, dir);

	return candidate.color;
}

vec4 closest_neighbor_n(vec2 pos, float start, float step, int n, float dir) {
	vec4 us = layer1(pos);
	Candidate candidate;
	candidate.color = us;
	candidate.distance = NO_DISTANCE;

	for (int i = 0; i < n; i++) {
		float distance = float(i + 1) * step + sign(step) * start;
		vec4 color = closest_neighbor(pos, distance, dir);
		if (candidate_passes(candidate, color, us, dir)) {
			candidate.color = color;
			candidate.distance = length(color - us);
		}
	}

	return candidate.color;
}
vec4 layer3(vec2 uv) {
    float start = 0.05;
    float step = 0.0000005;
    int steps = 5;
    return closest_neighbor_n(uv, width * start, width * step, steps, -1.);
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    // Normalized pixel coordinates (from 0 to 1)
    vec2 uv = fragCoord/width;
    fragColor = layer3(uv);

}

void main() {
  mainImage(frag, gl_FragCoord.xy);
}