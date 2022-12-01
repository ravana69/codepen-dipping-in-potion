/*********
 * made by Matthias Hurrle (@atzedent)
 */

/** @type {HTMLCanvasElement} */
const canvas = window.canvas
const gl = canvas.getContext("webgl2")
const dpr = Math.max(1, .5 * window.devicePixelRatio)
/** @type {Map<string,PointerEvent>} */
const touches = new Map()

const vertexSource = `#version 300 es
#ifdef GL_FRAGMENT_PRECISION_HIGH
precision highp float;
#else
precision mediump float;
#endif

in vec2 position;

void main(void) {
    gl_Position = vec4(position, 0., 1.);
}
`
const fragmentSource = `#version 300 es
/*********
* made by Matthias Hurrle (@atzedent)
*/

#ifdef GL_FRAGMENT_PRECISION_HIGH
precision highp float;
#else
precision mediump float;
#endif

out vec4 fragColor;

uniform vec2 resolution;
uniform float time;
uniform vec2 touch;
uniform int pointerCount;

const float TAU = radians(360.);

#define PI .5 * TAU
#define T time

#define SURF_DIST .01
#define MAX_DIST 50.
#define MAX_STEPS 50

struct Material {
	float dist;
	int idx;
};

float SmoothMin(float a, float b, float k) {
    float h = clamp(.5  + .5 * (b - a) / k, .0, 1.);
    
	return mix(b, a, h) - k * h * (1. - h);
}

Material MatSMin(Material a, Material b, float k) {
    Material mat = b;

    if (a.dist < b.dist) {
        mat = a;
    }

    return Material(SmoothMin(a.dist, b.dist, k), mat.idx);
}

// Material MatMin(Material a, Material b) {
// 	if (a.dist < b.dist) return a;

//     return b;
// }

mat2 Rot(float a) {
	float s = sin(a), c = cos(a);

	return mat2(c, -s, s, c);
}

mat3 RotX(float a) {
	float s = sin(a), c = cos(a);

	return
		mat3(
			vec3(1, 0, 0),
			vec3(0, c, -s),
			vec3(0, s, c)
		);
}

mat3 RotY(float a) {
	float s = sin(a), c = cos(a);

	return
		mat3(
			vec3(c, 0, s),
			vec3(0, 1, 0),
			vec3(-s, 0, c)
		);
}

float Circle(float d, float r, float w, float b) {
	return smoothstep(b, -b, abs(d - (r - w)) - w);
}

vec3 Pattern(vec2 uv) {
	float rhm = .5 + .5 * sin(T);
	float scale = smoothstep(.0, 1., rhm);

	uv *= 2.-rhm;
	uv = fract(uv * .125 +.5) -.5;

	float d = smoothstep(.0, .5, length(uv));
	vec3 color = vec3(.0, 1.-1.*d, 1.-1.*d);

	float k = 12. - 11. * smoothstep(.0, 1., .5+.5*cos(T));
    float mkd = pow(pow(abs(uv.x), k)+pow(abs(uv.y), k), 1./k);
	float sdf =
		Circle(
			sin(T - 10. * mkd),
			.5 * scale,
			.2 + .3 * scale,
			.05 + .195 * (1. - scale)
		);

	float fig = smoothstep(.0, 1., sdf);

	return max(color, vec3(fig));
}

float BoxFrame(vec3 p, vec3 b, float e) {
	p = abs(p)-b;
	vec3 q = abs(p+e)-e;
	const float r = .05;

	return min(min(
		length(
			max(vec3(p.x, q.y, q.z), 0.0))+
		min(max(p.x, max(q.y, q.z)), 0.0) - r,
		length(
			max(vec3(q.x, p.y, q.z), 0.0))+
		min(max(q.x, max(p.y, q.z)), 0.0) - r),
		length(
			max(vec3(q.x, q.y, p.z), 0.0))+
		min(max(q.x, max(q.y, p.z)), 0.0) - r);
}

Material GetDist(vec3 p) {
    vec3 pattern = Pattern(p.xz);
	return MatSMin(
		Material(
			BoxFrame(
				p*RotX(T)*RotY(radians(45.))-vec3(0,.5,0), vec3(1,.5,1), .125),
			1
		),
		Material(
			(p.y + 1.14) - (pattern.x + pattern.y + pattern.z) * .01, 0
		)
        , .25
	);
}

vec3 GetRayDir(vec2 uv, vec3 p, vec3 l, float z) {
	vec3
	f = normalize(l-p),
	r = normalize(cross(vec3(.0, 1., .0), f)),
	u = cross(f, r),
	c = f*z,
	i = c + uv.x*r + uv.y*u,
	d = normalize(i);

	return d;
}

vec3 GetNormal(vec3 p) {
	vec2 e = vec2(.001, .0);
	Material d = GetDist(p);
	vec3 n = d.dist - vec3(
		GetDist(p-e.xyy).dist,
		GetDist(p-e.yxy).dist,
		GetDist(p-e.yyx).dist
	);

	return normalize(n);
}

Material RayMarch(vec3 ro, vec3 rd) {
	float d = .0;
	Material mat;

	for (int i = 0; i < MAX_STEPS; i++) {
		vec3 p = ro + rd * d;
		mat = GetDist(p);
		d += mat.dist;

		if (d > MAX_DIST || abs(d) < SURF_DIST) break;
	}

	return Material(d, mat.idx);
}

vec3 Render(inout vec3 ro, inout vec3 rd, inout float ref) {
	Material d = RayMarch(ro, rd);
	vec3 col = vec3(0);

	if (d.dist > MAX_DIST) return col;

	vec3 p = ro + rd * d.dist;
	vec3 lightPos = ro;

	vec3 l = normalize(lightPos);
	vec3 n = GetNormal(p);
	vec3 r = reflect(rd, n);
	vec3 rn = normalize(r);

	float fres = clamp(1.+dot(r, n), .0, 1.);
	float diffuse = smoothstep(.05, .95, dot(l, n) * .5 + .5);
	float spot = clamp(dot(rn, reflect(n, vec3(0))), .0, 1.);

	col += .25 * vec3(1.,.9,.95) * pow(diffuse, 8.);
	col += .5 * pow(spot, 16.);

	vec3 mat = vec3(1);

	if (d.idx == 0) {
		float rhm = .5 + .5*sin(T);

		mat = mix(
			mat,
			Pattern(p.xz),
			fres
		);
		ref = mix(.05, .25, fres);
	} else if (d.idx == 1) {
		mat = mix(mat, vec3(1,0,0), exp(log(fres)));
		ref = mix(.005, .25, fres);
	}

	ro = p + n * SURF_DIST * 3.;
	rd = r;

	return col * mat;
}

void main(void) {
	vec2 uv = (
		gl_FragCoord.xy - .5 * resolution.xy
	) / min(resolution.x, resolution.y);

	vec2 m = touch.xy / resolution.xy;
	m = m.x > .0
		? m
		: vec2(.625, .35);
	m.y *= .5;
	m.y = clamp(m.y, .0, .4);

	vec3 ro = vec3(0, 1, -6);

	bool aut = pointerCount == 0;;
	if (aut) {
		float t = T * .5;
		ro.xz += vec2(cos(T), sin(T));
		ro.xy += vec2(cos(t), sin(t)) * 4. + 2.;
	} else {
		ro.yz *= Rot(-m.y * PI + 1.);
		ro.xz *= Rot(-m.x * TAU);
	}

	vec3 rd = GetRayDir(uv, ro, vec3(.0), 1.);

	float ref;
	vec3 col = Render(ro, rd, ref);

	for (int i = 0; i < 2; i++) {
		col += ref * Render(ro, rd, ref);
	}

	col = pow(col, vec3(.45));

	fragColor = vec4(col, 1.0);
}
`
let time
let buffer
let program
let touch
let resolution
let pointerCount
let vertices = []
let touching = false

function resize() {
  const { innerWidth: width, innerHeight: height } = window

  canvas.width = width * dpr
  canvas.height = height * dpr

  gl.viewport(0, 0, width * dpr, height * dpr)
}

function compile(shader, source) {
  gl.shaderSource(shader, source)
  gl.compileShader(shader)

  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    console.error(gl.getShaderInfoLog(shader))
  }
}

function setup() {
  const vs = gl.createShader(gl.VERTEX_SHADER)
  const fs = gl.createShader(gl.FRAGMENT_SHADER)

  program = gl.createProgram()

  compile(vs, vertexSource)
  compile(fs, fragmentSource)

  gl.attachShader(program, vs)
  gl.attachShader(program, fs)
  gl.linkProgram(program)

  if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
    console.error(gl.getProgramInfoLog(program))
  }

  vertices = [-1.0, -1.0, 1.0, -1.0, -1.0, 1.0, -1.0, 1.0, 1.0, -1.0, 1.0, 1.0]

  buffer = gl.createBuffer()

  gl.bindBuffer(gl.ARRAY_BUFFER, buffer)
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW)

  const position = gl.getAttribLocation(program, "position")

  gl.enableVertexAttribArray(position)
  gl.vertexAttribPointer(position, 2, gl.FLOAT, false, 0, 0)

  time = gl.getUniformLocation(program, "time")
  touch = gl.getUniformLocation(program, "touch")
  pointerCount = gl.getUniformLocation(program, "pointerCount")
  resolution = gl.getUniformLocation(program, "resolution")
}

function draw(now) {
  gl.clearColor(0, 0, 0, 1)
  gl.clear(gl.COLOR_BUFFER_BIT)

  gl.useProgram(program)
  gl.bindBuffer(gl.ARRAY_BUFFER, buffer)

  gl.uniform1f(time, now * 0.001)
  gl.uniform2f(touch, ...getTouches())
  gl.uniform1i(pointerCount, touches.size)
  gl.uniform2f(resolution, canvas.width, canvas.height)
  gl.drawArrays(gl.TRIANGLES, 0, vertices.length * 0.5)
}

function getTouches() {
  if (!touches.size) {
    return [0, 0]
  }

  for (let [id, t] of touches) {
    const result = [dpr * t.clientX, dpr * (innerHeight - t.clientY)]

    return result
  }
}

function loop(now) {
  draw(now)
  requestAnimationFrame(loop)
}

function init() {
  setup()
  resize()
  loop(0)
}

document.body.onload = init
window.onresize = resize 
canvas.onpointerdown = e => {
  touching = true
  touches.set(e.pointerId, e)
}
canvas.onpointermove = e => {
  if (!touching) return
  touches.set(e.pointerId, e)
}
canvas.onpointerup = e => {
  touching = false
  touches.clear()
}
canvas.onpointerout = e => {
  touching = false
  touches.clear()
}