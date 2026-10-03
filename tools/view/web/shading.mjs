// Light curve, sky darkening and face orientation of Minecraft Java's client (vanilla 1.20-1.21), each as a JS function
// and as GLSL ES 3.00 in SHADING_GLSL mirroring it (same names and constants). No node imports: served as-is to the page.

const clamp = (v, lo, hi) => Math.min(hi, Math.max(lo, v))
const mix = (a, b, t) => a + (b - a) * t
const fract = v => v - Math.floor(v)

export function celestialAngle (timeOfDay) {
  const d = fract(timeOfDay / 24000 - 0.25)
  const e = 0.5 - Math.cos(d * Math.PI) / 2
  return (2 * d + e) / 3
}

export function skyDarken (timeOfDay, rain, thunder = 0) {
  const f = celestialAngle(timeOfDay)
  let g = 1 - (Math.cos(f * 2 * Math.PI) * 2 + 0.2)
  g = clamp(g, 0, 1)
  g = 1 - g
  g *= 1 - rain * 5 / 16
  g *= 1 - thunder * 5 / 16
  return g * 0.8 + 0.2
}

export const brightness = level => {
  const f = level / 15
  return f / (4 - 3 * f)
}

// the lightmap: [r, g, b] in 0..1 for sky and block light 0..15 (fractional when smooth-lit)
export function lightColor (sky, block, darken) {
  const s = brightness(sky) * (darken * 0.95 + 0.05)
  const b = brightness(block) * 1.5
  const blockRgb = [b, b * ((b * 0.6 + 0.4) * 0.6 + 0.4), b * (b * b * 0.6 + 0.4)]
  const skyVec = [mix(darken, 1, 0.35), mix(darken, 1, 0.35), 1]
  return blockRgb.map((v, i) => {
    let c = v + skyVec[i] * s
    c = mix(c, 0.75, 0.04)
    c = clamp(c, 0, 1)
    const ng = 1 - Math.pow(1 - c, 4)
    c = mix(c, ng, 0.5)
    c = mix(c, 0.75, 0.04)
    return clamp(c, 0, 1)
  })
}

// the ray stepped along `axis` (0 x, 1 y, 2 z) by stepSign and hit the face with outward normal -stepSign on it
export function faceUV (axis, stepSign, [lx, ly, lz]) {
  if (axis === 1) return stepSign < 0 ? { face: 'top', u: lx, v: lz } : { face: 'bottom', u: lx, v: 1 - lz }
  const v = 1 - ly
  if (axis === 2) return { face: 'side', u: stepSign < 0 ? lx : 1 - lx, v }
  return { face: 'side', u: stepSign < 0 ? 1 - lz : lz, v }
}

export const faceShade = (axis, stepSign) => axis === 1 ? (stepSign < 0 ? 1 : 0.5) : axis === 2 ? 0.8 : 0.6

// smooth lighting at one vertex: each arg {light: [sky, block], opaque}; an opaque neighbour takes the center's light
export function cornerLight (center, side1, side2, corner) {
  const lit = n => n.opaque ? center.light : n.light
  const cornerOpaque = corner.opaque || (side1.opaque && side2.opaque)
  const parts = [center, side1, side2, corner].map(lit)
  const mean = k => parts.reduce((a, p) => a + p[k], 0) / 4
  const ao = (1 + (side1.opaque ? 0.2 : 1) + (side2.opaque ? 0.2 : 1) + (cornerOpaque ? 0.2 : 1)) / 4
  return { sky: mean(0), block: mean(1), ao }
}

export const SHADING_GLSL = `
float celestialAngle(float timeOfDay) {
  float d = fract(timeOfDay / 24000.0 - 0.25);
  float e = 0.5 - cos(d * 3.14159265358979) / 2.0;
  return (2.0 * d + e) / 3.0;
}

float skyDarken(float timeOfDay, float rain, float thunder) {
  float f = celestialAngle(timeOfDay);
  float g = 1.0 - (cos(f * 2.0 * 3.14159265358979) * 2.0 + 0.2);
  g = clamp(g, 0.0, 1.0);
  g = 1.0 - g;
  g *= 1.0 - rain * 5.0 / 16.0;
  g *= 1.0 - thunder * 5.0 / 16.0;
  return g * 0.8 + 0.2;
}

float brightness(float level) {
  float f = level / 15.0;
  return f / (4.0 - 3.0 * f);
}

vec3 lightColor(float sky, float block, float darken) {
  float s = brightness(sky) * (darken * 0.95 + 0.05);
  float b = brightness(block) * 1.5;
  vec3 blockRgb = vec3(b, b * ((b * 0.6 + 0.4) * 0.6 + 0.4), b * (b * b * 0.6 + 0.4));
  vec3 skyVec = mix(vec3(darken, darken, 1.0), vec3(1.0), 0.35);
  vec3 c = blockRgb + skyVec * s;
  c = mix(c, vec3(0.75), 0.04);
  c = clamp(c, 0.0, 1.0);
  vec3 ng = 1.0 - pow(1.0 - c, vec3(4.0));
  c = mix(c, ng, 0.5);
  c = mix(c, vec3(0.75), 0.04);
  return clamp(c, 0.0, 1.0);
}

// (u, v, faceIndex): 0 top, 1 side, 2 bottom
vec3 faceUV(int axis, float stepSign, vec3 local) {
  if (axis == 1) return stepSign < 0.0 ? vec3(local.x, local.z, 0.0) : vec3(local.x, 1.0 - local.z, 2.0);
  float v = 1.0 - local.y;
  if (axis == 2) return vec3(stepSign < 0.0 ? local.x : 1.0 - local.x, v, 1.0);
  return vec3(stepSign < 0.0 ? 1.0 - local.z : local.z, v, 1.0);
}

float faceShade(int axis, float stepSign) {
  if (axis == 1) return stepSign < 0.0 ? 1.0 : 0.5;
  return axis == 2 ? 0.8 : 0.6;
}

// (sky, block, ao); opaque is (side1, side2, corner)
vec3 cornerLight(vec2 c, vec2 s1, vec2 s2, vec2 k, bvec3 opaque) {
  bool cornerOpaque = opaque.z || (opaque.x && opaque.y);
  vec2 sum = c + (opaque.x ? c : s1) + (opaque.y ? c : s2) + (opaque.z ? c : k);
  float ao = (1.0 + (opaque.x ? 0.2 : 1.0) + (opaque.y ? 0.2 : 1.0) + (cornerOpaque ? 0.2 : 1.0)) / 4.0;
  return vec3(sum / 4.0, ao);
}
`
