// WebGL2 side of the browser view: the block window as a 3D texture, a two-level DDA ray marcher in a full-screen
// fragment shader, entity boxes. Coordinates in the shader are relative to the window origin (a chunk-aligned corner).
import { SHADING_GLSL } from './shading.mjs'

export const MAX_ENTITIES = 64
export const KINDS = { cube: 0, box: 1, cross: 2, water: 3, lava: 4 }
const MATERIAL_COLUMNS = 4 // top, side, bottom, kind
const ISSUE_FLAG = 64 // the table's `issue` as a material flag, read by debugColor in the shader
const INFO_TEXELS = 4 // per material: layers+kind, flags+emit, box min, box max

const VERTEX = `#version 300 es
void main () {
  vec2 p = vec2(float((gl_VertexID << 1) & 2), float(gl_VertexID & 2));
  gl_Position = vec4(p * 2.0 - 1.0, 0.0, 1.0);
}`

const FRAGMENT = `#version 300 es
precision highp float;
precision highp int;
precision highp usampler3D;
precision highp usampler2D;
precision highp sampler2DArray;
uniform usampler3D uBlocks;
uniform usampler3D uCoarse;
uniform usampler3D uLightTex;
uniform sampler2D uMats;
uniform usampler2D uInfo;
uniform sampler2DArray uTex;
uniform float uLodMax;
uniform vec2 uRes;
uniform vec3 uEye;
uniform vec3 uFwd;
uniform vec3 uRight;
uniform vec3 uUp;
uniform float uHalf;
uniform ivec3 uSize;
uniform ivec2 uSlotOff;
uniform float uDist;
uniform float uDarken;
uniform int uDebug;
uniform int uEntCount;
uniform vec3 uEntMin[${MAX_ENTITIES}];
uniform vec3 uEntMax[${MAX_ENTITIES}];
uniform vec3 uEntCol[${MAX_ENTITIES}];
out vec4 outColor;

const int MAX_STEPS = 2048;
${SHADING_GLSL}
const uint CUTOUT = 1u;
const uint TRANSLUCENT = 2u;
const uint EMISSIVE = 16u;
const uint ISSUE = 64u;

// ?debug=1: a material the view draws wrong (flag set from the table's issue field) is a magenta/black checker, two texels a square
vec3 debugColor (vec3 col, uint flags, vec2 uv) {
  if (uDebug == 0 || (flags & ISSUE) == 0u) return col;
  vec2 q = floor(uv * 8.0);
  return mod(q.x + q.y, 2.0) < 1.0 ? vec3(1.0, 0.0, 1.0) : vec3(0.0);
}

bool inWindow (ivec3 c) {
  return all(greaterThanEqual(c, ivec3(0))) && all(lessThan(c, uSize));
}

ivec3 wrapCell (ivec3 c) {
  return ivec3((c.x + uSlotOff.x) % uSize.x, c.y, (c.z + uSlotOff.y) % uSize.z);
}

// the coarse flag of a cell's section: 0 the slot holds no column (its blocks and light are stale), 1 blocks, 2 all air
uint sectionFlag (ivec3 c) {
  return texelFetch(uCoarse, wrapCell(c) >> 4, 0).r;
}

// (sky, block) of a cell: open sky outside the window or in an unloaded slot, dark below the world
vec2 cellLight (ivec3 c) {
  if (c.y < 0) return vec2(0.0);
  if (!inWindow(c) || sectionFlag(c) == 0u) return vec2(15.0, 0.0);
  uint v = texelFetch(uLightTex, wrapCell(c), 0).r;
  return vec2(float(v >> 4), float(v & 15u));
}

// a full opaque cube: what occludes ambient light
bool occludes (ivec3 c) {
  if (!inWindow(c) || sectionFlag(c) != 1u) return false;
  uint m = texelFetch(uBlocks, wrapCell(c), 0).r;
  if (m == 0u) return false;
  return int(texelFetch(uInfo, ivec2(0, int(m)), 0).w) == 0 && (texelFetch(uInfo, ivec2(1, int(m)), 0).r & (CUTOUT | TRANSLUCENT)) == 0u;
}

// smooth light and ambient occlusion on the face of the cell that looks along -stepSign on axis, interpolated at local;
// inside: the face is within the cell (the top of a slab), so it looks into the cell itself, not the neighbour
vec3 faceLight (ivec3 cell, int axis, float stepSign, vec3 local, bool inside) {
  ivec3 f = cell;
  if (!inside) f[axis] -= int(stepSign);
  int b = (axis + 1) % 3;
  int c = (axis + 2) % 3;
  ivec3 eb = ivec3(0);
  ivec3 ec = ivec3(0);
  eb[b] = 1;
  ec[c] = 1;
  vec2 lg[9];
  bool og[9];
  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 3; j++) {
      ivec3 p = f + (i - 1) * eb + (j - 1) * ec;
      lg[i * 3 + j] = cellLight(p);
      og[i * 3 + j] = (i != 1 || j != 1) && occludes(p);
    }
  }
  vec3 sum = vec3(0.0);
  for (int i = 0; i < 2; i++) {
    for (int j = 0; j < 2; j++) {
      int di = i * 2 - 1;
      int dj = j * 2 - 1;
      int s1 = (1 + di) * 3 + 1;
      int s2 = 3 + 1 + dj;
      int k = (1 + di) * 3 + 1 + dj;
      vec3 corner = cornerLight(lg[4], lg[s1], lg[s2], lg[k], bvec3(og[s1], og[s2], og[k]));
      float wu = i == 1 ? local[b] : 1.0 - local[b];
      float wv = j == 1 ? local[c] : 1.0 - local[c];
      sum += corner * wu * wv;
    }
  }
  return sum;
}

vec3 cellColor (ivec3 c) {
  vec2 l = cellLight(c);
  return lightColor(l.x, l.y, uDarken);
}
// mip level from distance: derivatives are discontinuous across voxels
float lodAt (float t, float dAxis) {
  float texels = t * (2.0 * uHalf / uRes.x) * 16.0 / max(abs(dAxis), 0.25);
  return clamp(log2(max(texels, 1e-6)), 0.0, uLodMax);
}

vec4 texAt (uint layerCode, vec2 uv, float lod) {
  return textureLod(uTex, vec3(uv, float(layerCode) - 1.0), lod);
}

// the two diagonal quads through a cell (planes lx - lz = 0 and lx + lz = 1), nearest texel with alpha >= 0.5 past t
bool crossHit (vec3 lo, vec3 dd, float t, uint layerCode, out float sHit, out vec3 rgb) {
  float sa = (lo.z - lo.x) / (abs(dd.x - dd.z) < 1e-6 ? 1e-6 : dd.x - dd.z);
  float sb = (1.0 - lo.x - lo.z) / (abs(dd.x + dd.z) < 1e-6 ? 1e-6 : dd.x + dd.z);
  bool found = false;
  sHit = 1e30;
  for (int k = 0; k < 2; k++) {
    float s = k == 0 ? sa : sb;
    vec3 l = lo + dd * s;
    if (s < t - 1e-4 || s >= sHit || any(lessThan(l, vec3(-1e-4))) || any(greaterThan(l, vec3(1.0001)))) continue;
    vec4 c = texAt(layerCode, vec2(clamp(l.x, 0.0, 1.0), clamp(1.0 - l.y, 0.0, 1.0)), lodAt(s, 0.7));
    if (c.a < 0.5) continue;
    found = true;
    sHit = s;
    rgb = c.rgb;
  }
  return found;
}

vec3 skyColor (vec3 d) {
  float up = clamp(d.y, 0.0, 1.0);
  vec3 day = mix(vec3(200.0, 222.0, 255.0), vec3(105.0, 160.0, 250.0), up) / 255.0;
  return mix(vec3(0.01, 0.015, 0.04), day, clamp((uDarken - 0.2) / 0.8, 0.0, 1.0));
}

float shadeOf (int axis, float dirComponent) {
  if (axis == 1) return dirComponent < 0.0 ? 1.0 : 0.5;
  return axis == 2 ? 0.8 : 0.62;
}

void main () {
  vec2 ndc = gl_FragCoord.xy / uRes * 2.0 - 1.0;
  vec3 d = normalize(uFwd + uRight * (ndc.x * uHalf) + uUp * (ndc.y * uHalf * uRes.y / uRes.x));
  vec3 sky = skyColor(d);
  vec3 dd = vec3(abs(d.x) < 1e-7 ? 1e-7 : d.x, abs(d.y) < 1e-7 ? 1e-7 : d.y, abs(d.z) < 1e-7 ? 1e-7 : d.z);
  vec3 inv = 1.0 / dd;
  vec3 o = uEye;
  ivec3 stp = ivec3(sign(dd));
  vec3 s01 = vec3(greaterThan(dd, vec3(0.0)));

  vec3 t1 = (vec3(0.0) - o) * inv;
  vec3 t2 = (vec3(uSize) - o) * inv;
  vec3 tn = min(t1, t2);
  vec3 tf = max(t1, t2);
  float tEnter = max(max(tn.x, tn.y), tn.z);
  float tLeave = min(min(tf.x, tf.y), tf.z);
  float t = max(tEnter, 0.0);
  bool alive = tLeave > t && t < uDist;
  int axis = 1;
  if (tEnter > 0.0) axis = tn.x >= tn.y && tn.x >= tn.z ? 0 : (tn.y >= tn.z ? 1 : 2);

  ivec3 cell = clamp(ivec3(floor(o + dd * (t + (tEnter > 0.0 ? 1e-3 : 0.0)))), ivec3(0), uSize - 1);
  vec3 tMax = (vec3(cell) + s01 - o) * inv;
  vec3 tDelta = abs(inv);

  bool hit = false;
  float tHit = uDist;
  vec3 hitCol = vec3(0.0);
  ivec3 hitCell = ivec3(0);
  int hitAxis = 1;
  float hitSign = 1.0;
  vec3 hitLocal = vec3(0.5);
  bool hitInside = false; // the hit face is inside its cell (a slab top): lit from the cell itself
  int hitMode = 1; // 0 smooth light and AO, 1 the cell's own light, 2 emissive
  vec3 acc = vec3(0.0);
  float trans = 1.0;
  uint prevM = 0u;

  for (int i = 0; i < MAX_STEPS; i++) {
    if (!alive) break;
    ivec3 tc = ivec3((cell.x + uSlotOff.x) % uSize.x, cell.y, (cell.z + uSlotOff.y) % uSize.z);
    if (texelFetch(uCoarse, ivec3(tc.x >> 4, tc.y >> 4, tc.z >> 4), 0).r != 1u) {
      ivec3 slo = (cell >> 4) << 4;
      vec3 te = (vec3(slo) + s01 * 16.0 - o) * inv;
      int ax = te.x <= te.y && te.x <= te.z ? 0 : (te.y <= te.z ? 1 : 2);
      float tx = te[ax];
      ivec3 nc = clamp(ivec3(floor(o + dd * tx)), slo, slo + 15);
      nc[ax] = stp[ax] > 0 ? slo[ax] + 16 : slo[ax] - 1;
      cell = nc;
      axis = ax;
      t = tx;
      tMax = (vec3(cell) + s01 - o) * inv;
      prevM = 0u;
      if (any(lessThan(cell, ivec3(0))) || any(greaterThanEqual(cell, uSize)) || t >= uDist) break;
      continue;
    }
    uint m = texelFetch(uBlocks, tc, 0).r;
    if (m != 0u && !(m == prevM && (texelFetch(uInfo, ivec2(1, int(m)), 0).r & 32u) != 0u)) {
      uvec4 i0 = texelFetch(uInfo, ivec2(0, int(m)), 0);
      uint flags = texelFetch(uInfo, ivec2(1, int(m)), 0).r;
      int kind = int(i0.w);
      // a box face is hit where the ray meets the box (t, axis of that face), a miss passes over / beside it
      float ht = t;
      int hAxis = axis;
      bool miss = kind == 1 && !rayBoxLocal(o - vec3(cell), inv, vec3(texelFetch(uInfo, ivec2(2, int(m)), 0).xyz), vec3(texelFetch(uInfo, ivec2(3, int(m)), 0).xyz), t, min(min(tMax.x, tMax.y), tMax.z), axis, ht, hAxis);
      if (!miss) {
        float stepSign = float(stp[hAxis]);
        vec3 local = clamp(o + dd * ht - vec3(cell), 0.0, 1.0);
        vec3 fuv = faceUV(hAxis, stepSign, local);
        int face = int(fuv.z + 0.5);
        vec2 uv = fuv.xy;
        vec4 c = texelFetch(uMats, ivec2(face, int(m)), 0);
        if ((flags & 12u) != 0u) {
          bool end = hAxis == ((flags & 4u) != 0u ? 0 : 2);
          face = end ? 0 : 1;
          uv = end ? uv : uv.yx;
        }
        uint layerCode = i0[face];
        bool textured = layerCode != 0u;
        float shade = faceShade(hAxis, stepSign);
        if (kind == 2) {
          vec3 lo = o - vec3(cell);
          float sHit;
          vec3 rgb;
          if (textured && crossHit(lo, dd, t, i0.y, sHit, rgb)) {
            hit = true;
            tHit = sHit;
            vec3 crossAt = lo + dd * sHit;
            hitCol = debugColor(rgb, flags, vec2(crossAt.x, 1.0 - crossAt.y));
            hitCell = cell;
            hitMode = (flags & EMISSIVE) != 0u ? 2 : 1;
            break;
          }
          vec3 b1 = (vec3(0.25, 0.0, 0.25) - lo) * inv;
          vec3 b2 = (vec3(0.75, 0.8, 0.75) - lo) * inv;
          float bn = max(max(min(b1.x, b2.x), min(b1.y, b2.y)), min(b1.z, b2.z));
          float bf = min(min(max(b1.x, b2.x), max(b1.y, b2.y)), max(b1.z, b2.z));
          if (!textured && bn <= bf && bf >= t) {
            hit = true;
            tHit = max(bn, t);
            hitCol = texelFetch(uMats, ivec2(1, int(m)), 0).rgb * 0.95;
            hitCell = cell;
            hitMode = (flags & EMISSIVE) != 0u ? 2 : 1;
            break;
          }
        } else {
          vec4 tex = textured ? texAt(layerCode, uv, lodAt(ht, d[hAxis])) : c;
          // water stays a full cube (in Minecraft a source's surface is at 14/16)
          if (kind == 3 || (flags & 2u) != 0u) {
            if (m != prevM) {
              float alpha = kind == 3 || !textured ? c.a : (tex.a > 0.0 ? tex.a : 0.5);
              acc += trans * alpha * tex.rgb * shade * ((flags & EMISSIVE) != 0u ? vec3(1.0) : cellColor(cell));
              trans *= 1.0 - alpha;
            }
          } else if (!(textured && (flags & 1u) != 0u && tex.a < 0.5)) {
            hit = true;
            tHit = ht;
            hitCol = debugColor(tex.rgb, flags, uv) * shade;
            hitCell = cell;
            hitAxis = hAxis;
            hitSign = stepSign;
            hitLocal = local;
            hitInside = kind == 1 && local[hAxis] > 1e-3 && local[hAxis] < 1.0 - 1e-3;
            hitMode = (flags & EMISSIVE) != 0u ? 2 : (kind <= 1 ? 0 : 1);
            break;
          }
        }
      }
    }
    prevM = m;
    if (tMax.x < tMax.y && tMax.x < tMax.z) { axis = 0; t = tMax.x; cell.x += stp.x; tMax.x += tDelta.x; }
    else if (tMax.y < tMax.z) { axis = 1; t = tMax.y; cell.y += stp.y; tMax.y += tDelta.y; }
    else { axis = 2; t = tMax.z; cell.z += stp.z; tMax.z += tDelta.z; }
    if (any(lessThan(cell, ivec3(0))) || any(greaterThanEqual(cell, uSize)) || t >= uDist) break;
  }

  vec3 color = sky;
  float tNearest = hit ? tHit : uDist;
  if (hit) {
    float fog = clamp((tHit / uDist - 0.6) / 0.4, 0.0, 1.0);
    vec3 lit = vec3(1.0);
    if (hitMode == 0) {
      vec3 l = faceLight(hitCell, hitAxis, hitSign, hitLocal, hitInside);
      lit = l.z * lightColor(l.x, l.y, uDarken);
    } else if (hitMode == 1) {
      lit = cellColor(hitCell);
    }
    color = mix(hitCol * lit, sky, fog);
  }
  float bestT = tNearest;
  for (int i = 0; i < ${MAX_ENTITIES}; i++) {
    if (i >= uEntCount) break;
    vec3 e1 = (uEntMin[i] - o) * inv;
    vec3 e2 = (uEntMax[i] - o) * inv;
    vec3 en = min(e1, e2);
    vec3 ef = max(e1, e2);
    float ten = max(max(en.x, en.y), en.z);
    float tef = min(min(ef.x, ef.y), ef.z);
    if (ten > tef || tef < 0.0) continue;
    float te = max(ten, 0.0);
    if (te >= bestT) continue;
    bestT = te;
    int eax = en.x >= en.y && en.x >= en.z ? 0 : (en.y >= en.z ? 1 : 2);
    float fog = clamp((te / uDist - 0.6) / 0.4, 0.0, 1.0);
    color = mix(uEntCol[i] * shadeOf(eax, d[eax]) * cellColor(ivec3(floor(o + dd * max(te - 0.01, 0.0)))), sky, fog);
  }
  outColor = vec4(acc + trans * color, 1.0);
}`

const compile = (gl, type, source) => {
  const shader = gl.createShader(type)
  gl.shaderSource(shader, source)
  gl.compileShader(shader)
  if (gl.getShaderParameter(shader, gl.COMPILE_STATUS)) return shader
  throw new Error(`shader compile failed: ${gl.getShaderInfoLog(shader)}`)
}

const link = gl => {
  const program = gl.createProgram()
  gl.attachShader(program, compile(gl, gl.VERTEX_SHADER, VERTEX))
  gl.attachShader(program, compile(gl, gl.FRAGMENT_SHADER, FRAGMENT))
  gl.linkProgram(program)
  if (gl.getProgramParameter(program, gl.LINK_STATUS)) return program
  throw new Error(`program link failed: ${gl.getProgramInfoLog(program)}`)
}

const nearestTexture = (gl, target, unit) => {
  const texture = gl.createTexture()
  gl.activeTexture(gl.TEXTURE0 + unit)
  gl.bindTexture(target, texture)
  gl.texParameteri(target, gl.TEXTURE_MIN_FILTER, gl.NEAREST)
  gl.texParameteri(target, gl.TEXTURE_MAG_FILTER, gl.NEAREST)
  gl.texParameteri(target, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE)
  gl.texParameteri(target, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE)
  return texture
}

// RGBA8 texture, 4 columns by one row per material: top, side, bottom colour, then the kind in red
export const materialPixels = materials => {
  const data = new Uint8Array(materials.length * MATERIAL_COLUMNS * 4)
  materials.forEach((material, row) => {
    const faces = [material.top, material.side, material.bottom]
    faces.forEach((rgba, column) => data.set((rgba ?? [0, 0, 0, 0]).map(Math.round), (row * MATERIAL_COLUMNS + column) * 4))
    data[(row * MATERIAL_COLUMNS + 3) * 4] = KINDS[material.kind] ?? 0
  })
  return data
}

// RGBA16UI, INFO_TEXELS texels per material: (top+1, side+1, bottom+1, kind), (flags, emit), box min, box max (1/16 units)
export const materialInfo = materials => {
  const data = new Uint16Array(materials.length * INFO_TEXELS * 4)
  materials.forEach((m, row) => {
    const [x0, y0, z0, x1, y1, z1] = m.box ?? [0, 0, 0, 16, 16, 16]
    data.set([...(m.tex ?? [-1, -1, -1]).map(l => l + 1), KINDS[m.kind] ?? 0, (m.flags ?? 0) | (m.issue ? ISSUE_FLAG : 0), m.emit ?? 0, 0, 0, x0, y0, z0, 0, x1, y1, z1, 0], row * INFO_TEXELS * 4)
  })
  return data
}

// the level-major texture bytes as one subarray per mip level
export const textureLevels = (bytes, layers, size, levels) => {
  const sizes = Array.from({ length: levels }, (_, l) => (size >> l) ** 2 * 4 * layers)
  return sizes.map((n, l) => bytes.subarray(sizes.slice(0, l).reduce((a, b) => a + b, 0), sizes.slice(0, l + 1).reduce((a, b) => a + b, 0)))
}

export function createRenderer (canvas) {
  const gl = canvas.getContext('webgl2', { antialias: false, alpha: false, powerPreference: 'high-performance' })
  if (!gl) throw new Error('WebGL2 is not available')
  const debugInfo = gl.getExtension('WEBGL_debug_renderer_info')
  const renderer = debugInfo ? gl.getParameter(debugInfo.UNMASKED_RENDERER_WEBGL) : gl.getParameter(gl.RENDERER)
  const program = link(gl)
  const uniform = Object.fromEntries(['uBlocks', 'uCoarse', 'uMats', 'uInfo', 'uTex', 'uLodMax', 'uRes', 'uEye', 'uFwd', 'uRight', 'uUp', 'uHalf', 'uSize', 'uSlotOff', 'uDist', 'uDarken', 'uDebug', 'uLightTex', 'uEntCount', 'uEntMin', 'uEntMax', 'uEntCol']
    .map(name => [name, gl.getUniformLocation(program, name)]))
  gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1)
  gl.bindVertexArray(gl.createVertexArray())
  gl.useProgram(program)

  const blocks = nearestTexture(gl, gl.TEXTURE_3D, 0)
  const coarse = nearestTexture(gl, gl.TEXTURE_3D, 1)
  const mats = nearestTexture(gl, gl.TEXTURE_2D, 2)
  const info = nearestTexture(gl, gl.TEXTURE_2D, 3)
  const tex = nearestTexture(gl, gl.TEXTURE_2D_ARRAY, 4)
  const lightTex = nearestTexture(gl, gl.TEXTURE_3D, 5)
  gl.texParameteri(gl.TEXTURE_2D_ARRAY, gl.TEXTURE_MIN_FILTER, gl.NEAREST_MIPMAP_LINEAR)
  let world = null
  let lodMax = 0
  let debug = false

  const setMaterials = materials => {
    gl.activeTexture(gl.TEXTURE2)
    gl.bindTexture(gl.TEXTURE_2D, mats)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, MATERIAL_COLUMNS, materials.length, 0, gl.RGBA, gl.UNSIGNED_BYTE, materialPixels(materials))
    gl.activeTexture(gl.TEXTURE3)
    gl.bindTexture(gl.TEXTURE_2D, info)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA16UI, INFO_TEXELS, materials.length, 0, gl.RGBA_INTEGER, gl.UNSIGNED_SHORT, materialInfo(materials))
  }

  // once per page: texStorage is immutable
  const setTextures = ({ bytes, layers, size, levels }) => {
    gl.activeTexture(gl.TEXTURE4)
    gl.bindTexture(gl.TEXTURE_2D_ARRAY, tex)
    gl.texStorage3D(gl.TEXTURE_2D_ARRAY, levels, gl.RGBA8, size, size, layers)
    textureLevels(bytes, layers, size, levels).forEach((data, l) => {
      const n = size >> l
      gl.texSubImage3D(gl.TEXTURE_2D_ARRAY, l, 0, 0, 0, n, n, layers, gl.RGBA, gl.UNSIGNED_BYTE, data)
    })
    lodMax = levels - 1
  }

  // N columns a side, height blocks tall; texStorage contents start zeroed
  const allocate = (n, height) => {
    const sections = height >> 4
    gl.activeTexture(gl.TEXTURE0)
    gl.bindTexture(gl.TEXTURE_3D, blocks)
    gl.texStorage3D(gl.TEXTURE_3D, 1, gl.R16UI, n * 16, height, n * 16)
    gl.activeTexture(gl.TEXTURE1)
    gl.bindTexture(gl.TEXTURE_3D, coarse)
    gl.texStorage3D(gl.TEXTURE_3D, 1, gl.R8UI, n, sections, n)
    gl.activeTexture(gl.TEXTURE5)
    gl.bindTexture(gl.TEXTURE_3D, lightTex)
    gl.texStorage3D(gl.TEXTURE_3D, 1, gl.R8UI, n * 16, height, n * 16)
    world = { n, height, sections, zeros: new Uint16Array(256 * height), openSky: new Uint8Array(256 * height).fill(0xf0), noFlags: new Uint8Array(sections) }
  }

  // mats is 16*height*16 material indices, x fastest then y then z; flags one byte per section, bottom first; light is
  // sky << 4 | block per cell in the same order
  // slot flags: 0 no column (mats and light are stale and never read), 1 a section with blocks, 2 an all-air section
  const putFlags = (sx, sz, flags) => {
    gl.activeTexture(gl.TEXTURE1)
    gl.bindTexture(gl.TEXTURE_3D, coarse)
    gl.texSubImage3D(gl.TEXTURE_3D, 0, sx, 0, sz, 1, world.sections, 1, gl.RED_INTEGER, gl.UNSIGNED_BYTE, flags)
  }
  const uploadColumn = (sx, sz, mats16, flags, light) => {
    gl.activeTexture(gl.TEXTURE0)
    gl.bindTexture(gl.TEXTURE_3D, blocks)
    gl.texSubImage3D(gl.TEXTURE_3D, 0, sx * 16, 0, sz * 16, 16, world.height, 16, gl.RED_INTEGER, gl.UNSIGNED_SHORT, mats16)
    putFlags(sx, sz, flags.map(f => (f ? 1 : 2)))
    gl.activeTexture(gl.TEXTURE5)
    gl.bindTexture(gl.TEXTURE_3D, lightTex)
    gl.texSubImage3D(gl.TEXTURE_3D, 0, sx * 16, 0, sz * 16, 16, world.height, 16, gl.RED_INTEGER, gl.UNSIGNED_BYTE, light)
  }
  // an unloaded slot reads as air with open sky (so loaded neighbours are not shaded black): only its flags are zeroed,
  // the stale blocks and light stay in the textures and every read checks the flag first
  const clearSlot = (sx, sz) => putFlags(sx, sz, world.noFlags)

  const resize = (w, h) => {
    if (canvas.width !== w) canvas.width = w
    if (canvas.height !== h) canvas.height = h
    gl.viewport(0, 0, w, h)
  }

  // eye is relative to the window origin; entities are {min, max, color} in the same space
  const draw = ({ eye, basis, dist, darken, slotOff, entities }) => {
    const count = Math.min(entities.length, MAX_ENTITIES)
    const flat = key => new Float32Array(MAX_ENTITIES * 3).map((_, i) => (entities[Math.floor(i / 3)]?.[key]?.[i % 3]) ?? 0)
    gl.uniform1i(uniform.uBlocks, 0)
    gl.uniform1i(uniform.uCoarse, 1)
    gl.uniform1i(uniform.uMats, 2)
    gl.uniform1i(uniform.uInfo, 3)
    gl.uniform1i(uniform.uTex, 4)
    gl.uniform1i(uniform.uLightTex, 5)
    gl.uniform1f(uniform.uLodMax, lodMax)
    gl.uniform2f(uniform.uRes, canvas.width, canvas.height)
    gl.uniform3f(uniform.uEye, eye.x, eye.y, eye.z)
    gl.uniform3f(uniform.uFwd, basis.forward.x, basis.forward.y, basis.forward.z)
    gl.uniform3f(uniform.uRight, basis.right.x, basis.right.y, basis.right.z)
    gl.uniform3f(uniform.uUp, basis.up.x, basis.up.y, basis.up.z)
    gl.uniform1f(uniform.uHalf, basis.half)
    gl.uniform3i(uniform.uSize, world.n * 16, world.height, world.n * 16)
    gl.uniform2i(uniform.uSlotOff, slotOff.x, slotOff.z)
    gl.uniform1f(uniform.uDist, dist)
    gl.uniform1f(uniform.uDarken, darken)
    gl.uniform1i(uniform.uDebug, debug ? 1 : 0)
    gl.uniform1i(uniform.uEntCount, count)
    gl.uniform3fv(uniform.uEntMin, flat('min'))
    gl.uniform3fv(uniform.uEntMax, flat('max'))
    gl.uniform3fv(uniform.uEntCol, flat('color'))
    gl.drawArrays(gl.TRIANGLES, 0, 3)
  }

  const clear = (r, g, b) => {
    gl.clearColor(r, g, b, 1)
    gl.clear(gl.COLOR_BUFFER_BIT)
  }

  const setDebug = on => { debug = on }
  return { renderer, setDebug, setMaterials, setTextures, allocate, uploadColumn, clearSlot, resize, draw, clear, finish: () => gl.finish(), isAllocated: () => world !== null }
}
