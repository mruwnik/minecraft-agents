// Why JavaScript: WebGL/GPU; WebGL2 ray marcher for the block window.
// WebGL2 side of the browser view: the block window as a 3D texture, a two-level DDA ray marcher in a full-screen
// fragment shader, entity boxes. Coordinates in the shader are relative to the window origin (a chunk-aligned corner).
import { SHADING_GLSL } from './shading.mjs'
import { LABEL_MARGIN, MAX_LABELS, SEEING_MIN, SEE_NEAR, labelWidth, placeLabels, projectorFor } from './mobs.mjs'

export const MAX_ENTITIES = 64
export const MAX_PART_ROWS = 1024 // parts of all the mobs drawn with models at once; the shader loops at most 24 parts a mob
const PART_UNIT = 10 // texture unit of the parts table
export const KINDS = { cube: 0, box: 1, cross: 2, water: 3, lava: 4, model: 5 }
export const ELEMENT_CAP = 24 // elements looped per model voxel; tools/view/materials.mjs caps a state at the same number
const MATERIAL_COLUMNS = 4 // top, side, bottom, kind
const ISSUE_FLAG = 64 // the table's `issue` as a material flag, read by debugColor in the shader
const INFO_TEXELS = 8 // per material: layers+kind, flags+emit, box min, box max, up/down/north/south, east/west/elemOffset/elemCount, tints up..south, tints east/west
const TINT_GROUP_NAMES = ['none', 'grass', 'foliage', 'dry_foliage', 'water', 'constant'] // tools/view/tints.mjs TINT_GROUPS
const ELEMENT_TEXELS = 15 // tools/view/element-table.mjs documents the element layout the shader reads

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
precision highp sampler2D;
precision highp sampler2DArray;
uniform usampler3D uBlocks;
uniform usampler3D uCoarse;
uniform usampler3D uLightTex;
uniform usampler3D uBiomes; // per-world biome ids, one texel per 4x4x4 blocks
uniform sampler2D uBiomeColors; // per-world biome id (row) -> grass, foliage, dry foliage, water (columns), 4 x 256
uniform int uHasBiomeColors; // 1 once the world has a colour table; else the fixed group colours
uniform sampler2D uMats;
uniform usampler2D uInfo;
uniform sampler2D uElems;
uniform int uElemBase;
uniform vec3 uTintGroups[6];
uniform vec3 uTintConst[32];
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
uniform vec4 uEntRot[${MAX_ENTITIES}]; // a mob with a model: its right (x, z) in the world, then the first row and the count of its parts in uParts (count 0: a plain box)
uniform vec3 uEntOrg[${MAX_ENTITIES}]; // where its model's origin (feet) is
uniform sampler2D uParts; // a part per row, 4 texels: box min, box max, layers top bottom south north, layers east west (-1: none)
uniform int uLabelCount;
uniform vec4 uLabelRect[${MAX_LABELS}]; // x0, y0, x1, y1 in framebuffer pixels from the bottom left
uniform float uLabelDepth[${MAX_LABELS}]; // a label shows where the terrain and mobs are farther than this
uniform vec3 uLabelAt[${MAX_LABELS}]; // the mob's middle: a label is drawn only where the mob is lit enough to be seen or close
uniform float uLabelDist[${MAX_LABELS}]; // eye to the mob's middle
uniform sampler2D uLabels; // one row of text per label, white where there is ink (alpha)
out vec4 outColor;

const int MAX_STEPS = 2048;
${SHADING_GLSL}
const uint CUTOUT = 1u;
const uint TRANSLUCENT = 2u;
const uint EMISSIVE = 16u;
const uint ISSUE = 64u;
const uint SIX_FACES = 128u;
const int ELEMENT_CAP = ${ELEMENT_CAP};

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

// The colour a face with this tint group (0 none, 1 grass, 2 foliage, 3 dry foliage, 4 water, 5 constant + its table index) is
// multiplied by. Groups 1-4 read the biome of the cell's 4x4x4 block from the world's table; without a table, or in a slot
// holding no column, the fixed colour of the group.
vec3 tintFor (uint group, uint constIdx, ivec3 cell) {
  if (group == 0u) return vec3(1.0);
  if (group == 5u) return uTintConst[min(constIdx, 31u)];
  if (uHasBiomeColors == 0 || group > 4u || !inWindow(cell) || sectionFlag(cell) == 0u) return uTintGroups[min(group, 5u)];
  uint id = texelFetch(uBiomes, wrapCell(cell) >> ivec3(2), 0).r;
  return texelFetch(uBiomeColors, ivec2(int(group) - 1, int(id)), 0).rgb;
}

// ---- model elements (kind 5): the element table of tools/view/element-table.mjs ----

vec4 elemTexel (int lin) {
  return texelFetch(uElems, ivec2(lin & 1023, lin >> 10), 0);
}

// right-handed rotation by a radians about axis 1 x, 2 y, 3 z
vec3 rotAbout (vec3 v, int axis, float a) {
  float c = cos(a);
  float s = sin(a);
  if (axis == 1) return vec3(v.x, v.y * c - v.z * s, v.y * s + v.z * c);
  if (axis == 2) return vec3(v.x * c + v.z * s, v.y, -v.x * s + v.z * c);
  return vec3(v.x * c - v.y * s, v.x * s + v.y * c, v.z);
}

// where the point (element frame) sits on the face f (0 up, 1 down, 2 north, 3 south, 4 east, 5 west), in vanilla's uv frame, 0..1 each
vec2 faceFrac (int f, vec3 p, vec3 lo, vec3 hi) {
  vec3 q = (p - lo) / max(hi - lo, vec3(1e-4));
  if (f == 0) return vec2(q.x, q.z);
  if (f == 1) return vec2(q.x, 1.0 - q.z);
  if (f == 2) return vec2(1.0 - q.x, 1.0 - q.y);
  if (f == 3) return vec2(q.x, 1.0 - q.y);
  if (f == 4) return vec2(1.0 - q.z, 1.0 - q.y);
  return vec2(q.z, 1.0 - q.y);
}

// the texture of a face turned r quarter turns clockwise
vec2 rotFrac (vec2 f, int r) {
  if (r == 1) return vec2(f.y, 1.0 - f.x);
  if (r == 2) return vec2(1.0 - f.x, 1.0 - f.y);
  if (r == 3) return vec2(1.0 - f.y, f.x);
  return f;
}

float modelShade (vec3 n) {
  float up = max(n.y, 0.0);
  float down = max(-n.y, 0.0);
  return up * up + down * down * 0.5 + n.z * n.z * 0.8 + n.x * n.x * 0.6;
}

// The nearest element face a ray meets inside one voxel: lo is the eye in the voxel's own 0..1 space, dd the ray, [t, tExit] the span
// inside the voxel. Each element is tested in its own frame (rotated, rescaled), the entry face is textured, and a texel under alpha 0.5
// lets the ray pass. Returns the ray distance, the colour, the face normal in block space and whether the element is rotated.
bool modelHit (vec3 lo, vec3 dd, float t, float tExit, int offset, int count, vec3 dirW, ivec3 cell, out float sBest, out vec3 rgb, out vec3 nBest, out bool rotated, out bool shaded) {
  bool found = false;
  sBest = 1e30;
  rgb = vec3(0.0);
  nBest = vec3(0.0, 1.0, 0.0);
  rotated = false;
  shaded = true;
  for (int k = 0; k < ELEMENT_CAP; k++) {
    if (k >= count) break;
    int li = offset + k;
    int id = int(elemTexel(li >> 2)[li & 3] + 0.5);
    int base = uElemBase + id * ${ELEMENT_TEXELS};
    vec4 a = elemTexel(base);
    vec4 b = elemTexel(base + 1);
    int eflags = int(b.w + 0.5);
    int axisR = eflags & 3;
    vec3 p = lo * 16.0;
    vec3 dv = dd * 16.0;
    float ang = radians(a.w);
    if (axisR != 0) {
      vec3 origin = elemTexel(base + 2).xyz;
      p -= origin;
      if ((eflags & 4) != 0) {
        vec3 sc = vec3(cos(ang));
        sc[axisR - 1] = 1.0;
        p *= sc;
        dv *= sc;
      }
      p = rotAbout(p, axisR, -ang) + origin;
      dv = rotAbout(dv, axisR, -ang);
    }
    vec3 dsafe = vec3(abs(dv.x) < 1e-7 ? 1e-7 : dv.x, abs(dv.y) < 1e-7 ? 1e-7 : dv.y, abs(dv.z) < 1e-7 ? 1e-7 : dv.z);
    vec3 e1 = (a.xyz - p) / dsafe;
    vec3 e2 = (b.xyz - p) / dsafe;
    vec3 en = min(e1, e2);
    vec3 ef = max(e1, e2);
    float sIn = max(max(en.x, en.y), en.z);
    float sOut = min(min(ef.x, ef.y), ef.z);
    if (sIn > sOut || sIn < t - 1e-4 || sIn > tExit + 1e-4 || sIn > sBest + 1e-4) continue; // a later element wins a tie: an overlay over its base
    int ax = en.x >= en.y && en.x >= en.z ? 0 : (en.y >= en.z ? 1 : 2);
    bool positive = dsafe[ax] < 0.0; // entering through the max side: the face looks along +axis
    int f = ax == 1 ? (positive ? 0 : 1) : (ax == 2 ? (positive ? 3 : 2) : (positive ? 4 : 5));
    vec4 fa = elemTexel(base + 3 + 2 * f);
    int layerCode = int(fa.x + 0.5);
    if (layerCode == 0) continue;
    vec4 fuv = elemTexel(base + 4 + 2 * f);
    vec3 hp = p + dsafe * sIn;
    vec2 fr = rotFrac(faceFrac(f, hp, a.xyz, b.xyz), int(fa.y + 0.5));
    vec3 ne = vec3(0.0);
    ne[ax] = positive ? 1.0 : -1.0;
    vec3 nw = axisR != 0 ? rotAbout(ne, axisR, ang) : ne;
    vec4 c = texAt(uint(layerCode), mix(fuv.xy, fuv.zw, fr) / 16.0, lodAt(sIn, dot(dirW, nw)));
    if (c.a < 0.5) continue;
    found = true;
    sBest = sIn;
    int tintCode = int(fa.z + 0.5);
    rgb = c.rgb * tintFor(uint(tintCode & 7), uint(tintCode >> 3), cell);
    nBest = nw;
    rotated = axisR != 0;
    shaded = (eflags & 8) == 0;
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
      if (kind == 5) {
        uvec4 i5 = texelFetch(uInfo, ivec2(5, int(m)), 0);
        float sM;
        vec3 rgbM;
        vec3 nM;
        bool rotM;
        bool shadedM;
        if (modelHit(o - vec3(cell), dd, t, min(min(tMax.x, tMax.y), tMax.z), int(i5.z), int(i5.w), d, cell, sM, rgbM, nM, rotM, shadedM)) {
          ivec3 lightCell = cell;
          if (!rotM) {
            int nax = abs(nM.x) > 0.5 ? 0 : (abs(nM.y) > 0.5 ? 1 : 2);
            vec3 lp = o + dd * sM - vec3(cell);
            if (nM[nax] > 0.0 && lp[nax] > 0.999) lightCell[nax] += 1;
            else if (nM[nax] < 0.0 && lp[nax] < 0.001) lightCell[nax] -= 1;
          }
          hit = true;
          tHit = sM;
          hitCol = debugColor(rgbM, flags, vec2(0.5)) * (shadedM ? modelShade(nM) : 1.0);
          hitCell = lightCell;
          hitMode = (flags & EMISSIVE) != 0u ? 2 : 1;
          break;
        }
      }
      bool miss = kind == 5 || (kind == 1 && !rayBoxLocal(o - vec3(cell), inv, vec3(texelFetch(uInfo, ivec2(2, int(m)), 0).xyz), vec3(texelFetch(uInfo, ivec2(3, int(m)), 0).xyz), t, min(min(tMax.x, tMax.y), tMax.z), axis, ht, hAxis));
      if (!miss) {
        float stepSign = float(stp[hAxis]);
        vec3 local = clamp(o + dd * ht - vec3(cell), 0.0, 1.0);
        vec3 fuv = faceUV(hAxis, stepSign, local);
        int face = int(fuv.z + 0.5);
        vec2 uv = fuv.xy;
        vec4 c = texelFetch(uMats, ivec2(face, int(m)), 0);
        bool six = (flags & SIX_FACES) != 0u;
        if (!six && (flags & 12u) != 0u) {
          bool end = hAxis == ((flags & 4u) != 0u ? 0 : 2);
          face = end ? 0 : 1;
          uv = end ? uv : uv.yx;
        }
        uint layerCode = i0[face];
        uint sixTint = 0u;
        if (six) {
          int fi = hAxis == 1 ? (stepSign < 0.0 ? 0 : 1) : (hAxis == 2 ? (stepSign < 0.0 ? 3 : 2) : (stepSign < 0.0 ? 4 : 5));
          uvec4 i4 = texelFetch(uInfo, ivec2(4, int(m)), 0);
          uvec4 i5 = texelFetch(uInfo, ivec2(5, int(m)), 0);
          uint code = fi < 4 ? i4[fi] : i5[fi - 4];
          layerCode = code & 4095u;
          uv = rotFrac(uv, int(code >> 12));
          uvec4 i6 = texelFetch(uInfo, ivec2(6, int(m)), 0);
          uvec4 i7 = texelFetch(uInfo, ivec2(7, int(m)), 0);
          sixTint = fi < 4 ? i6[fi] : i7[fi - 4];
        }
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
          if (six) tex.rgb *= tintFor(sixTint & 7u, sixTint >> 3, cell);
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
    vec4 rot = uEntRot[i];
    if (rot.w > 0.5) {
      // a model: its parts in the mob's own frame (x its right, z its front), found by turning the ray into it
      vec2 right = rot.xy;
      vec2 front = vec2(rot.y, -rot.x);
      vec3 ro = o - uEntOrg[i];
      vec3 lo = vec3(dot(ro.xz, right), ro.y, dot(ro.xz, front));
      vec3 ld = vec3(dot(dd.xz, right), dd.y, dot(dd.xz, front));
      ld = vec3(abs(ld.x) < 1e-7 ? 1e-7 : ld.x, abs(ld.y) < 1e-7 ? 1e-7 : ld.y, abs(ld.z) < 1e-7 ? 1e-7 : ld.z);
      vec3 linv = 1.0 / ld;
      float pt = bestT;
      int row = -1;
      int pax = 0;
      for (int k = 0; k < 24; k++) {
        if (float(k) >= rot.w) break;
        int r = int(rot.z) + k;
        vec3 p1 = (texelFetch(uParts, ivec2(0, r), 0).xyz - lo) * linv;
        vec3 p2 = (texelFetch(uParts, ivec2(1, r), 0).xyz - lo) * linv;
        vec3 pn = min(p1, p2);
        vec3 pf = max(p1, p2);
        float pin = max(max(pn.x, pn.y), pn.z);
        float pout = min(min(pf.x, pf.y), pf.z);
        if (pin > pout || pout < 0.0) continue;
        pin = max(pin, 0.0);
        if (pin >= pt) continue;
        pt = pin;
        row = r;
        pax = pn.x >= pn.y && pn.x >= pn.z ? 0 : (pn.y >= pn.z ? 1 : 2);
      }
      if (row < 0) continue;
      vec4 a = texelFetch(uParts, ivec2(0, row), 0);
      vec4 b = texelFetch(uParts, ivec2(1, row), 0);
      vec3 f = (lo + ld * pt - a.xyz) / (b.xyz - a.xyz);
      float layer;
      vec2 uv;
      float shade;
      if (pax == 1) {
        bool top = ld.y < 0.0;
        layer = top ? texelFetch(uParts, ivec2(2, row), 0).x : texelFetch(uParts, ivec2(2, row), 0).y;
        uv = top ? vec2(f.x, 1.0 - f.z) : vec2(f.x, f.z);
        shade = top ? 1.0 : 0.5;
      } else if (pax == 2) {
        bool frontFace = ld.z < 0.0;
        layer = frontFace ? texelFetch(uParts, ivec2(2, row), 0).z : texelFetch(uParts, ivec2(2, row), 0).w;
        uv = frontFace ? vec2(1.0 - f.x, 1.0 - f.y) : vec2(f.x, 1.0 - f.y);
        shade = 0.8;
      } else {
        bool rightFace = ld.x < 0.0;
        layer = rightFace ? texelFetch(uParts, ivec2(3, row), 0).x : texelFetch(uParts, ivec2(3, row), 0).y;
        uv = rightFace ? vec2(f.z, 1.0 - f.y) : vec2(1.0 - f.z, 1.0 - f.y);
        shade = 0.62;
      }
      float texels = pt * (2.0 * uHalf / uRes.x) * 16.0 * 2.0;
      vec4 px = textureLod(uTex, vec3(clamp(uv, 0.0, 1.0), layer), clamp(log2(max(texels, 1e-6)), 0.0, uLodMax));
      bestT = pt;
      float pfog = clamp((pt / uDist - 0.6) / 0.4, 0.0, 1.0);
      color = mix((px.a < 0.5 ? uEntCol[i] : px.rgb) * shade * cellColor(ivec3(floor(o + dd * max(pt - 0.01, 0.0)))), sky, pfog);
      continue;
    }
    bestT = te;
    int eax = en.x >= en.y && en.x >= en.z ? 0 : (en.y >= en.z ? 1 : 2);
    float fog = clamp((te / uDist - 0.6) / 0.4, 0.0, 1.0);
    color = mix(uEntCol[i] * shadeOf(eax, d[eax]) * cellColor(ivec3(floor(o + dd * max(te - 0.01, 0.0)))), sky, fog);
  }
  for (int i = 0; i < ${MAX_LABELS}; i++) {
    if (i >= uLabelCount) break;
    vec4 r = uLabelRect[i];
    if (gl_FragCoord.x < r.x || gl_FragCoord.x >= r.z || gl_FragCoord.y < r.y || gl_FragCoord.y >= r.w || uLabelDepth[i] >= bestT) continue;
    vec3 seen = cellColor(ivec3(floor(uLabelAt[i])));
    if (uLabelDist[i] > ${SEE_NEAR}.0 && max(seen.r, max(seen.g, seen.b)) < ${SEEING_MIN}) continue;
    vec2 uv = (gl_FragCoord.xy - r.xy) / (r.zw - r.xy);
    vec4 ink = textureLod(uLabels, vec2(uv.x, (float(i) + 1.0 - uv.y) / ${MAX_LABELS}.0), 0.0);
    color = mix(mix(color, vec3(0.0), 0.6), ink.rgb, ink.a);
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

// The uniforms and the parts table for the entities with models (`model`: {parts: [{box, layers}], right: {x, z}, origin: [x, y, z]}, web/mob-models.mjs):
// rot per entity as (right x, right z, first row, part count) and org its origin, both zero for a plain box, and `rows` the table, 16 floats a part
// (see uParts). A model whose faces are not all layers of the texture array, or that no longer fits the table, is drawn as its plain box.
export const modelUniforms = (entities, layerIndex) => {
  const rot = new Float32Array(MAX_ENTITIES * 4)
  const org = new Float32Array(MAX_ENTITIES * 3)
  const rows = []
  entities.slice(0, MAX_ENTITIES).forEach((e, i) => {
    const layers = e.model?.parts.map(p => p.layers.map(l => layerIndex.get(l) ?? -1))
    if (!layers || layers.some(l => l.includes(-1)) || rows.length / 16 + layers.length > MAX_PART_ROWS) return
    rot.set([e.model.right.x, e.model.right.z, rows.length / 16, layers.length], i * 4)
    org.set(e.model.origin, i * 3)
    e.model.parts.forEach((p, k) => rows.push(...p.box.slice(0, 3), 0, ...p.box.slice(3), p.paint, ...layers[k], 0, 0))
  })
  return { rot, org, rows: Float32Array.from(rows) }
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

// RGBA16UI, INFO_TEXELS texels per material: (top+1, side+1, bottom+1, kind), (flags, emit), box min, box max (1/16 units),
// (up, down, north, south), (east, west, elemOffset, elemCount), (tint up..south), (tint east, west). A tint is group + 8 * constant index. A six-face cube (flag 128) has tex6 layers; each face is
// (layer + 1) | quarter turns << 12. A model has elemOffset / elemCount into the element table.
export const materialInfo = materials => {
  const data = new Uint16Array(materials.length * INFO_TEXELS * 4)
  const faceCode = (m, i) => (m.tex6 ? (m.tex6[i] + 1) | ((m.rot6?.[i] ?? 0) << 12) : 0)
  const tints = m => [...(m.tint6 ?? [0, 0, 0, 0, 0, 0]), 0, 0]
  materials.forEach((m, row) => {
    const [x0, y0, z0, x1, y1, z1] = m.box ?? [0, 0, 0, 16, 16, 16]
    const faces = [0, 1, 2, 3, 4, 5].map(i => faceCode(m, i))
    data.set([...(m.tex ?? [-1, -1, -1]).map(l => l + 1), KINDS[m.kind] ?? 0, (m.flags ?? 0) | (m.issue ? ISSUE_FLAG : 0), m.emit ?? 0, 0, 0, x0, y0, z0, 0, x1, y1, z1, 0, ...faces.slice(0, 4), faces[4], faces[5], m.elemOffset ?? 0, m.elemCount ?? 0, ...tints(m).slice(0, 4), tints(m)[4], tints(m)[5], 0, 0], row * INFO_TEXELS * 4)
  })
  return data
}

// the level-major texture bytes as one subarray per mip level
export const textureLevels = (bytes, layers, size, levels) => {
  const sizes = Array.from({ length: levels }, (_, l) => (size >> l) ** 2 * 4 * layers)
  return sizes.map((n, l) => bytes.subarray(sizes.slice(0, l).reduce((a, b) => a + b, 0), sizes.slice(0, l + 1).reduce((a, b) => a + b, 0)))
}

// ---- name labels over mobs (the shader draws the plate; the text comes from a small atlas, one row per label)
const LABEL_ATLAS_WIDTH = 256
const LABEL_ATLAS_ROW = 32

// The entity boxes {min, max, name, label?, kind?} (relative to the window origin, like eye) as the labels placeLabels gives for them.
export const boxLabels = ({ boxes, eye, basis, width, height, dist }) => placeLabels({
  eye,
  project: projectorFor(basis, width, height),
  width,
  height,
  entities: boxes
    .filter(b => b.name !== undefined || b.label !== undefined)
    .map(b => ({ name: b.name, label: b.label, kind: b.kind, x: (b.min[0] + b.max[0]) / 2, y: b.min[1], z: (b.min[2] + b.max[2]) / 2, width: b.max[0] - b.min[0], height: b.max[1] - b.min[1] }))
    .filter(e => Math.hypot(e.x - eye.x, e.y + e.height / 2 - eye.y, e.z - eye.z) <= dist)
})

// The shader's label uniforms for placeLabels' labels in a picture `height` pixels tall: rects as x0, y0, x1, y1 from the bottom
// left, and depths, both padded to MAX_LABELS.
export const labelUniforms = (labels, height) => {
  const rects = new Float32Array(MAX_LABELS * 4)
  const depths = new Float32Array(MAX_LABELS)
  const ats = new Float32Array(MAX_LABELS * 3)
  const dists = new Float32Array(MAX_LABELS)
  labels.slice(0, MAX_LABELS).forEach((l, i) => {
    const w = labelWidth(l.text, l.h)
    rects.set([l.px - w / 2, height - l.py, l.px + w / 2, height - l.py + l.h], i * 4)
    depths[i] = l.depth
    ats.set(l.at, i * 3)
    dists[i] = l.dist
  })
  return { count: Math.min(labels.length, MAX_LABELS), rects, depths, ats, dists }
}

// RGBA text for the shader: row i holds label i's text in white (alpha is the ink), drawn so that the row, stretched over the
// label's plate, shows the letters in their natural shape. Needs a 2D canvas, so browser only.
const labelAtlas = labels => {
  const canvas = typeof OffscreenCanvas === 'function' ? new OffscreenCanvas(LABEL_ATLAS_WIDTH, LABEL_ATLAS_ROW * MAX_LABELS) : Object.assign(document.createElement('canvas'), { width: LABEL_ATLAS_WIDTH, height: LABEL_ATLAS_ROW * MAX_LABELS })
  const ctx = canvas.getContext('2d')
  ctx.fillStyle = '#fff'
  ctx.textBaseline = 'middle'
  ctx.textAlign = 'center'
  ctx.font = `bold ${Math.round(LABEL_ATLAS_ROW * 0.72)}px monospace`
  labels.slice(0, MAX_LABELS).forEach((l, i) => {
    // the row is stretched over the plate, so the text is squeezed here to fill the plate's text cells and come out in natural shape
    const text = l.text.toUpperCase()
    const cells = text.length * 6 - 1
    ctx.save()
    ctx.translate(LABEL_ATLAS_WIDTH / 2, (i + 0.5) * LABEL_ATLAS_ROW)
    ctx.scale(LABEL_ATLAS_WIDTH * cells / (cells + 2 * LABEL_MARGIN) / ctx.measureText(text).width, 1)
    ctx.fillText(text, 0, 1)
    ctx.restore()
  })
  return ctx.getImageData(0, 0, LABEL_ATLAS_WIDTH, LABEL_ATLAS_ROW * MAX_LABELS)
}

const TABLE_ROW_BYTES = MATERIAL_COLUMNS * 4 + INFO_TEXELS * 8 // RGBA8 colours + RGBA16UI info per material

// One renderer owns the GL context, the program and the tables every world shares (materials, textures, elements, tints,
// debug); a world (createWorld) owns the per-scene block window. canvasOrGl: a canvas (the renderer makes the context) or an
// existing WebGL2 context (the renderer then draws into whatever framebuffer is bound, and never resizes the canvas itself).
export function createRenderer (canvasOrGl) {
  const gl = typeof canvasOrGl.getContext === 'function'
    ? canvasOrGl.getContext('webgl2', { antialias: false, alpha: false, powerPreference: 'high-performance' })
    : canvasOrGl
  if (!gl) throw new Error('WebGL2 is not available')
  const canvas = gl.canvas
  const debugInfo = gl.getExtension('WEBGL_debug_renderer_info')
  const renderer = debugInfo ? gl.getParameter(debugInfo.UNMASKED_RENDERER_WEBGL) : gl.getParameter(gl.RENDERER)
  const program = link(gl)
  const uniform = Object.fromEntries(['uBlocks', 'uCoarse', 'uMats', 'uInfo', 'uElems', 'uElemBase', 'uTintGroups', 'uTintConst', 'uTex', 'uLodMax', 'uRes', 'uEye', 'uFwd', 'uRight', 'uUp', 'uHalf', 'uSize', 'uSlotOff', 'uDist', 'uDarken', 'uDebug', 'uLightTex', 'uBiomes', 'uBiomeColors', 'uHasBiomeColors', 'uEntCount', 'uEntMin', 'uEntMax', 'uEntCol', 'uEntRot', 'uEntOrg', 'uParts', 'uLabelCount', 'uLabelRect', 'uLabelDepth', 'uLabelAt', 'uLabelDist', 'uLabels']
    .map(name => [name, gl.getUniformLocation(program, name)]))
  gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1)
  gl.bindVertexArray(gl.createVertexArray())
  gl.useProgram(program)

  const mats = nearestTexture(gl, gl.TEXTURE_2D, 2)
  const info = nearestTexture(gl, gl.TEXTURE_2D, 3)
  const tex = nearestTexture(gl, gl.TEXTURE_2D_ARRAY, 4)
  const elems = nearestTexture(gl, gl.TEXTURE_2D, 6)
  const labelTexture = nearestTexture(gl, gl.TEXTURE_2D, 9)
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR)
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR)
  const partsTexture = nearestTexture(gl, gl.TEXTURE_2D, PART_UNIT)
  let labelAtlasKey = null // the texts the atlas holds, so it is redrawn and uploaded only when they change
  let elemBase = 0
  let tintGroups = new Float32Array(6 * 3).fill(1)
  let tintConst = new Float32Array(32 * 3).fill(1)
  gl.texParameteri(gl.TEXTURE_2D_ARRAY, gl.TEXTURE_MIN_FILTER, gl.NEAREST_MIPMAP_LINEAR)
  let lodMax = 0
  let debug = false
  let tableVersion = null
  const sharedBytes = { materials: 0, elements: 0, textures: 0 }
  const worlds = new Set()

  // The tables are per Minecraft version and one renderer holds one set: every scene of a renderer must use the same version.
  // Returns false (and logs) when a different version was claimed before.
  const claimTable = version => {
    if (tableVersion === null) tableVersion = version
    if (tableVersion === version) return true
    console.error(`this renderer holds the tables of ${tableVersion}; a scene wants ${version} and is not drawn`)
    return false
  }

  const setMaterials = materials => {
    gl.activeTexture(gl.TEXTURE2)
    gl.bindTexture(gl.TEXTURE_2D, mats)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, MATERIAL_COLUMNS, materials.length, 0, gl.RGBA, gl.UNSIGNED_BYTE, materialPixels(materials))
    gl.activeTexture(gl.TEXTURE3)
    gl.bindTexture(gl.TEXTURE_2D, info)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA16UI, INFO_TEXELS, materials.length, 0, gl.RGBA_INTEGER, gl.UNSIGNED_SHORT, materialInfo(materials))
    sharedBytes.materials = materials.length * TABLE_ROW_BYTES
  }

  // the element table: RGBA32F, width texels per row (tools/view/element-table.mjs); a table with no elements leaves one zero row
  const setElements = ({ data, width, rows, listTexels }) => {
    gl.activeTexture(gl.TEXTURE6)
    gl.bindTexture(gl.TEXTURE_2D, elems)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, width, rows, 0, gl.RGBA, gl.FLOAT, data)
    elemBase = listTexels
    sharedBytes.elements = width * rows * 16
  }
  // the stage-1 tint colours: groups by TINT_GROUP_NAMES, constants by index (both 0..255 RGB)
  const setTints = ({ groups, constants }) => {
    tintGroups = Float32Array.from(TINT_GROUP_NAMES.flatMap(name => groups[name].map(v => v / 255)))
    tintConst = new Float32Array(32 * 3).fill(1)
    tintConst.set(constants.flat().map(v => v / 255))
  }
  gl.activeTexture(gl.TEXTURE6)
  gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, 1024, 1, 0, gl.RGBA, gl.FLOAT, new Float32Array(1024 * 4))

  // once per page: texStorage is immutable
  let layerIndex = new Map() // texture layer name -> layer, for the mobs' model faces
  const setTextures = ({ bytes, layers, size, levels, names = [] }) => {
    layerIndex = new Map(names.map((name, i) => [name, i]))
    gl.activeTexture(gl.TEXTURE4)
    gl.bindTexture(gl.TEXTURE_2D_ARRAY, tex)
    gl.texStorage3D(gl.TEXTURE_2D_ARRAY, levels, gl.RGBA8, size, size, layers)
    textureLevels(bytes, layers, size, levels).forEach((data, l) => {
      const n = size >> l
      gl.texSubImage3D(gl.TEXTURE_2D_ARRAY, l, 0, 0, 0, n, n, layers, gl.RGBA, gl.UNSIGNED_BYTE, data)
    })
    lodMax = levels - 1
    sharedBytes.textures = bytes.byteLength
  }

  // ---- one world: the block window of one scene (its own 3D textures; texture units 0, 1 and 5 are bound to them while drawing) ----
  const createWorld = () => {
    const blocks = nearestTexture(gl, gl.TEXTURE_3D, 0)
    const coarse = nearestTexture(gl, gl.TEXTURE_3D, 1)
    const lightTex = nearestTexture(gl, gl.TEXTURE_3D, 5)
    // Per-world biome data (read by tintFor):
    // biomes is R8UI, (n*4, height/4, n*4), one texel per 4x4x4 blocks, filled by uploadColumn's optional biomes argument;
    // biomeColors is RGBA8 4 x 256 (biome id -> colours), per world because scenes may show different worlds.
    const biomes = nearestTexture(gl, gl.TEXTURE_3D, 7)
    const biomeColors = nearestTexture(gl, gl.TEXTURE_2D, 8)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, 4, 256, 0, gl.RGBA, gl.UNSIGNED_BYTE, new Uint8Array(4 * 256 * 4))
    let size = null
    let hasColors = false

    const bind = () => {
      gl.activeTexture(gl.TEXTURE0)
      gl.bindTexture(gl.TEXTURE_3D, blocks)
      gl.activeTexture(gl.TEXTURE1)
      gl.bindTexture(gl.TEXTURE_3D, coarse)
      gl.activeTexture(gl.TEXTURE5)
      gl.bindTexture(gl.TEXTURE_3D, lightTex)
      gl.activeTexture(gl.TEXTURE7)
      gl.bindTexture(gl.TEXTURE_3D, biomes)
      gl.activeTexture(gl.TEXTURE8)
      gl.bindTexture(gl.TEXTURE_2D, biomeColors)
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
      gl.activeTexture(gl.TEXTURE7)
      gl.bindTexture(gl.TEXTURE_3D, biomes)
      gl.texStorage3D(gl.TEXTURE_3D, 1, gl.R8UI, n * 4, height >> 2, n * 4)
      size = { n, height, sections, noFlags: new Uint8Array(sections) }
      world.bytes = n * n * 256 * height * 3 + n * n * sections + n * n * 16 * (height >> 2) + 4 * 256 * 4
    }

    // mats is 16*height*16 material indices, x fastest then y then z; flags one byte per section, bottom first; light is
    // sky << 4 | block per cell in the same order
    // slot flags: 0 no column (mats and light are stale and never read), 1 a section with blocks, 2 an all-air section
    const putFlags = (sx, sz, flags) => {
      gl.activeTexture(gl.TEXTURE1)
      gl.bindTexture(gl.TEXTURE_3D, coarse)
      gl.texSubImage3D(gl.TEXTURE_3D, 0, sx, 0, sz, 1, size.sections, 1, gl.RED_INTEGER, gl.UNSIGNED_BYTE, flags)
    }
    // biomes (optional): 4 x height/4 x 4 bytes, x fastest then y then z
    const uploadColumn = (sx, sz, mats16, flags, light, biomeIds) => {
      gl.activeTexture(gl.TEXTURE0)
      gl.bindTexture(gl.TEXTURE_3D, blocks)
      gl.texSubImage3D(gl.TEXTURE_3D, 0, sx * 16, 0, sz * 16, 16, size.height, 16, gl.RED_INTEGER, gl.UNSIGNED_SHORT, mats16)
      putFlags(sx, sz, flags.map(f => (f ? 1 : 2)))
      gl.activeTexture(gl.TEXTURE5)
      gl.bindTexture(gl.TEXTURE_3D, lightTex)
      gl.texSubImage3D(gl.TEXTURE_3D, 0, sx * 16, 0, sz * 16, 16, size.height, 16, gl.RED_INTEGER, gl.UNSIGNED_BYTE, light)
      if (!biomeIds) return
      gl.activeTexture(gl.TEXTURE7)
      gl.bindTexture(gl.TEXTURE_3D, biomes)
      gl.texSubImage3D(gl.TEXTURE_3D, 0, sx * 4, 0, sz * 4, 4, size.height >> 2, 4, gl.RED_INTEGER, gl.UNSIGNED_BYTE, biomeIds)
    }
    // colours: 4 x 256 RGBA8 bytes (Uint8Array of 4096)
    const setBiomeColors = colors => {
      gl.activeTexture(gl.TEXTURE8)
      gl.bindTexture(gl.TEXTURE_2D, biomeColors)
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, 4, 256, 0, gl.RGBA, gl.UNSIGNED_BYTE, colors)
      hasColors = true
    }
    // body: GET /biomes/<world>.json. Rows are biome ids, 4 texels each (grass, foliage, dry foliage, water); ids without data and
    // row 255 take the plains row, or the fixed group colours when there is no plains. Without colours the world keeps the fixed colours.
    const setBiomes = body => {
      if (!body?.colors) return false
      const count = body.colors.length / 12
      const fixed = [1, 2, 3, 4].flatMap(g => [...tintGroups.slice(g * 3, g * 3 + 3)].map(v => Math.round(v * 255)))
      const plainsAt = body.names.indexOf('plains')
      const plains = plainsAt >= 0 ? body.colors.slice(plainsAt * 12, plainsAt * 12 + 12) : fixed
      const bytes = new Uint8Array(4 * 256 * 4)
      for (let id = 0; id < 256; id++) {
        const row = id < count && id < 255 && body.names[id] ? body.colors.slice(id * 12, id * 12 + 12) : plains
        for (let t = 0; t < 4; t++) bytes.set([row[t * 3], row[t * 3 + 1], row[t * 3 + 2], 255], (id * 4 + t) * 4)
      }
      setBiomeColors(bytes)
      return true
    }
    // an unloaded slot reads as air with open sky (so loaded neighbours are not shaded black): only its flags are zeroed,
    // the stale blocks and light stay in the textures and every read checks the flag first
    const clearSlot = (sx, sz) => putFlags(sx, sz, size.noFlags)

    const dispose = () => {
      for (const t of [blocks, coarse, lightTex, biomes, biomeColors]) gl.deleteTexture(t)
      size = null
      world.bytes = 0
      worlds.delete(world)
    }

    const world = { allocate, uploadColumn, clearSlot, setBiomeColors, setBiomes, hasBiomeColors: () => hasColors, biomes, biomeColors, dispose, bind, isAllocated: () => size !== null, size: () => size, bytes: 0 }
    worlds.add(world)
    return world
  }

  // sets the canvas size (when this renderer was given a canvas) and the viewport
  const resize = (w, h) => {
    if (canvas && canvas.width !== w) canvas.width = w
    if (canvas && canvas.height !== h) canvas.height = h
    gl.viewport(0, 0, w, h)
  }

  // eye is relative to the window origin; entities are {min, max, color} in the same space; width/height (default: the canvas size)
  // are the viewport drawn at the framebuffer's bottom-left
  const draw = (world, { eye, basis, dist, darken, slotOff, entities, width = canvas.width, height = canvas.height }) => {
    const size = world.size()
    const labels = boxLabels({ boxes: entities, eye, basis, width, height, dist })
    const labelKey = labels.map(l => l.text).join('\n')
    if (labelKey !== labelAtlasKey) {
      labelAtlasKey = labelKey
      gl.activeTexture(gl.TEXTURE9)
      gl.bindTexture(gl.TEXTURE_2D, labelTexture)
      const atlas = labelAtlas(labels)
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, atlas.width, atlas.height, 0, gl.RGBA, gl.UNSIGNED_BYTE, atlas)
    }
    const shown = labelUniforms(labels, height)
    const count = Math.min(entities.length, MAX_ENTITIES)
    const models = modelUniforms(entities, layerIndex)
    gl.activeTexture(gl.TEXTURE0 + PART_UNIT)
    gl.bindTexture(gl.TEXTURE_2D, partsTexture)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, 4, Math.max(1, models.rows.length / 16), 0, gl.RGBA, gl.FLOAT, models.rows.length ? models.rows : new Float32Array(16))
    const flat = key => new Float32Array(MAX_ENTITIES * 3).map((_, i) => (entities[Math.floor(i / 3)]?.[key]?.[i % 3]) ?? 0)
    world.bind()
    gl.viewport(0, 0, width, height)
    gl.uniform1i(uniform.uBlocks, 0)
    gl.uniform1i(uniform.uCoarse, 1)
    gl.uniform1i(uniform.uMats, 2)
    gl.uniform1i(uniform.uInfo, 3)
    gl.uniform1i(uniform.uTex, 4)
    gl.uniform1i(uniform.uElems, 6)
    gl.uniform1i(uniform.uElemBase, elemBase)
    gl.uniform3fv(uniform.uTintGroups, tintGroups)
    gl.uniform3fv(uniform.uTintConst, tintConst)
    gl.uniform1i(uniform.uLightTex, 5)
    gl.uniform1i(uniform.uBiomes, 7)
    gl.uniform1i(uniform.uBiomeColors, 8)
    gl.uniform1i(uniform.uHasBiomeColors, world.hasBiomeColors() ? 1 : 0)
    gl.uniform1f(uniform.uLodMax, lodMax)
    gl.uniform2f(uniform.uRes, width, height)
    gl.uniform3f(uniform.uEye, eye.x, eye.y, eye.z)
    gl.uniform3f(uniform.uFwd, basis.forward.x, basis.forward.y, basis.forward.z)
    gl.uniform3f(uniform.uRight, basis.right.x, basis.right.y, basis.right.z)
    gl.uniform3f(uniform.uUp, basis.up.x, basis.up.y, basis.up.z)
    gl.uniform1f(uniform.uHalf, basis.half)
    gl.uniform3i(uniform.uSize, size.n * 16, size.height, size.n * 16)
    gl.uniform2i(uniform.uSlotOff, slotOff.x, slotOff.z)
    gl.uniform1f(uniform.uDist, dist)
    gl.uniform1f(uniform.uDarken, darken)
    gl.uniform1i(uniform.uDebug, debug ? 1 : 0)
    gl.uniform1i(uniform.uEntCount, count)
    gl.uniform3fv(uniform.uEntMin, flat('min'))
    gl.uniform3fv(uniform.uEntMax, flat('max'))
    gl.uniform3fv(uniform.uEntCol, flat('color'))
    gl.uniform4fv(uniform.uEntRot, models.rot)
    gl.uniform3fv(uniform.uEntOrg, models.org)
    gl.uniform1i(uniform.uParts, PART_UNIT)
    gl.uniform1i(uniform.uLabels, 9)
    gl.uniform1i(uniform.uLabelCount, shown.count)
    gl.uniform4fv(uniform.uLabelRect, shown.rects)
    gl.uniform1fv(uniform.uLabelDepth, shown.depths)
    gl.uniform3fv(uniform.uLabelAt, shown.ats)
    gl.uniform1fv(uniform.uLabelDist, shown.dists)
    gl.drawArrays(gl.TRIANGLES, 0, 3)
  }

  const clear = (r, g, b) => {
    gl.clearColor(r, g, b, 1)
    gl.clear(gl.COLOR_BUFFER_BIT)
  }

  const setDebug = on => { debug = on }
  // bytes of GPU texture memory: the shared tables and each live world (texture storage only; no driver padding or mip overhead guessed)
  const memory = () => {
    const shared = Object.values(sharedBytes).reduce((a, b) => a + b, 0)
    const perWorld = [...worlds].map(w => w.bytes)
    return { shared, sharedParts: { ...sharedBytes }, worlds: perWorld, total: shared + perWorld.reduce((a, b) => a + b, 0) }
  }
  return { gl, renderer, setDebug, setMaterials, setTextures, setElements, setTints, claimTable, createWorld, resize, draw, clear, memory, finish: () => gl.finish() }
}
