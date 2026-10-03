// WebGL2 side of the browser view: the block window as a 3D texture, a two-level DDA ray marcher in a full-screen
// fragment shader, entity boxes. Coordinates in the shader are relative to the window origin (a chunk-aligned corner).
export const MAX_ENTITIES = 64
export const KINDS = { cube: 0, partial: 1, cross: 2, water: 3, lava: 4 }
const MATERIAL_COLUMNS = 4 // top, side, bottom, kind

const VERTEX = `#version 300 es
void main () {
  vec2 p = vec2(float((gl_VertexID << 1) & 2), float(gl_VertexID & 2));
  gl_Position = vec4(p * 2.0 - 1.0, 0.0, 1.0);
}`

const FRAGMENT = `#version 300 es
precision highp float;
precision highp int;
precision highp usampler3D;
uniform usampler3D uBlocks;
uniform usampler3D uCoarse;
uniform sampler2D uMats;
uniform vec2 uRes;
uniform vec3 uEye;
uniform vec3 uFwd;
uniform vec3 uRight;
uniform vec3 uUp;
uniform float uHalf;
uniform ivec3 uSize;
uniform ivec2 uSlotOff;
uniform float uDist;
uniform float uLight;
uniform int uEntCount;
uniform vec3 uEntMin[${MAX_ENTITIES}];
uniform vec3 uEntMax[${MAX_ENTITIES}];
uniform vec3 uEntCol[${MAX_ENTITIES}];
out vec4 outColor;

const int MAX_STEPS = 2048;

vec3 skyColor (vec3 d) {
  float up = clamp(d.y, 0.0, 1.0);
  return mix(vec3(200.0, 222.0, 255.0), vec3(105.0, 160.0, 250.0), up) / 255.0 * uLight;
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
  vec3 acc = vec3(0.0);
  float trans = 1.0;
  bool prevWater = false;

  for (int i = 0; i < MAX_STEPS; i++) {
    if (!alive) break;
    ivec3 tc = ivec3((cell.x + uSlotOff.x) % uSize.x, cell.y, (cell.z + uSlotOff.y) % uSize.z);
    if (texelFetch(uCoarse, ivec3(tc.x >> 4, tc.y >> 4, tc.z >> 4), 0).r == 0u) {
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
      prevWater = false;
      if (any(lessThan(cell, ivec3(0))) || any(greaterThanEqual(cell, uSize)) || t >= uDist) break;
      continue;
    }
    uint m = texelFetch(uBlocks, tc, 0).r;
    bool water = false;
    if (m != 0u) {
      int kind = int(texelFetch(uMats, ivec2(3, int(m)), 0).r * 255.0 + 0.5);
      float comp = d[axis];
      int row = axis == 1 ? (comp < 0.0 ? 0 : 2) : 1;
      vec4 c = texelFetch(uMats, ivec2(row, int(m)), 0);
      if (kind == 2) {
        vec3 bo = o - vec3(cell);
        vec3 b1 = (vec3(0.25, 0.0, 0.25) - bo) * inv;
        vec3 b2 = (vec3(0.75, 0.8, 0.75) - bo) * inv;
        float bn = max(max(min(b1.x, b2.x), min(b1.y, b2.y)), min(b1.z, b2.z));
        float bf = min(min(max(b1.x, b2.x), max(b1.y, b2.y)), max(b1.z, b2.z));
        if (bn <= bf && bf >= t) {
          hit = true;
          tHit = max(bn, t);
          hitCol = texelFetch(uMats, ivec2(1, int(m)), 0).rgb * 0.95;
          break;
        }
      } else if (kind == 3) {
        water = true;
        if (!prevWater) {
          acc += trans * c.a * c.rgb * shadeOf(axis, comp) * uLight;
          trans *= 1.0 - c.a;
        }
      } else {
        hit = true;
        tHit = t;
        hitCol = c.rgb * shadeOf(axis, comp);
        break;
      }
    }
    prevWater = water;
    if (tMax.x < tMax.y && tMax.x < tMax.z) { axis = 0; t = tMax.x; cell.x += stp.x; tMax.x += tDelta.x; }
    else if (tMax.y < tMax.z) { axis = 1; t = tMax.y; cell.y += stp.y; tMax.y += tDelta.y; }
    else { axis = 2; t = tMax.z; cell.z += stp.z; tMax.z += tDelta.z; }
    if (any(lessThan(cell, ivec3(0))) || any(greaterThanEqual(cell, uSize)) || t >= uDist) break;
  }

  vec3 color = sky;
  float tNearest = hit ? tHit : uDist;
  if (hit) {
    float fog = clamp((tHit / uDist - 0.6) / 0.4, 0.0, 1.0);
    color = mix(hitCol * uLight, sky, fog);
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
    color = mix(uEntCol[i] * shadeOf(eax, d[eax]) * uLight, sky, fog);
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

export function createRenderer (canvas) {
  const gl = canvas.getContext('webgl2', { antialias: false, alpha: false, powerPreference: 'high-performance' })
  if (!gl) throw new Error('WebGL2 is not available')
  const debugInfo = gl.getExtension('WEBGL_debug_renderer_info')
  const renderer = debugInfo ? gl.getParameter(debugInfo.UNMASKED_RENDERER_WEBGL) : gl.getParameter(gl.RENDERER)
  const program = link(gl)
  const uniform = Object.fromEntries(['uBlocks', 'uCoarse', 'uMats', 'uRes', 'uEye', 'uFwd', 'uRight', 'uUp', 'uHalf', 'uSize', 'uSlotOff', 'uDist', 'uLight', 'uEntCount', 'uEntMin', 'uEntMax', 'uEntCol']
    .map(name => [name, gl.getUniformLocation(program, name)]))
  gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1)
  gl.bindVertexArray(gl.createVertexArray())
  gl.useProgram(program)

  const blocks = nearestTexture(gl, gl.TEXTURE_3D, 0)
  const coarse = nearestTexture(gl, gl.TEXTURE_3D, 1)
  const mats = nearestTexture(gl, gl.TEXTURE_2D, 2)
  let world = null

  const setMaterials = materials => {
    gl.activeTexture(gl.TEXTURE2)
    gl.bindTexture(gl.TEXTURE_2D, mats)
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, MATERIAL_COLUMNS, materials.length, 0, gl.RGBA, gl.UNSIGNED_BYTE, materialPixels(materials))
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
    world = { n, height, sections, zeros: new Uint16Array(256 * height), noFlags: new Uint8Array(sections) }
  }

  // mats is 16*height*16 material indices, x fastest then y then z; flags one byte per section, bottom first
  const uploadColumn = (sx, sz, mats16, flags) => {
    gl.activeTexture(gl.TEXTURE0)
    gl.bindTexture(gl.TEXTURE_3D, blocks)
    gl.texSubImage3D(gl.TEXTURE_3D, 0, sx * 16, 0, sz * 16, 16, world.height, 16, gl.RED_INTEGER, gl.UNSIGNED_SHORT, mats16)
    gl.activeTexture(gl.TEXTURE1)
    gl.bindTexture(gl.TEXTURE_3D, coarse)
    gl.texSubImage3D(gl.TEXTURE_3D, 0, sx, 0, sz, 1, world.sections, 1, gl.RED_INTEGER, gl.UNSIGNED_BYTE, flags)
  }
  const clearSlot = (sx, sz) => uploadColumn(sx, sz, world.zeros, world.noFlags)

  const resize = (w, h) => {
    if (canvas.width !== w) canvas.width = w
    if (canvas.height !== h) canvas.height = h
    gl.viewport(0, 0, w, h)
  }

  // eye is relative to the window origin; entities are {min, max, color} in the same space
  const draw = ({ eye, basis, dist, light, slotOff, entities }) => {
    const count = Math.min(entities.length, MAX_ENTITIES)
    const flat = key => new Float32Array(MAX_ENTITIES * 3).map((_, i) => (entities[Math.floor(i / 3)]?.[key]?.[i % 3]) ?? 0)
    gl.uniform1i(uniform.uBlocks, 0)
    gl.uniform1i(uniform.uCoarse, 1)
    gl.uniform1i(uniform.uMats, 2)
    gl.uniform2f(uniform.uRes, canvas.width, canvas.height)
    gl.uniform3f(uniform.uEye, eye.x, eye.y, eye.z)
    gl.uniform3f(uniform.uFwd, basis.forward.x, basis.forward.y, basis.forward.z)
    gl.uniform3f(uniform.uRight, basis.right.x, basis.right.y, basis.right.z)
    gl.uniform3f(uniform.uUp, basis.up.x, basis.up.y, basis.up.z)
    gl.uniform1f(uniform.uHalf, basis.half)
    gl.uniform3i(uniform.uSize, world.n * 16, world.height, world.n * 16)
    gl.uniform2i(uniform.uSlotOff, slotOff.x, slotOff.z)
    gl.uniform1f(uniform.uDist, dist)
    gl.uniform1f(uniform.uLight, light)
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

  return { renderer, setMaterials, allocate, uploadColumn, clearSlot, resize, draw, clear, finish: () => gl.finish(), isAllocated: () => world !== null }
}
