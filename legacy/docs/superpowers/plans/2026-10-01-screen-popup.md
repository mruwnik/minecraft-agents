# Screen popup and faster live stream: implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** the look popup shows the HUD and any open container like the game does, and the live stream renders 2-3x faster.

**Architecture:** the renderer (`src/vision/renderer.mjs`) keeps its output byte-identical while dropping per-pixel work; `eyes.mjs` skips the worker when the scene key is unchanged; a new `screen` quick action on the body answers everything the popup draws (`src/body/window-watch.mjs` keeps the open container); the dashboard route `/api/screen/<Name>` proxies it and `index.html` draws it.

**Tech Stack:** node 22 ESM, `node:test`, mineflayer, the dashboard's plain HTML/JS page tested through `vm`.

Spec: `docs/superpowers/specs/2026-10-01-screen-popup-design.md`.

## Global Constraints

- Tests: `node --test test/<file>.test.mjs`; the whole suite `node --test test/*.test.mjs` must stay green.
- The renderer's picture stays byte-identical (`test/vision.test.mjs` pins it with a hash).
- Comments say why, never what. No backward compatibility shims: `inventory slots=true` and `/api/inventory` go away.
- No new npm dependencies. Functional style, early returns, imports at the top.
- Commit each task on branch `screen-popup`; messages end with `Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>`.
- Never start, stop or signal a body or the dashboard; never touch `state/` beyond reading.

---

### Task 1: render without per-pixel allocation, entities tested only inside their screen rectangle

**Files:**
- Modify: `src/vision/renderer.mjs:203-423` (rays, camera, render)
- Test: `test/vision.test.mjs`

**Interfaces:**
- Consumes: nothing new.
- Produces: `render(...)` unchanged signature and output; `castRay(grid, info, o, d, maxDist, accept)` unchanged signature; `directionFor` unchanged.

- [ ] **Step 1: Pin the current picture with a hash, with entities in the scene**

Append to `test/vision.test.mjs` (after the existing render tests, which build `scene`):

```js
import crypto from 'node:crypto'   // at the top of the file with the other imports

// The renderer's picture for this scene, pinned: a change in the pixels is a change on purpose, and updates the hash.
// Entities sit on screen, off screen and behind the eye; the floor's far rows reach the fog.
const pinned = {
  ...scene,
  entities: [
    { name: 'cow', kind: 'passive', x: 0.5, y: -1, z: -2.5, width: 0.9, height: 1.4 },
    { name: 'item', x: 2.5, y: -1, z: -1.5, width: 0.35, height: 0.35 },
    { name: 'zombie', kind: 'hostile', x: 0.5, y: -1, z: 3.5, width: 0.6, height: 1.95 },
    { name: 'sheep', x: -9, y: -1, z: -2, width: 0.9, height: 1.3 }
  ]
}
const sha = img => crypto.createHash('sha256').update(img.rgba).digest('hex').slice(0, 16)
test('render: the pinned view is drawn exactly as before', () => {
  const img = render({ ...pinned, yaw: 0, pitch: 0, width: 64, height: 40, fov: 100, maxDist: 12 })
  assert.deepEqual([sha(img), img.seen.map(e => e.name)], ['REPLACE_ME', ['cow', 'item']])
})
test('render: the pinned panorama is drawn exactly as before', () => {
  const img = render({ ...pinned, panorama: true, width: 96, height: 24, maxDist: 12 })
  assert.deepEqual([sha(img), img.seen.map(e => e.name).sort()], ['REPLACE_ME', ['cow', 'item', 'zombie']])
})
```

- [ ] **Step 2: Run it, read the real hashes from the failure, write them in**

Run: `node --test test/vision.test.mjs 2>&1 | grep -A3 "pinned"`
Expected: both fail with `REPLACE_ME` against a 16-hex-char hash; put those hashes in. (If `seen` differs from the list above, the entity positions need moving so that exactly those are on screen: the cow and item in front, the zombie behind, the sheep off to the left. Adjust and re-run until `seen` matches, then pin the hash.) Run again: PASS.

- [ ] **Step 3: Replace the ray and camera code**

Replace `src/vision/renderer.mjs` from the `// ---- rays` banner through the end of `rayMaker` with:

```js
// ---------------------------------------------------------------- rays
const ENTRY_FACE = { x: ['east', 'west'], y: ['top', 'bottom'], z: ['south', 'north'] } // [moving negative, moving positive]

// Nearest intersection of a ray with an axis-aligned box, into `out` ({t, face}); false when it misses. Scalars only:
// a frame asks this a few hundred thousand times and each returned object was a share of the frame.
function rayBox (o, d, x1, y1, z1, x2, y2, z2, out) {
  let tNear = -Infinity
  let tFar = Infinity
  let axis = 'x'
  if (d.x === 0) { if (o.x < x1 || o.x > x2) return false } else {
    const t1 = (x1 - o.x) / d.x
    const t2 = (x2 - o.x) / d.x
    if (Math.min(t1, t2) > tNear) { tNear = Math.min(t1, t2); axis = 'x' }
    tFar = Math.min(tFar, Math.max(t1, t2))
  }
  if (d.y === 0) { if (o.y < y1 || o.y > y2) return false } else {
    const t1 = (y1 - o.y) / d.y
    const t2 = (y2 - o.y) / d.y
    if (Math.min(t1, t2) > tNear) { tNear = Math.min(t1, t2); axis = 'y' }
    tFar = Math.min(tFar, Math.max(t1, t2))
  }
  if (d.z === 0) { if (o.z < z1 || o.z > z2) return false } else {
    const t1 = (z1 - o.z) / d.z
    const t2 = (z2 - o.z) / d.z
    if (Math.min(t1, t2) > tNear) { tNear = Math.min(t1, t2); axis = 'z' }
    tFar = Math.min(tFar, Math.max(t1, t2))
  }
  if (tNear > tFar || tNear < 0) return false
  out.t = tNear
  out.face = ENTRY_FACE[axis][d[axis] > 0 ? 1 : 0]
  return true
}

// where a ray meets one of the two diagonal quads plants are drawn on (a*px + b*pz = c), or -1
const crossPlane = (o, d, x, y, z, a, b, c) => {
  const denom = a * d.x + b * d.z
  if (denom === 0) return -1
  const t = (c - a * o.x - b * o.z) / denom
  const lx = o.x + d.x * t - x
  const ly = o.y + d.y * t - y
  const lz = o.z + d.z * t - z
  return t >= 0 && lx >= 0 && lx <= 1 && ly >= 0 && ly <= 1 && lz >= 0 && lz <= 1 ? t : -1
}

const clamp01 = v => Math.min(1, Math.max(0, v))

// a hit, with the texel it lands on; `image` is filled in by the renderer's accept
const hitAt = (x, y, z, id, block, face, o, d, t) => {
  const lx = o.x + d.x * t - x
  const ly = o.y + d.y * t - y
  const lz = o.z + d.z * t - z
  const top = face === 'top' || face === 'bottom'
  return { x, y, z, id, block, t, face, u: top ? lx : face === 'north' || face === 'south' ? lx : lz, v: top ? lz : 1 - ly, image: null }
}

const boxHit = { t: 0, face: '' }
// the nearest of a block's boxes past `after` that accept() takes, trying them in order of distance without sorting
const nearestBoxHit = (block, x, y, z, id, o, d, maxDist, accept) => {
  let after = -1
  for (;;) {
    let bestT = Infinity
    let bestFace = null
    for (const b of block.boxes) {
      if (!rayBox(o, d, x + clamp01(b[0]), y + clamp01(b[1]), z + clamp01(b[2]), x + clamp01(b[3]), y + clamp01(b[4]), z + clamp01(b[5]), boxHit)) continue
      if (boxHit.t > after && boxHit.t < bestT) { bestT = boxHit.t; bestFace = boxHit.face }
    }
    if (bestFace === null || bestT > maxDist) return null
    const hit = hitAt(x, y, z, id, block, bestFace, o, d, bestT)
    if (accept(hit)) return hit
    after = bestT
  }
}

const nearestCrossHit = (block, x, y, z, id, o, d, maxDist, accept) => {
  const t1 = crossPlane(o, d, x, y, z, 1, -1, x - z)
  const t2 = crossPlane(o, d, x, y, z, 1, 1, x + z + 1)
  const first = t1 >= 0 && (t2 < 0 || t1 <= t2) ? t1 : t2
  const second = first === t1 ? t2 : t1
  for (const t of [first, second]) {
    if (t < 0 || t > maxDist) continue
    const hit = hitAt(x, y, z, id, block, 'cross', o, d, t)
    if (accept(hit)) return hit
  }
  return null
}

// Walk the voxels along a ray (Amanatides & Woo) until something solid is hit. `info(stateId)` describes a block:
// null for air, {kind:'cube'}, {kind:'boxes', boxes} or {kind:'cross'}. `accept(hit)` can reject see-through texels.
// Every pixel walks a hundred-odd cells, so the walk keeps to scalars: an array or two per step was most of a frame.
export function castRay (grid, info, o, d, maxDist, accept = () => true) {
  const { data, origin, size } = grid
  let x = Math.floor(o.x)
  let y = Math.floor(o.y)
  let z = Math.floor(o.z)
  const sx = Math.sign(d.x)
  const sy = Math.sign(d.y)
  const sz = Math.sign(d.z)
  const dx = d.x === 0 ? Infinity : Math.abs(1 / d.x)
  const dy = d.y === 0 ? Infinity : Math.abs(1 / d.y)
  const dz = d.z === 0 ? Infinity : Math.abs(1 / d.z)
  let nx = d.x === 0 ? Infinity : ((d.x > 0 ? x + 1 : x) - o.x) / d.x
  let ny = d.y === 0 ? Infinity : ((d.y > 0 ? y + 1 : y) - o.y) / d.y
  let nz = d.z === 0 ? Infinity : ((d.z > 0 ? z + 1 : z) - o.z) / d.z
  let t = 0
  let entered = null
  while (t <= maxDist) {
    const lx = x - origin.x
    const ly = y - origin.y
    const lz = z - origin.z
    const inside = lx >= 0 && ly >= 0 && lz >= 0 && lx < size.x && ly < size.y && lz < size.z
    // past the grid's far side there is nothing left to hit
    if (!inside && ((lx < 0 && sx <= 0) || (lx >= size.x && sx >= 0) || (ly < 0 && sy <= 0) || (ly >= size.y && sy >= 0) || (lz < 0 && sz <= 0) || (lz >= size.z && sz >= 0))) return null
    const id = inside ? data[(ly * size.z + lz) * size.x + lx] : 0
    const block = id ? info(id) : null
    if (block) {
      const hit = block.kind === 'cube'
        ? (entered && t <= maxDist ? hitAt(x, y, z, id, block, entered, o, d, t) : null)
        : block.kind === 'cross'
          ? nearestCrossHit(block, x, y, z, id, o, d, maxDist, accept)
          : nearestBoxHit(block, x, y, z, id, o, d, maxDist, accept)
      if (hit && (block.kind !== 'cube' || accept(hit))) return hit
    }
    if (nx <= ny && nx <= nz) {
      t = nx
      nx += dx
      x += sx
      entered = sx > 0 ? 'west' : 'east'
    } else if (ny <= nz) {
      t = ny
      ny += dy
      y += sy
      entered = sy > 0 ? 'bottom' : 'top'
    } else {
      t = nz
      nz += dz
      z += sz
      entered = sz > 0 ? 'north' : 'south'
    }
  }
  return null
}

// ---------------------------------------------------------------- camera
// mineflayer's convention: yaw 0 faces north (-z) and grows turning left; pitch > 0 looks up.
export const directionFor = (yaw, pitch) => ({
  x: -Math.sin(yaw) * Math.cos(pitch),
  y: Math.sin(pitch),
  z: -Math.cos(yaw) * Math.cos(pitch)
})
const cross = (a, b) => ({ x: a.y * b.z - a.z * b.y, y: a.z * b.x - a.x * b.z, z: a.x * b.y - a.y * b.x })

// ray(px, py, out) writes a pixel's unit direction into `out`. project(p) is the pixel a point lands on, or null when
// the camera cannot say: a point behind a perspective eye, or any point of a panorama (its projection is not linear,
// so a box's corners do not bound its picture)
function cameraFor ({ panorama, yaw = 0, pitch = 0, fov = 90, width, height }) {
  if (panorama) {
    // 360 degrees around, north in the middle, square pixels at the horizon
    const vfov = 2 * Math.PI * height / width
    return {
      ray: (px, py, out) => {
        const d = directionFor((0.5 - (px + 0.5) / width) * 2 * Math.PI, (0.5 - (py + 0.5) / height) * vfov)
        out.x = d.x
        out.y = d.y
        out.z = d.z
      },
      project: () => null
    }
  }
  const forward = directionFor(yaw, pitch)
  const right = directionFor(yaw - Math.PI / 2, 0)
  const up = cross(right, forward)
  const half = Math.tan(fov * Math.PI / 360)
  return {
    ray: (px, py, out) => {
      const sx = ((px + 0.5) / width * 2 - 1) * half
      const sy = (1 - (py + 0.5) / height * 2) * half * height / width
      const x = forward.x + right.x * sx + up.x * sy
      const y = forward.y + right.y * sx + up.y * sy
      const z = forward.z + right.z * sx + up.z * sy
      const len = Math.hypot(x, y, z)
      out.x = x / len
      out.y = y / len
      out.z = z / len
    },
    project: p => {
      const f = p.x * forward.x + p.y * forward.y + p.z * forward.z
      if (f <= 1e-9) return null
      const sx = (p.x * right.x + p.y * right.y + p.z * right.z) / f
      const sy = (p.x * up.x + p.y * up.y + p.z * up.z) / f
      return { px: (sx / half + 1) / 2 * width - 0.5, py: (1 - sy / (half * height / width)) / 2 * height - 0.5 }
    }
  }
}
```

Note the ray formulas keep the original expression order (`* half * height / width`), so the directions are bit-identical.

- [ ] **Step 4: Replace `render`**

Replace from `// ---- render` banner's `export function render` to the end of the file with:

```js
// the pixels an entity's box can cover: its corners projected, a pixel wider for rounding; every pixel when the camera
// cannot project it. null when the whole box is behind the eye, which no ray reaches
const PAD = 1
const screenRect = (project, eye, box, width, height) => {
  let x1 = Infinity
  let y1 = Infinity
  let x2 = -Infinity
  let y2 = -Infinity
  let behind = 0
  for (let i = 0; i < 8; i++) {
    const p = project({ x: box[i & 1 ? 3 : 0] - eye.x, y: box[i & 2 ? 4 : 1] - eye.y, z: box[i & 4 ? 5 : 2] - eye.z })
    if (!p) { behind++; continue }
    x1 = Math.min(x1, p.px)
    y1 = Math.min(y1, p.py)
    x2 = Math.max(x2, p.px)
    y2 = Math.max(y2, p.py)
  }
  if (behind === 8 && project({ x: 1, y: 0, z: 0 }) !== null) return null
  if (behind) return [0, 0, width - 1, height - 1]
  return [Math.max(0, Math.floor(x1) - PAD), Math.max(0, Math.floor(y1) - PAD), Math.min(width - 1, Math.ceil(x2) + PAD), Math.min(height - 1, Math.ceil(y2) + PAD)]
}

// Draw the world. `near` is the fraction of the picture closer than NEAR. `texture(blockName, face, props)` returns {width,height,rgba,tint?} or null; entities are
// {name, kind?, x, y, z, width, height} boxes. Returns {width,height,rgba,seen} where `seen` lists the entities
// that actually ended up on screen (not hidden behind blocks) with the pixel they are centred on.
export function render ({ grid, info, texture, eye, entities = [], timeOfDay, width, height, maxDist = 64, ...camera }) {
  const cam = cameraFor({ ...camera, width, height })
  const light = daylight(timeOfDay)
  const rgba = new Uint8Array(width * height * 4)
  const boxes = entities
    .filter(e => Math.hypot(e.x - eye.x, e.y - eye.y, e.z - eye.z) <= maxDist)
    .map(e => ({ e, box: [e.x - e.width / 2, e.y, e.z - e.width / 2, e.x + e.width / 2, e.y + e.height, e.z + e.width / 2], pixels: 0, sumX: 0, sumY: 0 }))
    .map(b => ({ ...b, rect: screenRect(cam.project, eye, b.box, width, height) }))
    .filter(b => b.rect)
  let nearPixels = 0
  // one block description serves every cell of that state, so its pictures are looked up once a frame, not once a hit
  const pictures = new Map()
  const picture = (block, face) => {
    const key = face === 'top' || face === 'bottom' || face === 'cross' ? face : 'side'
    if (!pictures.has(block)) pictures.set(block, {})
    const faces = pictures.get(block)
    if (!(key in faces)) faces[key] = texture(block.name, key, block.props)
    return faces[key]
  }
  const accept = hit => {
    const image = picture(hit.block, hit.face)
    hit.image = image
    return !image || image.rgba[texel(image, hit.u, hit.v) + 3] >= 128
  }
  const d = { x: 0, y: 0, z: 0 }
  for (let py = 0; py < height; py++) {
    const rowBoxes = boxes.filter(b => py >= b.rect[1] && py <= b.rect[3])
    for (let px = 0; px < width; px++) {
      cam.ray(px, py, d)
      const up = clamp01(d.y)
      const skyR = mix(200, 105, up) * light
      const skyG = mix(222, 160, up) * light
      const skyB = mix(255, 250, up) * light
      const hit = castRay(grid, info, eye, d, maxDist, accept)
      const limit = hit ? hit.t : maxDist
      let nearest = null
      let nearestT = Infinity
      let nearestFace = null
      for (const b of rowBoxes) {
        if (px < b.rect[0] || px > b.rect[2]) continue
        if (!rayBox(eye, d, b.box[0], b.box[1], b.box[2], b.box[3], b.box[4], b.box[5], boxHit)) continue
        if (boxHit.t < limit && boxHit.t < nearestT) { nearest = b; nearestT = boxHit.t; nearestFace = boxHit.face }
      }
      let r = skyR
      let g = skyG
      let b = skyB
      if (nearest) {
        nearest.pixels++
        nearest.sumX += px
        nearest.sumY += py
        const base = ENTITY_COLORS[nearest.e.name] ?? ENTITY_COLORS[nearest.e.kind] ?? (nearest.e.kind === 'hostile' ? HOSTILE : hashColor(nearest.e.name))
        const shade = FACE_SHADE[nearestFace] * Math.max(light, 0.6)
        r = base[0] * shade
        g = base[1] * shade
        b = base[2] * shade
      } else if (hit) {
        if (hit.t < NEAR) nearPixels++
        const fog = (hit.t / maxDist) ** 2
        const shade = FACE_SHADE[hit.face] * light
        if (hit.image) {
          const at = texel(hit.image, hit.u, hit.v)
          const tint = hit.image.tint
          r = mix(hit.image.rgba[at] * (tint?.[0] ?? 255) / 255 * shade, skyR, fog)
          g = mix(hit.image.rgba[at + 1] * (tint?.[1] ?? 255) / 255 * shade, skyG, fog)
          b = mix(hit.image.rgba[at + 2] * (tint?.[2] ?? 255) / 255 * shade, skyB, fog)
        } else {
          const base = hashColor(hit.block.name ?? String(hit.id))
          r = mix(base[0] * shade, skyR, fog)
          g = mix(base[1] * shade, skyG, fog)
          b = mix(base[2] * shade, skyB, fog)
        }
      }
      const at = (py * width + px) * 4
      rgba[at] = r
      rgba[at + 1] = g
      rgba[at + 2] = b
      rgba[at + 3] = 255
    }
  }
  const seen = boxes.filter(b => b.pixels > 0).map(({ e, pixels, sumX, sumY }) => ({
    name: e.label ?? e.name,
    px: Math.round(sumX / pixels),
    py: Math.round(sumY / pixels),
    dist: Math.round(Math.hypot(e.x - eye.x, e.y + e.height / 2 - eye.y, e.z - eye.z))
  }))
  return { width, height, rgba, seen, near: nearPixels / (width * height) }
}
```

Careful points for byte identity: the old code computed `c * FACE_SHADE[face] * light` as `(c * shade) * light`; the new `shade = FACE_SHADE * light` then `c * shade` rounds differently. **Keep the old order**: write `hit.image.rgba[at] * (tint?.[0] ?? 255) / 255 * FACE_SHADE[hit.face] * light` and for entities `base[0] * FACE_SHADE[nearestFace] * Math.max(light, 0.6)`; drop the `shade` locals. Same for `sky`: old `mix(200, 105, up) * light` is kept as is.

- [ ] **Step 5: Run the vision tests**

Run: `node --test test/vision.test.mjs`
Expected: all pass, including both pinned hashes. If a pinned hash fails, diff pixel by pixel against the old renderer (`git stash` to get it back) and fix the expression order; do not re-pin.

- [ ] **Step 6: Add the behaviour tests the rectangles make possible**

```js
test('render: an entity whose box is wholly behind the eye is neither drawn nor seen', () => {
  const behind = { ...scene, entities: [{ name: 'cow', x: 0.5, y: -1, z: 3.5, width: 0.9, height: 1.4 }] }
  const img = render({ ...behind, yaw: 0, pitch: 0, width: 32, height: 32, fov: 90, maxDist: 12 })
  assert.deepEqual(img.seen, [])
  assert.equal(sha(img), sha(render({ ...scene, yaw: 0, pitch: 0, width: 32, height: 32, fov: 90, maxDist: 12 })))
})
```

Run: `node --test test/vision.test.mjs` → PASS.

- [ ] **Step 7: Measure**

Copy `/private/tmp/claude-501/-Users-dan-code-minecraft-agents/167461b2-4c4c-4911-a0bc-7c4020e1a3f1/scratchpad/bench-render.mjs` to the repo root as `.bench.tmp.mjs`, add 30 `item` entities within 20 blocks of the eye and a ring of `cross` blocks (`wheat`, stateId via `registry.blocksByName.wheat.defaultState`) on `farmland` around the eye, run `node .bench.tmp.mjs 1.21.4 320x180` before (`git stash`) and after. Record both medians in the commit message. Delete `.bench.tmp.mjs`.

- [ ] **Step 8: Commit**

```bash
git add src/vision/renderer.mjs test/vision.test.mjs
git commit -m "render: entities tested only inside their screen rectangle, no allocation per pixel (NN ms -> MM ms a frame)

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 1b: sky rays stop at the world's ceiling; the walk steps its index; textures cached on the block

Measured by the optimisation review on a farm scene (scripts in
`/private/tmp/claude-501/-Users-dan-code-minecraft-agents/167461b2-4c4c-4911-a0bc-7c4020e1a3f1/scratchpad/rv/`, `bench-farm.mjs`
and the variants `ceil`, `fastdda`, `pic`): after Task 1 a frame is ~35 ms; the ceiling takes it to ~29 (pitch -0.25) or ~28
(pitch 0), stepping the index to ~21, the texture cache saves ~3 ms, zlib level 1 saves 2.5 ms for 9 KB more a frame. All keep
the picture byte for byte.

**Files:**
- Modify: `src/vision/eyes.mjs` (`snapshotWorld`, the `blockUpdate` handler, the scene posted to the worker), `src/vision/renderer.mjs` (`castRay`, `render`'s `picture`, `encodePng`)
- Test: `test/vision.test.mjs`

- [ ] **Step 1: Failing test for the ceiling**

```js
test('castRay: above the grid\'s highest block, a ray going up or level hits nothing without walking', () => {
  const grid = { ...world([[0, -2, 0, 1]]), top: -2 }
  let looked = 0
  const counting = id => { looked++; return info(id) }
  assert.equal(castRay(grid, counting, { x: 0.5, y: 0.5, z: 0.5 }, { x: 0, y: 1, z: 0 }, 12), null)
  assert.equal(looked, 0)
  assert.ok(castRay(grid, counting, { x: 0.5, y: 0.5, z: 0.5 }, { x: 0, y: -1, z: 0 }, 12))
})
```

Run → fails (the first ray walks and `looked` is not 0).

- [ ] **Step 2: Implement the ceiling**

`castRay`: `const top = grid.top ?? Infinity` before the loop; at the top of each iteration, before the bounds test: `if (y > top && sy >= 0) return null`. The pinned-hash tests pass with no `top` in their grids (`Infinity` disables it).

`eyes.mjs` `snapshotWorld`: track `let top = -Infinity` and `if (id) { grid.set(...); if (local.y > top) top = local.y }`; return `Object.assign(grid, { top })`. The `blockUpdate` handler: `if (now.stateId && now.position.y > copy.grid.top) copy.grid.top = now.position.y` (a ceiling only rises: a block removed under it leaves rays walking a little further, never wrong). The scene posted to the worker carries `top: world.grid.top`, and `render` passes the grid through as it does now.

- [ ] **Step 3: Step the index**

In `castRay`, compute before the loop the `t` at which the ray leaves the grid (per axis: the distance to the grid face in the direction of travel, `Infinity` when `d[a] === 0`; the minimum of the three) and `const end = Math.min(maxDist, tOut)`; the eye must be inside the grid (`assert` it: throw `new Error('the eye is outside the grid')` when not, which `eyes.mjs` never does since the copy is centred on it). Keep `idx = (ly * size.z + lz) * size.x + lx` and step it by `sx`, `sy * size.x * size.z` or `sz * size.x` in the three branches; drop the `inside` test and the far-side test. `while (t <= end)`. The pinned hashes must still pass; the castRay tests at `test/vision.test.mjs:132-156` all start inside the grid.

- [ ] **Step 4: Textures on the block description, zlib level 1**

`render`: replace the `pictures` Map with `const faces = block.pictures ??= {}` inside `picture` (the worker's descriptions live for its lifetime; a test's `info` objects live for the test). `encodePng`: `zlib.deflateSync(raw, { level: 1 })`, with a comment: the frame is 9 KB bigger and 2.5 ms sooner, and the stream sends ten a second.

- [ ] **Step 5: Run, measure, commit**

`node --test test/vision.test.mjs` → PASS (pinned hashes unchanged). Re-run the Task 1 bench; record the median.

```bash
git add src/vision/eyes.mjs src/vision/renderer.mjs test/vision.test.mjs
git commit -m "render: sky rays stop at the world's ceiling, the walk steps its index, textures stay on the block (MM ms -> KK ms)

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 2: an unchanged scene answers the last frame without the worker

**Files:**
- Modify: `src/vision/eyes.mjs:70-124`
- Test: `test/vision.test.mjs` (there are `makeEyes` tests near the end; follow their stub bot)

**Interfaces:**
- Produces: `export const lookKey = ({ eye, yaw, pitch, timeOfDay, entities, world, width, height, maxDist, panorama, fov }) => string` from `src/vision/eyes.mjs`.

- [ ] **Step 1: Failing tests for the key**

```js
import { makeEyes, lookKey } from '../src/vision/eyes.mjs'   // extend the existing import

const base = { eye: { x: 0.5, y: 65.62, z: 0.5 }, yaw: 0.1, pitch: 0, timeOfDay: 6000, entities: [{ name: 'cow', x: 3, y: 65, z: 2 }], world: '1:0', width: 320, height: 180, maxDist: 64, panorama: false, fov: 100 }
for (const [name, change, same] of [
  ['the same scene', {}, true],
  ['a step of under a sixteenth of a block', { eye: { x: 0.52, y: 65.62, z: 0.5 } }, true],
  ['a turn of under half a degree', { yaw: 0.1 + 0.004 }, true],
  ['a step of a block', { eye: { x: 1.5, y: 65.62, z: 0.5 } }, false],
  ['a turn', { yaw: 0.3 }, false],
  ['a hundred ticks later', { timeOfDay: 6100 }, false],
  ['an entity that moved a block', { entities: [{ name: 'cow', x: 4, y: 65, z: 2 }] }, false],
  ['a block changed', { world: '1:1' }, false],
  ['another size', { width: 480, height: 270 }, false],
  ['a panorama', { panorama: true }, false]
]) test(`lookKey: ${name} ${same ? 'draws nothing new' : 'is a new picture'}`, () => assert.equal(lookKey({ ...base, ...change }) === lookKey(base), same))
```

Run: `node --test test/vision.test.mjs` → fails: `lookKey` is not exported.

- [ ] **Step 2: Implement `lookKey` and the cache in `look`**

In `src/vision/eyes.mjs`, above `makeEyes`:

```js
// Everything a picture depends on, coarse enough that a body standing still answers the same key: the eye to a
// sixteenth of a block, the direction to half a degree, the day to a hundred ticks, entities to a quarter block,
// and the world copy's identity and edit count
export const lookKey = ({ eye, yaw, pitch, timeOfDay, entities, world, width, height, maxDist, panorama, fov }) => [
  Math.round(eye.x * 16), Math.round(eye.y * 16), Math.round(eye.z * 16),
  Math.round(yaw * 360 / Math.PI), Math.round(pitch * 360 / Math.PI),
  Math.floor((timeOfDay ?? 0) / 100),
  world, width, height, maxDist, panorama ? 'pano' : 'view', fov,
  ...entities.map(e => `${e.name}@${Math.round(e.x * 4)},${Math.round(e.y * 4)},${Math.round(e.z * 4)}`)
].join('|')
```

In `makeEyes`: give each world copy an identity and an edit count:

```js
  let copy = null
  let copies = 0
  const forget = () => { copy = null }
  bot.on('chunkColumnLoad', forget)
  bot.on('chunkColumnUnload', forget)
  bot.on('blockUpdate', (old, now) => { if (!copy) return; copy.grid.set(now.position.x, now.position.y, now.position.z, now.stateId); copy.edits++ })
  const worldAround = (eye, radius) => {
    const at = { x: Math.floor(eye.x), y: Math.floor(eye.y), z: Math.floor(eye.z) }
    const fits = copy && copy.world === bot.world && copy.radius === radius && ['x', 'y', 'z'].every(a => Math.abs(at[a] - copy.centre[a]) <= MARGIN)
    if (!fits) copy = { grid: snapshotWorld(at, radius + MARGIN, Math.min(radius, 48) + MARGIN), world: bot.world, radius, centre: at, id: ++copies, edits: 0 }
    return copy
  }
```

In `look`, replace from `const { origin, size, data } = worldAround(eye, maxDist)` through `fs.writeFileSync(file, out.png)` with:

```js
    const entities = visibleEntities()
    const world = worldAround(eye, maxDist)
    const key = lookKey({ eye, yaw, pitch, timeOfDay: bot.time.timeOfDay, entities, world: `${world.id}:${world.edits}`, width, height, maxDist, panorama, fov: a.fov ?? 100 })
    // a body standing still is asked for the same picture ten times a second: the worker draws it once
    const out = last?.key === key ? last.out : await draw({
      grid: { origin: world.grid.origin, size: world.grid.size, data: world.grid.data }, eye: { x: eye.x, y: eye.y, z: eye.z }, entities, timeOfDay: bot.time.timeOfDay,
      width, height, maxDist, panorama, yaw, pitch, fov: a.fov ?? 100
    })
    last = { key, out }
    fs.mkdirSync(snapshotDir, { recursive: true })
    const file = path.join(snapshotDir, a.file ?? `look-${String(++shot).padStart(3, '0')}.png`)
    fs.writeFileSync(file, out.png)
```

with `let last = null` declared beside `let shot = 0`.

- [ ] **Step 3: A test that the worker is asked once for two identical looks**

Look at the existing `makeEyes` tests in `test/vision.test.mjs` (they build a stub bot with `EventEmitter`, a prismarine-chunk column and `registry`). Add one that calls `look()` twice with the bot unchanged and asserts the second answer's `view`/`seen` equal the first and that only one `look-*.png` was written per call but the files are byte-equal; then changes `bot.time.timeOfDay` by 100 and asserts a third look writes a different... no: assert the second look returned within 5 ms (`performance.now()` around it) while the first took longer than that. Keep whichever assertion is deterministic on this machine; the byte-equal files and the fast second answer both are.

Run: `node --test test/vision.test.mjs` → PASS.

- [ ] **Step 4: Commit**

```bash
git add src/vision/eyes.mjs test/vision.test.mjs
git commit -m "look: an unchanged scene answers the last frame, the worker draws only what changed

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 3: the `screen` action, with the open container

**Files:**
- Create: `src/body/window-watch.mjs`
- Modify: `src/lib/inventory.mjs` (add `armorPoints`), `src/bot.mjs:2852-2861` (the `inventory` action; add `screen`), `src/bot.mjs:~290` (wire the watcher), `src/lib/help.mjs:110` (`inventory` args; add `screen`), `src/job-policy.mjs:4` (add `'screen'`)
- Test: create `test/window-watch.test.mjs`; `test/lib.test.mjs` (armorPoints; find the inventory cases there)

**Interfaces:**
- Produces: `watchWindows(bot, { nearest, now }) => () => window | null`; `containerBlocks(type) => RegExp | null`; `armorPoints(attributes) => number`; the body's `screen` action answering `{ hp, food, xp, oxygen, armor, slots, selected, window }` with `window = { type, title, at, size, open, closedAt, slots }`.

- [ ] **Step 1: Failing tests**

`test/window-watch.test.mjs`:

```js
import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { watchWindows, containerBlocks, LINGER_MS } from '../src/body/window-watch.mjs'

const window = (type, slots, inventoryStart, title = '{"translate":"container.chest"}') => Object.assign(new EventEmitter(), { type, title, slots, inventoryStart })
const bread = { name: 'bread', count: 3 }
const setup = () => {
  const bot = new EventEmitter()
  let t = 1000
  const clock = { now: () => t, tick: ms => { t += ms } }
  const open = watchWindows(bot, { nearest: () => ({ x: 96, y: 70, z: -79 }), now: clock.now })
  return { bot, clock, open }
}

test('no window: nothing', () => assert.equal(setup().open(), null))

test('a chest opens: its stacks by container slot, where it stands, how big it is', () => {
  const { bot, open } = setup()
  bot.emit('windowOpen', window('minecraft:generic_9x3', [null, bread, ...Array(25).fill(null), { name: 'dirt', count: 64 }, ...Array(36).fill({ name: 'mine', count: 1 })], 27))
  assert.deepEqual(open(), { type: 'minecraft:generic_9x3', title: 'chest', at: { x: 96, y: 70, z: -79 }, size: 27, open: true, closedAt: null, slots: [{ slot: 1, name: 'bread', count: 3 }, { slot: 26, name: 'dirt', count: 64 }] })
})

test('a slot changing while it is open changes the answer; one in my own inventory does not', () => {
  const { bot, open } = setup()
  const w = window('minecraft:hopper', [bread, null, null, null, null, ...Array(36).fill(null)], 5)
  bot.emit('windowOpen', w)
  w.slots[0] = null
  w.emit('updateSlot', 0, bread, null)
  assert.deepEqual(open().slots, [])
  w.slots[7] = bread
  w.emit('updateSlot', 7, null, bread)
  assert.deepEqual(open().slots, [])
})

test('closed: kept for LINGER_MS with the time it closed, then gone', () => {
  const { bot, clock, open } = setup()
  bot.emit('windowOpen', window('minecraft:furnace', [null, null, null, ...Array(36).fill(null)], 3))
  clock.tick(300)
  bot.emit('windowClose')
  assert.deepEqual([open().open, open().closedAt], [false, 1300])
  clock.tick(LINGER_MS - 1)
  assert.equal(open().open, false)
  clock.tick(1)
  assert.equal(open(), null)
})

for (const [type, name, matches] of [
  ['minecraft:generic_9x3', 'chest', true], ['minecraft:generic_9x3', 'barrel', true], ['minecraft:generic_9x3', 'red_shulker_box', true],
  ['minecraft:generic_9x6', 'chest', true], ['minecraft:generic_9x6', 'barrel', false], ['minecraft:furnace', 'furnace', true],
  ['minecraft:blast_furnace', 'furnace', false], ['minecraft:smoker', 'smoker', true], ['minecraft:hopper', 'hopper', true],
  ['minecraft:generic_3x3', 'dropper', true], ['minecraft:crafting', 'crafting_table', true]
]) test(`containerBlocks: ${type} ${matches ? 'is' : 'is not'} a ${name}`, () => assert.equal(containerBlocks(type).test(name), matches))
test('containerBlocks: an unknown window has no block', () => assert.equal(containerBlocks('minecraft:beacon'), null))
```

In `test/lib.test.mjs`, beside the inventory cases:

```js
import { armorPoints } from '../src/lib/inventory.mjs'   // extend the existing import
for (const [name, attributes, expected] of [
  ['nothing sent yet', {}, 0],
  ['bare', { 'minecraft:armor': { value: 0, modifiers: [] } }, 0],
  ['an iron helmet and boots as additive modifiers', { 'minecraft:armor': { value: 0, modifiers: [{ amount: 2, operation: 0 }, { amount: 2, operation: 0 }] } }, 4],
  ['the older key', { 'generic.armor': { value: 1, modifiers: [{ amount: 5, operation: 0 }] } }, 6],
  ['toughness is not armour', { 'minecraft:armor_toughness': { value: 2, modifiers: [] }, 'minecraft:armor': { value: 3, modifiers: [] } }, 3],
  ['a multiplier modifier is not counted', { 'minecraft:armor': { value: 4, modifiers: [{ amount: 0.5, operation: 1 }] } }, 4]
]) test(`armorPoints: ${name}`, () => assert.equal(armorPoints(attributes), expected))
```

Run: `node --test test/window-watch.test.mjs test/lib.test.mjs` → fail (module/export missing).

- [ ] **Step 2: Implement**

`src/body/window-watch.mjs`:

```js
// The container the body has open, for the dashboard's screen. Kept a moment after it closes: withdraw, deposit and
// smelt open and close a chest inside one call, often under a second, so a one-second poll would never see one otherwise
export const LINGER_MS = 5000

const BLOCKS = {
  'minecraft:generic_9x3': /^(chest|trapped_chest|barrel|[a-z_]*shulker_box)$/,
  'minecraft:generic_9x6': /^(chest|trapped_chest)$/,
  'minecraft:furnace': /^furnace$/,
  'minecraft:blast_furnace': /^blast_furnace$/,
  'minecraft:smoker': /^smoker$/,
  'minecraft:hopper': /^hopper$/,
  'minecraft:generic_3x3': /^(dispenser|dropper)$/,
  'minecraft:crafting': /^crafting_table$/
}
export const containerBlocks = type => BLOCKS[type] ?? null

// the window's title as the game shows it: a chat component, usually {"translate":"container.chest"}
const titleOf = title => {
  try {
    const chat = typeof title === 'string' ? JSON.parse(title) : title
    return chat?.text || chat?.translate?.replace(/^container\./, '') || String(title)
  } catch { return String(title) }
}
const stacks = (slots, size) => slots.slice(0, size).flatMap((item, slot) => item ? [{ slot, name: item.name, count: item.count }] : [])

export function watchWindows (bot, { nearest, now = Date.now }) {
  let current = null
  bot.on('windowOpen', window => {
    const size = window.inventoryStart
    current = { type: window.type, title: titleOf(window.title), at: nearest(window.type), size, open: true, closedAt: null, slots: stacks(window.slots, size) }
    window.on('updateSlot', slot => { if (slot < size) current.slots = stacks(window.slots, size) })
  })
  bot.on('windowClose', () => { if (current) current = { ...current, open: false, closedAt: now() } })
  return () => {
    if (!current) return null
    if (current.open || now() - current.closedAt < LINGER_MS) return current
    current = null
    return null
  }
}
```

`src/lib/inventory.mjs`, after `inventorySlots`:

```js
// the armour the game's HUD shows: the player's armor attribute, base plus the worn pieces' additive modifiers.
// 0 until the server sends it (it comes with the first armour change)
export const armorPoints = (attributes = {}) => {
  const key = Object.keys(attributes).find(k => /(^|[.:])armor$/.test(k))
  if (!key) return 0
  const { value, modifiers = [] } = attributes[key]
  return value + modifiers.filter(m => m.operation === 0).reduce((n, m) => n + m.amount, 0)
}
```

`src/bot.mjs`: import `{ watchWindows, containerBlocks } from './body/window-watch.mjs'` and `armorPoints` beside `inventorySlots`. Declare `let openWindow = () => null` beside `let eyes`. In the `bot.once('spawn', ...)` block after `eyes = makeEyes(...)`:

```js
    openWindow = watchWindows(bot, {
      nearest: type => {
        const blocks = containerBlocks(type)
        const block = blocks && bot.findBlock({ matching: b => blocks.test(b.name), maxDistance: 6 })
        return block ? { x: block.position.x, y: block.position.y, z: block.position.z } : null
      }
    })
```

Replace the `inventory` action:

```js
  inventory () {
    const slot = n => bot.inventory.slots[bot.getEquipmentDestSlot(n)]?.name ?? null
    return {
      items: inventoryCounts(),
      freeSlots: bot.inventory.emptySlotCount(),
      armor: { head: slot('head'), torso: slot('torso'), legs: slot('legs'), feet: slot('feet'), offhand: slot('off-hand') }
    }
  },

  // the dashboard's: what the player's screen shows. Every stack by slot would be dozens of tokens a driver pays for on
  // each ./mc inventory, so they live here and not there
  screen () {
    return {
      hp: Math.round(bot.health),
      food: bot.food,
      xp: bot.experience.level,
      oxygen: bot.oxygenLevel,
      armor: armorPoints(bot.entity.attributes),
      ...inventorySlots(bot.inventory.slots, bot.quickBarSlot),
      window: openWindow()
    }
  },
```

`src/lib/help.mjs:110`: `inventory: { section: 'sense', args: '', doc: 'what I carry, what I wear and how many slots are free' },` and after it `screen: { section: 'sense', args: '', doc: "the dashboard's screen: hp, food, xp, oxygen, armour points, every stack by window slot, the selected hotbar slot and the container I have open (kept 5 s after it closes)" },`.

`src/job-policy.mjs:4`: add `'screen'` after `'inventory'`.

Check `grep -rn "slots: true\|slots=true" src tools test` and remove every remaining caller except the dashboard (Task 4 moves it).

- [ ] **Step 3: Run**

`node --test test/window-watch.test.mjs test/lib.test.mjs test/help.test.mjs test/job-policy.test.mjs 2>&1 | tail -5` (use whichever of the last two exist) → PASS. Then `node tools/check-code.mjs` if it exists → clean.

- [ ] **Step 4: Commit**

```bash
git add src/body/window-watch.mjs src/lib/inventory.mjs src/bot.mjs src/lib/help.mjs src/job-policy.mjs test/window-watch.test.mjs test/lib.test.mjs
git commit -m "screen: one quick action for what the player's screen shows, with the container the body has open

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 4: `/api/screen/<Name>`

**Files:**
- Modify: `tools/dashboard/lib.mjs:105-130` (route), `tools/dashboard.mjs:258-268, 357, 359` and the header comment at lines 8-9, `README.md` (grep `api/inventory`)
- Test: `test/dashboard.test.mjs:229-278` (routes)

- [ ] **Step 1: Failing test**

In the `routes` table replace the three `/api/inventory` rows with:

```js
  ['/api/screen/Chani', { kind: 'screen', name: 'Chani' }],
  ['/api/screen/', { kind: 'unknown' }],
  ['/api/screen/../../etc/passwd', { kind: 'unknown' }],
```

Run: `node --test test/dashboard.test.mjs` → the three fail.

- [ ] **Step 2: Implement**

In `tools/dashboard/lib.mjs` `route`, rename the inventory pattern and kind to `screen` (the regex on the name stays as it was). In `tools/dashboard.mjs`: `serveInventory` becomes

```js
// one body's screen: HUD numbers, inventory slots and the container it has open. `screen` is a quick action
// (src/bot.mjs): it reads the bot's own state, so this never interrupts whatever the body is doing.
const serveScreen = async (res, name) => {
  const agent = agents.find(a => a.name === name)
  if (!agent) return sendJson(res, 404, { error: `no agent folder called ${name}` })
  const r = await ask(agent.apiPort, 'screen', {}, 5000)
  if (!r.ok) return sendJson(res, 503, { error: r.error ?? r.answer?.error ?? 'the body did not answer' })
  return sendJson(res, 200, r.answer)
}
```

handler `screen: (res, query, r) => serveScreen(res, r.name)`, the 404 hint says `/api/screen/<Name>`, the header comment too. `grep -rn "api/inventory" README.md tools docs` and update.

- [ ] **Step 3: Run and commit**

`node --test test/dashboard.test.mjs` → PASS.

```bash
git add tools/dashboard/lib.mjs tools/dashboard.mjs test/dashboard.test.mjs README.md
git commit -m "dashboard: /api/screen/<Name> serves the body's screen

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 5: the popup draws the HUD and the open container

**Files:**
- Modify: `tools/dashboard/index.html` (CSS 63-77, markup 172-182, script 740-810)
- Test: `test/dashboard-look.test.mjs`

**Interfaces:**
- Consumes: `/api/screen/<Name>` → `{ hp, food, xp, oxygen, armor, slots, selected, window }`.

- [ ] **Step 1: Failing tests**

In `test/dashboard-look.test.mjs`: the `page()` fixture's default becomes a screen `{ hp: 20, food: 20, xp: 0, oxygen: 20, armor: 0, slots: [], selected: 0, window: null }` and the fetch stub answers `/api/screen/`; `carrying` gains `hp: 13, food: 7, xp: 5, oxygen: 20, armor: 5, window: null`; every `/api/inventory` expectation becomes `/api/screen`. Add:

```js
const hud = async screen => { const { el } = await shown(screen); return el('lookHud').innerHTML }
const count = (html, cls) => (html.match(new RegExp(`class="${cls}"`, 'g')) ?? []).length
test('hud: 13 hp is six full hearts, a half and three empty; 7 food three full drumsticks, a half and six empty', async () => {
  const html = await hud(carrying)
  assert.deepEqual([count(html, 'heart full'), count(html, 'heart half'), count(html, 'heart empty')], [6, 1, 3])
  assert.deepEqual([count(html, 'food full'), count(html, 'food half'), count(html, 'food empty')], [3, 1, 6])
})
test('hud: armour points show as chestplates only when worn; air bubbles only under water; the xp level as a number', async () => {
  const html = await hud(carrying)
  assert.deepEqual([count(html, 'armor full'), count(html, 'armor half'), count(html, 'bubble full'), count(html, 'bubble empty')], [2, 1, 0, 0])
  assert.match(html, /class="xp">5</)
  const dry = await hud({ ...carrying, armor: 0, oxygen: 20 })
  assert.equal(count(dry, 'armor full') + count(dry, 'armor empty'), 0)
  const wet = await hud({ ...carrying, oxygen: 11 })
  assert.deepEqual([count(wet, 'bubble full'), count(wet, 'bubble half'), count(wet, 'bubble empty')], [5, 1, 4])
})

const chest = { type: 'minecraft:generic_9x3', title: 'chest', at: { x: 96, y: 70, z: -79 }, size: 27, open: true, closedAt: null, slots: [{ slot: 1, name: 'bread', count: 3 }, { slot: 26, name: 'dirt', count: 64 }] }
const windowCells = async window => { const { el } = await shown({ ...carrying, window }); return { cells: cells(el('lookWindow').innerHTML), meta: el('lookWindowMeta').textContent } }
test('container: a chest draws 27 cells in three rows of nine with its stacks, captioned with its place', async () => {
  const { cells: drawn, meta } = await windowCells(chest)
  assert.equal(Object.keys(drawn).length, 27)
  assert.equal(drawn[26].inside, '<img src="/api/icon/dirt" data-item="dirt" alt=""><span class="count">64</span>')
  assert.equal(meta, 'chest at 96,70,-79')
})
test('container: closed a moment ago says so', async () => {
  const { meta } = await windowCells({ ...chest, open: false, closedAt: Date.now() - 3000 })
  assert.equal(meta, 'chest at 96,70,-79 · closed 3 s ago')
})
test('container: none open draws nothing', async () => {
  const { el } = await shown(carrying)
  assert.deepEqual([el('lookWindow').innerHTML, el('lookWindowMeta').textContent], ['', ''])
})
for (const [type, size, slot, place] of [
  ['minecraft:generic_9x6', 54, 53, '6/9'],
  ['minecraft:hopper', 5, 4, '1/5'],
  ['minecraft:furnace', 3, 2, '2/5'],   // input top-left, fuel below it, the output to the right
  ['minecraft:furnace', 3, 1, '3/1'],
  ['minecraft:generic_3x3', 9, 8, '3/3'],
  ['minecraft:crafting', 10, 0, '2/5'],  // the result, right of the 3x3
  ['minecraft:beacon', 1, 0, '1/1']
]) test(`container: ${type} slot ${slot} sits at ${place}`, async () => {
  const { el } = await shown({ ...carrying, window: { ...chest, type, size, slots: [{ slot, name: 'coal', count: 1 }] } })
  assert.match(el('lookWindow').innerHTML, new RegExp(`data-slot="${slot}" style="grid-area:${place}"`))
})
```

The `cells` helper's regex must keep matching the cell markup (`<div class="slot" data-slot="N" style="grid-area:R/C" ...>`).

Run: `node --test test/dashboard-look.test.mjs` → fail.

- [ ] **Step 2: Implement**

Markup inside `#lookCard`, after `<img id="lookBig" ...>`:

```html
    <div id="lookHud"></div>
    <div id="lookWindowMeta" class="dim"></div>
    <div id="lookWindow" class="screen"></div>
    <div id="lookInventory" class="screen"></div>
```

CSS: move the grey-screen rules from `#lookInventory` to `.screen` (keep the inventory's `grid-template-*` on `#lookInventory`); `#lookWindow` gets `grid-template-columns: repeat(var(--columns), 36px)` set inline per window; `.screen:empty { display: none }`. The HUD: `#lookHud { display: grid; grid-template-columns: 1fr auto 1fr; gap: 2px 12px; align-self: center; width: 340px }` with `i.heart`, `i.food`, `i.armor`, `i.bubble` as 9x9 inline-block icons drawn with CSS (a heart from two rounded squares rotated 45°, a drumstick as a rounded rectangle, a chestplate as a rounded square with notch, a bubble as a circle), `.full` in the game's colour (hearts `#f00`, food `#b5651d`, armour `#ddd`, bubbles `#8cf`), `.half` the left half coloured (`linear-gradient` 50%), `.empty` dark grey. Shapes do not need to be exact; counts and classes do.

Script, replacing `slotHtml`/`inventoryHtml`/`loadInventory` and friends:

```js
const cellHtml = (slot, stack, selected, [row, column]) => {
  const inside = stack ? iconHtml(stack.name) + (stack.count > 1 ? `<span class="count">${stack.count}</span>` : '') : ''
  return `<div class="slot${selected ? ' selected' : ''}" data-slot="${slot}" style="grid-area:${row}/${column}"${stack ? ` title="${stack.name} ×${stack.count}"` : ''}>${inside}</div>`
}
const inventoryHtml = screen => {
  const bySlot = new Map(screen.slots.map(s => [s.slot, s]))
  return INVENTORY_SLOTS.map(slot => cellHtml(slot, bySlot.get(slot), slot === 36 + screen.selected, slotPlace(slot))).join('')
}
// where the game draws a container's slots: by window type, [row, column] of #lookWindow's grid and how many columns it has
const rows9 = s => [1 + Math.floor(s / 9), 1 + s % 9]
const windowLayout = type => /hopper$/.test(type) ? { columns: 5, place: s => [1, 1 + s] }
  : /furnace$|smoker$/.test(type) ? { columns: 5, place: s => [[1, 1], [3, 1], [2, 5]][s] }
  : /generic_3x3$/.test(type) ? { columns: 3, place: s => [1 + Math.floor(s / 3), 1 + s % 3] }
  : /crafting$/.test(type) ? { columns: 5, place: s => s === 0 ? [2, 5] : [1 + Math.floor((s - 1) / 3), 1 + (s - 1) % 3] }
  : { columns: 9, place: rows9 }
const windowHtml = w => {
  const { columns, place } = windowLayout(w.type)
  const bySlot = new Map(w.slots.map(s => [s.slot, s]))
  return { columns, html: Array.from({ length: w.size }, (_, slot) => cellHtml(slot, bySlot.get(slot), false, place(slot))).join('') }
}
const windowMeta = w => `${w.title} at ${w.at ? `${w.at.x},${w.at.y},${w.at.z}` : '?'}${w.open ? '' : ` · closed ${Math.round((Date.now() - w.closedAt) / 1000)} s ago`}`
// the HUD as the game draws it: ten icons a row, each full, half or empty
const hudRow = (kind, value) => Array.from({ length: 10 }, (_, i) => `<i class="${kind} ${value >= (i + 1) * 2 ? 'full' : value >= i * 2 + 1 ? 'half' : 'empty'}"></i>`).join('')
const hudHtml = s => [
  `<span>${s.armor > 0 ? hudRow('armor', s.armor) : ''}</span><span class="xp">${s.xp}</span><span>${s.oxygen < 20 ? hudRow('bubble', s.oxygen) : ''}</span>`,
  `<span>${hudRow('heart', s.hp)}</span><span></span><span>${hudRow('food', s.food)}</span>`
].join('')
```

Redraw-when-changed per part (`shown` map keyed by element id):

```js
const shownHtml = {}
const show = (id, html) => {
  if (shownHtml[id] === html) return
  shownHtml[id] = html
  el(id).innerHTML = html
}
const showScreen = screen => {
  show('lookHud', screen ? hudHtml(screen) : '')
  show('lookInventory', screen ? inventoryHtml(screen) : '')
  const w = screen?.window
  const drawn = w ? windowHtml(w) : { columns: 9, html: '' }
  el('lookWindow').style.setProperty('--columns', drawn.columns)
  show('lookWindow', drawn.html)
  show('lookWindowMeta', w ? windowMeta(w) : '')
}
const loadScreen = () => {
  if (!selected) return Promise.resolve()
  return fetch(`/api/screen/${encodeURIComponent(selected)}?t=${Date.now()}`).then(res => (res.ok ? res.json() : null)).then(showScreen).catch(() => showScreen(null))
}
```

(`lookWindowMeta` uses `textContent` in the test: have `show` write `innerHTML` for the grids and `textContent` for the meta; the stub node in the test has both fields, so pick by id: `id === 'lookWindowMeta' ? el(id).textContent = html : el(id).innerHTML = html`.) `liveInventory` → `liveScreen` calling `loadScreen`, the `INVENTORY_MS` timer unchanged. The vm stub in the test needs `style: { setProperty () {} }` on nodes.

- [ ] **Step 3: Run**

`node --test test/dashboard-look.test.mjs` → PASS. Then open the dashboard in a browser only if one is already running (do not start it): not required.

- [ ] **Step 4: Commit**

```bash
git add tools/dashboard/index.html test/dashboard-look.test.mjs
git commit -m "look popup: the HUD and the open container, drawn as the game's screen

Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>"
```

---

### Task 6: whole suite and docs

- [ ] `node --test test/*.test.mjs` → green. `node tools/check-code.mjs` → clean.
- [ ] `grep -rn "inventory slots\|slots=true\|api/inventory" README.md docs src tools test` → nothing stale.
- [ ] The driver-facing docs (`README.md`, `tools/new-agent.mjs` briefing text) mention nothing about `screen` beyond `./mc help`; no change unless they list the dashboard's routes.
