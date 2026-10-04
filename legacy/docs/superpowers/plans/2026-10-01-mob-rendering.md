# Mob rendering: implementation plan

Spec: `docs/superpowers/specs/2026-10-01-mob-rendering-design.md`.

## Global Constraints

- Work only in the worktree `scratchpad/mob-render` on branch `mob-render`; run every command from there. Never merge.
- TDD: write the failing test, run it and watch it fail, implement, run it green, commit. One task, one commit.
- Prefer extending `test/vision.test.mjs` and `test/dashboard-look.test.mjs`. Plain `test()` functions, parametrized
  with arrays, no conditionals in tests, no wall-clock timing assertions.
- No new dependencies. Comments say why, never what. `src/vision/renderer.mjs` stays pure.
- The look reply's `seen` strings keep their format (`cow 12m @px210,140`).
- Do not start or stop a body or the dashboard; do not write under `state/`; never `--no-verify`.
- Known flaky tests (not ours unless they also pass on screen-popup and fail here): oak-access, tree-scaffold,
  cocoa-routing, terrain-routing, scaffolding-physics, forage-transport, "live birch geometry", "Treebeard oak logs".
- Bench: `node <scratchpad>/mob-bench.mjs` (480x270, eight mobs). Before: ~45 ms with mobs, ~42 ms without.

## Task 1: mobs as part silhouettes turned by their yaw

Files: `src/vision/renderer.mjs`, `test/vision.test.mjs`.

Test (after the "an entity whose box is wholly behind the eye" test):

```js
// a 64x64 view north from the scene's eye with fov 90: the pixel a point dx, dy across and up, dz north of the eye lands on
const pixelAt = (dx, dy, dz) => [Math.round((dx / dz + 1) * 32 - 0.5), Math.round((1 - dy / dz) * 32 - 0.5)]
const view64 = { yaw: 0, pitch: 0, width: 64, height: 64, fov: 90, maxDist: 12 }
const bare = render({ ...scene, ...view64 })
const covers = (img, [x, y]) => pixel(img, x, y).join() !== pixel(bare, x, y).join()
// a cow three blocks north, side on: east is yaw -pi/2, west pi/2
const sideOn = yaw => render({ ...scene, ...view64, entities: [{ name: 'cow', x: 0.5, y: -1, z: -2.5, width: 0.9, height: 1.4, yaw }] })
test('render: a cow seen side on stands on legs, with the floor showing between them', () => {
  const img = sideOn(-Math.PI / 2)
  assert.deepEqual([covers(img, pixelAt(-0.11, -1.22, 3)), covers(img, pixelAt(0.3, -1.22, 3))], [false, true])
})
for (const [name, yaw, east, west] of [['east', -Math.PI / 2, true, false], ['west', Math.PI / 2, false, true]]) {
  test(`render: a cow facing ${name} has its head out on that side`, () => {
    const img = sideOn(yaw)
    assert.deepEqual([covers(img, pixelAt(0.8, -0.2, 3)), covers(img, pixelAt(-0.8, -0.2, 3))], [east, west])
  })
}
```

Run `node --test --test-name-pattern="side on|cow facing" test/vision.test.mjs`: the gap test fails (one box covers
it) and both facing tests fail (the box is symmetric and 0.45 wide).

Implementation, in `renderer.mjs` above `render`:

```js
// ---------------------------------------------------------------- mobs
// A mob is drawn as a few boxes in its own frame: x across and z forward in widths, y up in heights, so one table
// serves a chicken and a ravager. The last number picks the palette entry: 0 body, 1 head, 2 limbs.
const FAMILIES = {
  biped: [[-0.42, 0.75, -0.42, 0.42, 1, 0.42, 1], [-0.42, 0.375, -0.21, 0.42, 0.75, 0.21, 0], [-0.83, 0.375, -0.21, -0.42, 0.75, 0.21, 0],
    [0.42, 0.375, -0.21, 0.83, 0.75, 0.21, 0], [-0.42, 0, -0.21, 0, 0.375, 0.21, 2], [0, 0, -0.21, 0.42, 0.375, 0.21, 2]],
  quadruped: [[-0.5, 0.4, -0.8, 0.5, 0.8, 0.55, 0], [-0.33, 0.55, 0.55, 0.33, 1, 0.95, 1], [-0.45, 0, -0.75, -0.15, 0.4, -0.45, 2],
    [0.15, 0, -0.75, 0.45, 0.4, -0.45, 2], [-0.45, 0, 0.2, -0.15, 0.4, 0.5, 2], [0.15, 0, 0.2, 0.45, 0.4, 0.5, 2]],
  creeper: [[-0.42, 0.7, -0.42, 0.42, 1, 0.42, 1], [-0.42, 0.25, -0.25, 0.42, 0.7, 0.25, 0], [-0.42, 0, 0.25, 0, 0.25, 0.6, 2],
    [0, 0, 0.25, 0.42, 0.25, 0.6, 2], [-0.42, 0, -0.6, 0, 0.25, -0.25, 2], [0, 0, -0.6, 0.42, 0.25, -0.25, 2]],
  spider: [[-0.3, 0.25, -0.55, 0.3, 0.8, 0, 0], [-0.2, 0.25, 0, 0.2, 0.65, 0.3, 1], ...[-0.25, -0.1, 0.05, 0.2].map(z => [-0.5, 0.05, z, 0.5, 0.4, z + 0.06, 2])],
  bird: [[-0.5, 0.3, -0.5, 0.5, 0.75, 0.4, 0], [-0.3, 0.6, 0.25, 0.3, 1, 0.65, 1], [-0.3, 0, -0.05, -0.1, 0.3, 0.1, 2], [0.1, 0, -0.05, 0.3, 0.3, 0.1, 2]],
  blob: [[-0.5, 0, -0.5, 0.5, 1, 0.5, 1]]
}
const FAMILY_OF = Object.fromEntries(Object.entries({
  biped: 'player zombie husk drowned skeleton stray bogged parched wither_skeleton villager wandering_trader pillager vindicator evoker illusioner witch piglin piglin_brute zombified_piglin zombie_villager enderman iron_golem snow_golem creaking warden',
  quadruped: 'cow mooshroom pig sheep goat horse donkey mule skeleton_horse zombie_horse llama trader_llama camel camel_husk wolf fox cat ocelot polar_bear panda hoglin zoglin ravager sniffer armadillo turtle',
  creeper: 'creeper',
  spider: 'spider cave_spider',
  bird: 'chicken parrot'
}).flatMap(([family, names]) => names.split(' ').map(name => [name, family])))

// The mob's parts sized to it, the eye turned into its frame (rays are turned per pixel), and the world-space box
// round its turned parts for screenRect. Turning keeps lengths, so a hit's t compares with the terrain's directly.
const mobFor = (e, eye) => {
  const parts = FAMILIES[FAMILY_OF[e.name] ?? (e.height >= 2 * e.width ? 'biped' : 'blob')]
    .map(([x1, y1, z1, x2, y2, z2, paint]) => [x1 * e.width, y1 * e.height, z1 * e.width, x2 * e.width, y2 * e.height, z2 * e.width, paint])
  const hull = [0, 1, 2].map(i => Math.min(...parts.map(p => p[i]))).concat([3, 4, 5].map(i => Math.max(...parts.map(p => p[i]))))
  const yaw = e.yaw ?? 0
  const right = { x: Math.cos(yaw), z: -Math.sin(yaw) }
  const forward = { x: -Math.sin(yaw), z: -Math.cos(yaw) }
  const corners = [[hull[0], hull[2]], [hull[3], hull[2]], [hull[0], hull[5]], [hull[3], hull[5]]]
    .map(([x, z]) => [e.x + x * right.x + z * forward.x, e.z + x * right.z + z * forward.z])
  const ox = eye.x - e.x
  const oz = eye.z - e.z
  return {
    e,
    parts,
    hull,
    right,
    forward,
    eye: { x: ox * right.x + oz * right.z, y: eye.y - e.y, z: ox * forward.x + oz * forward.z },
    box: [Math.min(...corners.map(c => c[0])), e.y + hull[1], Math.min(...corners.map(c => c[1])), Math.max(...corners.map(c => c[0])), e.y + hull[4], Math.max(...corners.map(c => c[1]))],
    pixels: 0,
    sumX: 0,
    sumY: 0
  }
}
```

In `render`, `boxes` becomes `mobs` built with `mobFor(e, eye)` (then `rect` and the filter as before); the per-pixel
loop turns the ray into each candidate's frame, tests the hull, then the parts:

```js
      for (const m of rowMobs) {
        if (px < m.rect[0] || px > m.rect[2]) continue
        local.x = d.x * m.right.x + d.z * m.right.z
        local.y = d.y
        local.z = d.x * m.forward.x + d.z * m.forward.z
        const h = m.hull
        if (!rayBox(m.eye, local, h[0], h[1], h[2], h[3], h[4], h[5], boxHit) || boxHit.t >= limit || boxHit.t >= nearestT) continue
        for (const p of m.parts) {
          if (!rayBox(m.eye, local, p[0], p[1], p[2], p[3], p[4], p[5], boxHit)) continue
          if (boxHit.t < limit && boxHit.t < nearestT) { nearest = m; nearestT = boxHit.t; nearestFace = boxHit.face }
        }
      }
```

with `const local = { x: 0, y: 0, z: 0 }` beside `d`. Colour stays as before for now. Run the new tests green, then the
whole file: the two pinned hashes change on purpose (the cow, sheep and zombie are now silhouettes); put the new hashes
in and check the seen names are unchanged. Run the bench.

Commit: "render: mobs as part silhouettes turned the way they face, not one box".

## Task 2: game colours, and a face that shows which way a mob looks

Files: `src/vision/renderer.mjs`, `test/vision.test.mjs`.

Test:

```js
// the mean colour of the pixels an entity changed, standing three blocks north facing the eye
const entityColour = entity => {
  const img = render({ ...scene, ...view64, entities: [{ x: 0.5, y: -1, z: -2.5, yaw: Math.PI, ...entity }] })
  const changed = []
  for (let y = 0; y < 64; y++) for (let x = 0; x < 64; x++) if (covers(img, [x, y])) changed.push(pixel(img, x, y))
  return [0, 1, 2].map(i => changed.reduce((s, c) => s + c[i], 0) / changed.length)
}
for (const [name, entity, looks] of [
  ['a creeper is green', { name: 'creeper', kind: 'hostile', width: 0.6, height: 1.7 }, ([r, g, b]) => g > r && g > b],
  ['a skeleton is pale', { name: 'skeleton', kind: 'hostile', width: 0.6, height: 1.99 }, c => Math.min(...c) > 120],
  ['a spider is dark', { name: 'spider', kind: 'hostile', width: 1.4, height: 0.9 }, c => Math.max(...c) < 90],
  ['a hostile without colours of its own is red', { name: 'breeze', kind: 'hostile', width: 0.6, height: 1.77 }, ([r, g, b]) => r > 2 * g && r > 2 * b],
  ['a player is magenta', { name: 'player', kind: 'player', width: 0.6, height: 1.8 }, ([r, g, b]) => r > g && b > g]
]) test(`render: ${name}`, () => assert.ok(looks(entityColour(entity)), String(entityColour(entity))))

test('render: a zombie facing the eye shows its face, one facing away the back of its head', () => {
  const zombie = yaw => render({ ...scene, ...view64, entities: [{ name: 'zombie', kind: 'hostile', x: 0.5, y: -1, z: -2.5, width: 0.6, height: 1.95, yaw }] })
  const head = pixelAt(0, 0.7, 2.75)
  assert.notDeepEqual(pixel(zombie(Math.PI), ...head), pixel(zombie(0), ...head))
})
```

(The `test(...)` body inside a loop has no conditional; the `if` is in the helper, which collects pixels.)

Run: the creeper, skeleton, spider and breeze cases fail (hash colours), the face test fails (front and back of the head
are the same colour and shade). Implementation: replace `ENTITY_COLORS` with

```js
// [body, head, limbs, face] in the game's colours; the face is the front of the head
const plain = (c, face = c.map(v => v * 0.55)) => [c, c, c, face]
const PALETTES = {
  player: [[235, 60, 235], [215, 160, 125], [235, 60, 235], [120, 80, 60]],
  zombie: [[40, 150, 155], [95, 150, 80], [65, 60, 160], [40, 70, 40]],
  ...
  item: plain([255, 225, 40], [255, 225, 40])
}
const paletteFor = e => PALETTES[e.name] ?? PALETTES[e.kind] ?? plain(e.kind === 'hostile' ? HOSTILE : hashColor(e.name))
```

(full table in the commit: zombie, husk, drowned, zombie_villager, skeleton, stray, bogged, wither_skeleton, creeper,
spider, cave_spider, enderman, witch, pillager, vindicator, evoker, piglin, zombified_piglin, blaze, slime, cow,
mooshroom, pig, sheep, chicken, horse, wolf, cat, fox, villager, wandering_trader, iron_golem, player, item). `mobFor`
keeps `palette: paletteFor(e)` and the loop remembers the hit part's paint; the colour is
`paint === 1 && face === 'south' ? palette[3] : palette[paint]` ('south' is the front: the ray entered through the
mob's +z face). Green; update the pinned hashes; commit "render: mobs in their game colours, the face on the front of
the head".

## Task 3: `seen` says the kind and the pixel box each entity covers

Files: `src/vision/renderer.mjs`, `test/vision.test.mjs`.

Test:

```js
test('render: seen gives each entity the box of pixels it covers, holding its centre, smaller for a chicken than a cow', () => {
  const at = entity => render({ ...scene, ...view64, entities: [{ x: 0.5, y: -1, z: -2.5, yaw: Math.PI, kind: 'passive', ...entity }] }).seen[0]
  const cow = at({ name: 'cow', width: 0.9, height: 1.4 })
  const chicken = at({ name: 'chicken', width: 0.4, height: 0.7 })
  const area = ({ box: [x1, y1, x2, y2] }) => (x2 - x1 + 1) * (y2 - y1 + 1)
  const holds = ({ px, py, box: [x1, y1, x2, y2] }) => x1 <= px && px <= x2 && y1 <= py && py <= y2
  assert.deepEqual([cow.kind, holds(cow), holds(chicken), area(chicken) < area(cow) / 3], ['passive', true, true, true])
})
```

Implementation: each mob starts with `x1: Infinity, y1: Infinity, x2: -Infinity, y2: -Infinity`; a pixel it wins
widens them; `seen` entries add `kind: e.kind` and `box: [x1, y1, x2, y2]`. Commit "render: seen carries each
entity's kind and the pixels it covers".

## Task 4: eyes pass yaw and size, key the cache on yaw, and answer marks when asked

Files: `src/vision/eyes.mjs`, `test/vision.test.mjs`.

Tests: two rows in the `lookKey` table:

```js
  ['an entity that turned a degree', { entities: [{ name: 'cow', x: 3, y: 65, z: 2, yaw: 0.017 }] }, true],
  ['an entity that turned round', { entities: [{ name: 'cow', x: 3, y: 65, z: 2, yaw: Math.PI }] }, false],
```

and after the cache test:

```js
// a cow without a size of its own (mineflayer gives none for some), three blocks in front of the body
const withCow = () => Object.assign(standingBot(), { entities: { 7: { name: 'cow', type: 'animal', kind: 'Passive mobs', position: new Vec3(0.5, 64, -2.5), yaw: 0 } } })
test('look: marks=true outlines each seen entity in fractions of the picture, the seen strings as they were', async () => {
  const look = eyesFor(withCow())
  const plain = await look({ file: 'a.png' })
  const marked = await look({ file: 'b.png', marks: true })
  const [mark] = marked.marks
  assert.deepEqual([plain.marks, marked.seen, mark.name, mark.kind, mark.dist], [undefined, plain.seen, 'cow', 'animal', plain.seen[0].split(' ')[1].replace('m', '') * 1])
  assert.ok(mark.box.every(v => v >= 0 && v <= 1) && mark.box[0] < mark.box[2] && mark.box[1] < mark.box[3])
})
test("look: an entity without a size takes its registry's, so a cow is not drawn a player's height", async () => {
  const marked = await eyesFor(withCow())({ file: 'a.png', marks: true })
  const [{ box }] = marked.marks
  // 1.4 high three blocks off; a 1.8 default would reach higher up the picture
  assert.ok(box[1] > 0.4, String(box))
})
```

Implementation: `visibleEntities` adds `yaw: e.yaw ?? 0` and sizes fall back to
`bot.registry.entitiesByName?.[e.name]?.width/height`; `lookKey`'s token gains `/${Math.round((e.yaw ?? 0) * 8 / Math.PI)}`;
the reply adds `...(a.marks ? { marks: out.seen.map(...) } : {})` with box `[x1 / width, y1 / height, (x2 + 1) / width,
(y2 + 1) / height]` rounded to three decimals. Commit "look: marks=true for the dashboard; mobs drawn turned and their
own size, the cache keyed on yaw".

## Task 5: the dashboard outlines and labels each mob

Files: `tools/dashboard.mjs`, `tools/dashboard/index.html`, `test/dashboard-look.test.mjs`.

Test:

```js
test('look popup: each frame outlines and labels the entities it shows, and a frame with none clears them', () => {
  const { el, streams } = page()
  el('lookimg').listeners.click()
  const marks = [{ name: 'cow', kind: 'passive', dist: 3, box: [0.25, 0.5, 0.5, 0.75] }, { name: 'zombie', kind: 'hostile', dist: 9, box: [0.6, 0.1, 0.7, 0.4] }]
  streams[0].onmessage({ data: JSON.stringify({ png: 'AAAA', view: 'north pitch 0', blocked: '', seen: ['cow 3m @px1,2'], marks }) })
  const drawn = el('lookBigMarks').innerHTML
  assert.deepEqual([drawn.match(/class="mark/g).length, drawn.includes('left:25%;top:50%;width:25%;height:25%'), drawn.includes('cow 3m'), drawn.includes('mark hostile')], [2, true, true, true])
  streams[0].onmessage({ data: JSON.stringify({ png: 'AAAA', view: 'north pitch 0', blocked: '', seen: [], marks: [] }) })
  assert.equal(el('lookBigMarks').innerHTML, '')
})
```

Implementation: `lookFrame` returns `marks: r.answer.marks ?? []`; `serveLook` and `streamLook` ask with `marks: true`
and `serveLook` adds an `x-look-marks` header. In the page: `lookimg` and `lookBig` each sit in a
`<div class="lookFrame">` with a `<div id="lookimgMarks" class="marks">` / `lookBigMarks` over it; `marksHtml` draws a
`.mark` per mark at percentages with a `<span>name distm</span>` label; `showLook` writes `${img.id}Marks` on every
frame without an error; `loadLook` reads `x-look-marks`; `select` clears both. Commit "dashboard: each mob in the look
picture outlined and labelled with its distance".

## Task 6: docs, bench, full runs

README Vision section and AGENT_GUIDE `look` row: mobs are drawn as simple shapes in their game colours (players
magenta), facing the way they look; `seen` is unchanged. Run the bench, `node --test test/*.test.mjs` twice,
`node tools/check-code.mjs`. Commit "docs: what a look shows of mobs now".
