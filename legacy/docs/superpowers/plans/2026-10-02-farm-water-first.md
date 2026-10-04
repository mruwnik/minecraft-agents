# Farm water first Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `farm.maintain` and `farm.build` set up the plan's water before tilling, till only beds that water already hydrates, plant each bed right after its till, and report `farm_needs_water` when the water cannot be set up.

**Architecture:** The layout already places water: a plan's `~` cells are its channels, and `planErrors` (src/lib/plan.mjs) refuses to save a plan with a farmland cell that no `~` hydrates. So no layout change. The pure job list (`farmJobs`, src/lib/jobs.mjs) is re-ordered so channel work (clear, pour, cover) comes before till; a new pure `hydrated(worldAt, cell)` answers the vanilla rule; both composites check it against the live world right before each till and hold the till (and its plant) on a dry bed, collecting those beds into a `farm_needs_water` summary line that the existing `farm_attention` event carries.

**Tech Stack:** Node ESM, `node:test`, fake api in `test/helpers.mjs`.

## Global Constraints

- Vanilla hydration, as the repo already encodes it (src/lib/place.mjs `dryCells`, src/lib/plan.mjs `planErrors`): water (a water block or a waterlogged block: `holdsWater` in src/lib/world.mjs) within 4 on x AND within 4 on z (a 9x9 square), at the bed's own level or one above.
- Idempotent: a re-run with the water in place does no water work (no find_blocks, fill, dig of a channel, pour).
- Events: no new chat. The water failure goes out on the existing `farm_attention` event as `reasons.farm_needs_water`, naming the missing item(s) and the beds still dry.
- A held till is not a sweep failure: harvest, planting of beds that are already farmland, and storage carry on.
- Tests: extend existing files, plain `test(...)` functions, no conditionals in test bodies. Comments describe why only.
- Commits end with `Co-Authored-By: Claude Opus 5.5 (1M context) <noreply@anthropic.com>`. Never `--no-verify`. Never write under `state/`.
- Run `node tools/check-code.mjs` before each commit.

---

### Task 1: Pure judgements: water before till, `hydrated`, the `dry` bare reason

**Files:**
- Modify: `src/lib/jobs.mjs` (JOB_ORDER, new `hydrated` export)
- Modify: `src/lib/farm.mjs` (`BARE_ORDER` gains `'dry'` right after `'unfilled'`)
- Modify: `src/farm/attention.mjs` (`FARM_ISSUE_FIELDS` gains `'farm_needs_water'`, first in the list)
- Test: `test/lib.test.mjs` (extend; find the existing farmJobs / bareLine tests and add beside them)

**Interfaces:**
- Produces: `hydrated(worldAt, { x, y, z }) -> boolean` exported from src/lib/jobs.mjs (and so from src/lib.mjs, which re-exports it). `y` is the bed's ground level (the farmland/dirt block, i.e. a till job's y). `worldAt(x, y, z)` returns a block `{ name, properties }` or null.
- Produces: farmJobs order `['skip', 'clear', 'pour', 'cover', 'till', 'plant', 'place']`, with `sowAsTilled` still putting each bed's plant straight after its till.
- Produces: bareLine accepts why=`'dry'` and sorts it after `unfilled`.

- [ ] **Step 1: Failing tests** in test/lib.test.mjs:

```js
import { hydrated } from '../src/lib.mjs'   // merge into the file's existing import from '../src/lib.mjs'

for (const [name, world, expected] of [
  ['water 4 across on both axes, level', { '4,63,4': 'water' }, true],
  ['water 5 across', { '5,63,0': 'water' }, false],
  ['water one above the bed', { '0,64,3': 'water' }, true],
  ['water one below the bed', { '0,62,1': 'water' }, false],
  ['a waterlogged slab in the channel', { '1,63,0': { name: 'oak_slab', properties: { waterlogged: 'true' } } }, true],
  ['nothing loaded', {}, false]
]) {
  test(`hydrated: ${name}`, () => {
    const at = (x, y, z) => { const b = world[`${x},${y},${z}`]; return typeof b === 'string' ? { name: b, properties: {} } : b ?? null }
    assert.equal(hydrated(at, { x: 0, y: 63, z: 0 }), expected)
  })
}

test('farmJobs: a dry channel is poured and covered before the bed beside it is tilled and sown', () => {
  // build cells with the same helper the neighbouring farmJobs tests use (planCells from ./plan-fixture.mjs, plan 'w~', y=63)
  // world: 0,63,0 dirt (bed), 1,63,0 air (channel already dug), 1,62,0 dirt, everything at y=64 air
  // items: { water_bucket: 1, oak_slab: 1, wheat_seeds: 1 }
  // expected: farmJobs(...).map(j => j.do) deepEqual ['pour', 'cover', 'till', 'plant']
})

test('bareLine: dry beds are said after unfilled ones', () => {
  assert.equal(bareLine([{ why: 'dry', note: 'no water within 4' }, { why: 'unfilled', note: '0,63,0' }]), '2 (unfilled:1 0,63,0; dry:1 no water within 4)')
})
```

Fill in the farmJobs test body with real code using the fixtures the surrounding farmJobs tests already use (read them first; reuse, do not invent a new fixture).

- [ ] **Step 2: Run, watch them fail**: `node --test test/lib.test.mjs 2>&1 | tail -30`. Expected: hydrated is not a function; order ['till', 'pour', 'cover', 'plant'] or similar; bareLine sorts dry last.

- [ ] **Step 3: Implement**

In src/lib/jobs.mjs:

```js
const JOB_ORDER = ['skip', 'clear', 'pour', 'cover', 'till', 'plant', 'place']
```
with a why-comment above it: a bed tilled before its channel holds water dries back to dirt, so the water goes in first.

```js
// farmland stays farmland with water within 4 on x and z, level with it or one up (the rule dryCells and planErrors use)
export const hydrated = (worldAt, { x, y, z }) => [y, y + 1].some(wy => {
  for (let dx = -4; dx <= 4; dx++) for (let dz = -4; dz <= 4; dz++) if (holdsWater(worldAt(x + dx, wy, z + dz))) return true
  return false
})
```
(`holdsWater` is already imported in jobs.mjs.)

In src/lib/farm.mjs: `const BARE_ORDER = ['unfilled', 'dry', 'untilled', 'no seed', 'unreachable', 'water', 'failed']`, and add `dry (no water within 4 yet: the till is held until the channel holds water)` to the word list in the comment block above `NO_HOE`.

In src/farm/attention.mjs: `FARM_ISSUE_FIELDS` starts with `'farm_needs_water'`.

- [ ] **Step 4: Run** `node --test test/lib.test.mjs` and every farm test: `node --test test/farm-*.test.mjs test/wetplan.test.mjs test/cover.test.mjs`. Fix any test whose asserted job order encoded till-before-pour by updating its expectation to the new order (that order is the bug). Report each test you changed and why.

- [ ] **Step 5: Commit** `farm jobs: channels are poured and covered before any bed is tilled; hydrated() for the till gate`.

---

### Task 2: The composites hold a till on a dry bed and report `farm_needs_water`

**Files:**
- Modify: `library/farm/maintain.mjs` (the job loop in `sweep`)
- Modify: `src/build/plan.mjs` (`buildFromPlan`: `tryJob` and `summary`)
- Modify: `AGENT_GUIDE.md` (one sentence in the `farm.maintain` row, line ~190, near "For the channels it fetches its own water")
- Test: `test/farm-water.test.mjs` (extend), and fix fixtures in `test/farm-maintain.test.mjs`, `test/farm-fill.test.mjs`, `test/farm-maintain-stop.test.mjs`, `test/wetplan.test.mjs`, or any other test that tills on a plan with no water within 4 in its fake world: add a water cell to that world (e.g. a `'<x>,63,<z+2>': 'water'` beside the beds, off the plan), never weaken the gate.

**Interfaces:**
- Consumes: `hydrated(worldAt, { x, y, z })` from `../../src/lib.mjs` / `../lib.mjs`; a till job's `{ x, y, z }` is exactly the bed cell to pass.
- Consumes: bareLine why `'dry'`; summary key `farm_needs_water` already in FARM_ISSUE_FIELDS.
- Consumes: `waterShortfall`'s reason string (`NO_BUCKET`, `noWaterLine(range)`, shore lines) already held in the `water` variable in both composites.

Behaviour, both composites:
1. Jobs run in farmJobs order, so every pour and cover has run (or been skipped with its reason) before the first till.
2. Right before a till, if `!hydrated(api.block, job)`: do not till; record the bed as dry; skip that bed's plant job.
3. At the end (and in maintain, also in `sayJobs` so a checkpoint hand-back carries it), when any bed was held dry, set `summary.farm_needs_water` to one line: the reason the water is missing, then the count and first three cells, e.g.
   `no bucket: craft item=bucket (3 iron_ingot); 12 beds held untilled until water is within 4: 2,63,0 3,63,0 4,63,0 +9 more`.
   Reason: the `water` variable when set (no bucket / no water within 32 / no shore); otherwise `the plan's channels near them are still dry` (a pour that failed, or a plan whose water lies elsewhere).
4. maintain: each dry bed also goes into `bare` via `leave(job, 'dry', 'no water within 4')` and into the `untilled` set, so its plant is skipped by the existing `untilled.has` check. build: keep a `dryBeds` Set keyed by `jobGroundKey(job)`; a plant whose `jobGroundKey` is in it returns without acting.
5. A bed that is already farmland gets no till job, so it is planted as before, wet or dry.

- [ ] **Step 1: Failing tests** in test/farm-water.test.mjs, using its `field`, `pockets`, `POND`, `pondFound`, `found`, `NO_BUCKET` helpers and the `sweep` there (which runs maintain). Make the bed dirt so it needs a till: `field('w~', { '0,63,0': 'dirt', '0,64,0': 'air' })` (plus POND where needed). Give `till` and `place` answers that mutate the world (`till: p => { world[key(p)] = 'farmland' }`, place sets `wheat#0` and decrements seed), and `pour` that writes water at y+1 as the existing preserve-channels test does. Return `made.calls` and `made.events` from the sweep (extend the helper rather than duplicating it).

```js
test('farm.maintain: the channel is poured before the bed beside it is tilled, and the bed is sown at once', async () => {
  // items { bucket: 1, oak_slab: 1, wheat_seeds: 1, stone_hoe: 1 }, POND, pondFound
  // assert the order of the work calls filtered to /^(pour|till|place) / is: pour 1,62,0 ; till 0,63,0 ; place (oak_slab cover) and place wheat_seeds at 0,64,0,
  // with the till index > the pour index and the wheat place index === till index + 1
  // assert summary.farm_needs_water === undefined
})

test('farm.maintain: no bucket means no till on the dry bed, and farm_needs_water says why and where', async () => {
  // items { wheat_seeds: 1, stone_hoe: 1 }, no POND
  // assert no call starts with 'till ', world['0,63,0'] === 'dirt'
  // assert summary.farm_needs_water matches NO_BUCKET text and '0,63,0'
  // assert summary.bare matches /dry:1/
  // assert events.some(e => e.type === 'farm_attention' && e.reasons.farm_needs_water)
})

test('farm.maintain: with the channel already holding water a second sweep does no water work', async () => {
  // world: field('w~', { '0,63,0': 'dirt', '0,64,0': 'air', '1,63,0': 'oak_slab#top~' }), items { wheat_seeds: 1, stone_hoe: 1 }
  // assert no call matches /^(find_blocks|fill|pour|dig) /, exactly one 'till 0,63,0', summary.farm_needs_water undefined
})

test('farm.build: no bucket means no till on the dry bed, and farm_needs_water says why', async () => {
  // buildFarm.run(api, { place, partial: true }) with the no-bucket world and items above
  // assert no till call, summary.farm_needs_water matches NO_BUCKET, and a farm_attention event carries it
})
```

Write real bodies; the comments above are the assertions to make. No `if` in test bodies.

- [ ] **Step 2: Run, watch them fail**: `node --test test/farm-water.test.mjs 2>&1 | tail -40`.

- [ ] **Step 3: Implement** in library/farm/maintain.mjs (job loop, after the hoe checks and before `tryJob`):

```js
if (job.do === 'till' && !hydrated(api.block, job)) { untilled.add(bedKey(job)); dryBeds.push(job); leave(job, 'dry', 'no water within 4'); continue }
```
declare `const dryBeds = []` beside `untilled`, and in `sayJobs`:
```js
if (dryBeds.length) summary.farm_needs_water = needsWaterLine(water, dryBeds)
```
Put `needsWaterLine(reason, beds)` in src/lib/jobs.mjs as a pure export (with a Task-1-style unit test in test/lib.test.mjs) so both composites share it:
```js
export const needsWaterLine = (reason, beds) => `${reason ?? "the plan's channels near them are still dry"}; ${beds.length} bed${beds.length === 1 ? '' : 's'} held untilled until water is within 4: ${beds.slice(0, 3).map(b => `${b.x},${b.y},${b.z}`).join(' ')}${beds.length > 3 ? ` +${beds.length - 3} more` : ''}`
```
In src/build/plan.mjs `tryJob` (inside the `farm &&` till/plant block, after the missingGround check): a till on a non-hydrated bed adds to `dryBeds` (array) and a `dryKeys` Set, returns false; a plant whose `jobGroundKey(job)` is in `dryKeys` returns false. In `summary()` add `...(dryBeds.length ? { farm_needs_water: needsWaterLine(water, dryBeds) } : {})`. `water` is declared with `let` later in buildFromPlan than `summary`/`tryJob`; move its declaration (`let water = null`) above them and assign it where it is assigned today.

Ensure `left()` / `unfinished` in both composites does not re-list a held dry till as unfinished: maintain already filters `bareBeds`; in build, filter jobs whose `jobGroundKey` is in `dryKeys` out of `left()`.

AGENT_GUIDE.md, in the farm.maintain row after the sentence about fetching its own water, add one sentence: `The channels are poured and covered before any bed is tilled, a bed with no water within 4 is not tilled at all (bare= says dry:N), and farm_needs_water= (on farm_attention) names what is missing and the beds held; farm.build does the same.`

- [ ] **Step 4: Run** `node --test test/farm-*.test.mjs test/wetplan.test.mjs test/cover.test.mjs test/lib.test.mjs test/routine*.test.mjs`. Fix fixtures that tilled with no water in reach by adding water to their fake world (list each in the report). Then `npm test 2>&1 | tail -40` once; known flaky: oak-access, tree-scaffold, cocoa-routing, terrain-routing, scaffolding-physics, forage-transport, "live birch geometry", "Treebeard oak logs"; gate-waypoints and attribute-protocol fail only in worktrees.

- [ ] **Step 5: Commit** `farm.maintain/farm.build: hold the till on a dry bed and report farm_needs_water`.
