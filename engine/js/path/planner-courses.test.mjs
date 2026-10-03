// The live tester's courses planned on their fixture terrain: the verdicts and the paths through tight gaps.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { courseSnapshot, laneSnapshot } from './courses.mjs'
import { plan } from './planner.mjs'
import { boxesNear, bodyHits } from './space.mjs'
import { defaultStateTable } from './blocks.mjs'

const planCourse = name => {
  const { snapshot, from, goal } = courseSnapshot(name)
  return plan(snapshot, { from, goal })
}
const verdict = r => `${r.status}${r.reason ? `/${r.reason}` : ''}`

const GAP_Z = 3217 // the shared boundary of the two gap cells of the cocoa wall at x 2880
const POD_REACH = 0.127 // measured live: age 2 pods on both sides leave 3217.0 +- 0.127 for the body centre

const found = [
  'cocoa-a0-both-feet', 'cocoa-a1-both-feet', 'cocoa-a2-both-feet',
  'cocoa-a0-both-feethead', 'cocoa-a1-both-feethead', 'cocoa-a2-both-feethead',
  'cocoa-a2-one-feethead', 'cocoa-a2-both-head', 'cocoa-a2-both-feethead-diag',
  'cocoa-open-a2-both-feethead', 'cocoa-open-a2-one-feethead', 'cocoa-farm-across', 'cocoa-farm-along',
  'checker-we', 'rand50-we', 'rand50-ew', 'target-in-rand50', 'full-walled',
  'fence-diag', 'trap-ceil-top', 'tunnel-stairs'
]
for (const name of found) {
  test(`course ${name}: found`, () => {
    const r = planCourse(name)
    assert.equal(verdict(r), 'found')
    const last = r.path.steps.at(-1)
    assert.ok(Math.hypot(last.x - courseSnapshot(name).goal.x, last.z - courseSnapshot(name).goal.z) <= 1.5)
  })
}

// every step of a path has the body-centre position the executor steers to
test('steps carry px, pz: the cell centre for ordinary cells', () => {
  const r = planCourse('tunnel-stairs')
  r.path.steps.forEach(s => {
    assert.ok(Number.isFinite(s.px) && Number.isFinite(s.pz))
    assert.ok(s.px >= s.x && s.px <= s.x + 1 && s.pz >= s.z && s.pz <= s.z + 1)
  })
  assert.deepEqual([r.path.steps[0].px, r.path.steps[0].pz], [r.path.steps[0].x + 0.5, r.path.steps[0].z + 0.5])
})

// the age 2 cocoa wall (logs at x 2880, gap cells z 3216 and 3217, pods on both sides): the body centre must stay within
// 0.127 of the boundary between the gap cells while it is in the wall's column and on the cell boundaries around it
const narrow = ['cocoa-a2-both-feet', 'cocoa-a2-both-feethead', 'cocoa-a2-both-head', 'cocoa-a2-both-feethead-diag']
for (const name of narrow) {
  test(`course ${name}: the path crosses the wall within ${POD_REACH} of z ${GAP_Z}`, () => {
    const { steps } = planCourse(name).path
    const inWall = steps.filter(s => s.x === 2880)
    assert.ok(inWall.length >= 1)
    inWall.forEach(s => assert.ok(Math.abs(s.pz - GAP_Z) <= POD_REACH + 1e-9, `step ${s.x},${s.z} pz ${s.pz}`))
    const crossings = steps.filter(s => s.cx !== undefined && (s.cx === 2880 || s.cx === 2881))
    assert.ok(crossings.length >= 2)
    crossings.forEach(s => assert.ok(Math.abs(s.cz - GAP_Z) <= POD_REACH + 1e-9, `crossing ${s.cx},${s.cz}`))
  })
}

// a younger pod leaves more room
test('cocoa age 0 pods leave a wider crossing than age 2', () => {
  const widest = name => Math.max(...planCourse(name).path.steps.filter(s => s.x === 2880).map(s => Math.abs(s.pz - GAP_Z)))
  assert.ok(widest('cocoa-a0-both-feethead') <= 0.5)
  assert.ok(widest('cocoa-a2-both-feethead') <= POD_REACH + 1e-9)
})

const oneWide = laneSnapshot('tricky', [
  'fill 2880 161 3209 2880 163 3215 jungle_log',
  'fill 2880 161 3217 2880 163 3223 jungle_log',
  'setblock 2880 161 3216 cocoa[age=2,facing=north]',
  'setblock 2880 162 3216 cocoa[age=2,facing=north]'
])
test('a one-wide gap between logs with an age 2 pod in it: no position fits, no path through', () => {
  const r = plan(oneWide, { from: { x: 2857, y: 161, z: 3216 }, goal: { kind: 'near', x: 2902, y: 161, z: 3216, range: 1 } })
  assert.notEqual(r.status, 'found')
  assert.ok(!(r.path?.steps ?? []).some(s => s.x >= 2880))
})

test('the same one-wide gap with the pod removed is passable', () => {
  const free = laneSnapshot('tricky', [
    'fill 2880 161 3209 2880 163 3215 jungle_log',
    'fill 2880 161 3217 2880 163 3223 jungle_log'
  ])
  const r = plan(free, { from: { x: 2857, y: 161, z: 3216 }, goal: { kind: 'near', x: 2902, y: 161, z: 3216, range: 1 } })
  assert.equal(r.status, 'found')
})

// a bottom trapdoor ceiling leaves 1.0 of clearance: the body cannot walk under it, but a hand opens each one in its way
test('trap-ceil-bottom: 1.0 of clearance under a bottom trapdoor ceiling, found by opening the trapdoors in the way', () => {
  const r = planCourse('trap-ceil-bottom')
  assert.equal(r.status, 'found')
  assert.match(r.path.summary, /opens \d+ trapdoors/)
  assert.ok(r.path.cost.opens >= 10)
})

test('the walk through the gap costs a plain walk plus 0.1 s per tight cell entered', () => {
  const r = planCourse('cocoa-a2-both-feethead')
  const plain = (r.path.steps.length - 1) / 4.317
  // the three cells of the wall's column and the cells on either side of it are tight; diagonals cost more than 1/4.317
  assert.ok(r.path.cost.seconds >= plain + 0.3 - 1e-6)
})

// the start's exact position picks its region inside a tight cell
const gapStart = (z, pz) => ({ x: 2880, y: 161, z, px: 2880.9, pz })
const afterWall = { kind: 'near', x: 2890, y: 161, z: 3217, range: 1 }
const podWall = courseSnapshot('cocoa-a2-both-feethead').snapshot
const starts = [
  ['the south edge of the north gap cell', gapStart(3216, 3216.95)],
  ['the north edge of the south gap cell', gapStart(3217, 3217.05)]
]
for (const [name, from] of starts) {
  test(`starting inside the pod wall, ${name}: found, the first step keeps the start's region`, () => {
    const r = plan(podWall, { from, goal: afterWall })
    assert.equal(r.status, 'found')
    assert.ok(Math.abs(r.path.steps[0].pz - GAP_Z) <= POD_REACH + 1e-9)
  })
}

test('starting in a cell where no position fits is not standable', () => {
  const r = plan(oneWide, { from: { x: 2880, y: 161, z: 3216 }, goal: afterWall })
  assert.deepEqual([r.status, r.reason], ['none', 'start-not-standable'])
})

test('the search reports its tight-cell statistics', () => {
  const { stats } = planCourse('cocoa-a2-both-feethead')
  assert.ok(stats.tightCells >= 3 && stats.masks >= stats.tightMasks && stats.tightMasks > 0 && stats.regions >= stats.tightMasks && stats.maskMs >= 0)
})

// Independent check at a finer grid than the planner's: flood the body-centre positions (1/16 or finer steps, 4-connected,
// every position checked against the collision boxes) and ask whether the far side of the course is reachable.
const table = defaultStateTable()
const reachable = (snapshot, [x0, z0, x1, z1], [sx, sz], [tx, tz], res) => {
  const cache = new Map()
  const boxes = (cx, cz) => {
    const key = cx * 100000 + cz
    if (!cache.has(key)) cache.set(key, boxesNear(snapshot, table, cx, 161, cz, 161, 162.8))
    return cache.get(key)
  }
  const free = (i, j) => !bodyHits(boxes(Math.floor(i / res), Math.floor(j / res)), i / res, j / res)
  const seen = new Set([`${Math.round(sx * res)},${Math.round(sz * res)}`])
  const queue = [[Math.round(sx * res), Math.round(sz * res)]]
  for (let head = 0; head < queue.length; head++) {
    const [i, j] = queue[head]
    if (Math.hypot(i - tx * res, j - tz * res) < res * 0.6) return true
    ;[[1, 0], [-1, 0], [0, 1], [0, -1]].forEach(([di, dj]) => {
      const k = `${i + di},${j + dj}`
      if (i + di < x0 * res || i + di > x1 * res || j + dj < z0 * res || j + dj > z1 * res || seen.has(k) || !free(i + di, j + dj)) return
      seen.add(k)
      queue.push([i + di, j + dj])
    })
  }
  return false
}

// the planner and the finer flood agree on whether the body fits through
const passages = [
  ['fence-diag', [2870, 3205, 2890, 3225], [2879.5, 3217.5], [2882.5, 3216.5], 32, true, 'found'],
  // cobblestone wall posts are 0.5 wide: the pockets either side of the seam do not join for a 0.62 body
  ['wall-diag', [2870, 3205, 2890, 3225], [2879.5, 3217.5], [2882.5, 3216.5], 32, false, 'partial'],
  // a field of bamboo in every cell still leaves a 0.62 passage, thanks to the per-position offsets
  ['full-walled', [2855, 3209, 2905, 3224], [2857.5, 3216.5], [2902.5, 3216.5], 16, true, 'found']
]
for (const [name, area, start, target, res, fits, status] of passages) {
  test(`course ${name}: a body ${fits ? 'fits' : 'does not fit'} through, ${status}`, () => {
    assert.equal(reachable(courseSnapshot(name).snapshot, area, start, target, res), fits)
    assert.equal(planCourse(name).status, status)
  })
}
