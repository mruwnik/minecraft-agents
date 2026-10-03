import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { pickNext, createThumbnailer } from './thumbs.mjs'

const base = { now: 100000, minIntervalMs: 2000, budgetLeftMs: 1000 }
const body = (name, over = {}) => ({ name, interested: true, poseMtimeMs: 500, renderMtimeMs: 400, renderedAt: 90000, ...over })

const pickCases = [
  ['changed mtime renders', [body('A')], base, 'A'],
  ['unchanged mtime skipped', [body('A', { renderMtimeMs: 500 })], base, null],
  ['older cached mtime than pose renders', [body('A', { renderMtimeMs: 100 })], base, 'A'],
  ['interval not elapsed', [body('A', { renderedAt: 99000 })], base, null],
  ['interval exactly elapsed', [body('A', { renderedAt: 98000 })], base, 'A'],
  ['stalest first', [body('A', { renderedAt: 95000 }), body('B', { renderedAt: 80000 }), body('C', { renderedAt: 90000 })], base, 'B'],
  ['never rendered beats rendered', [body('A', { renderedAt: 1000 }), body('B', { renderMtimeMs: null, renderedAt: null })], base, 'B'],
  ['budget exhausted', [body('A')], { ...base, budgetLeftMs: 0 }, null],
  ['budget negative', [body('A')], { ...base, budgetLeftMs: -5 }, null],
  ['uninterested skipped', [body('A', { interested: false })], base, null],
  ['uninterested skipped, other picked', [body('A', { interested: false, renderedAt: 1 }), body('B')], base, 'B'],
  ['no pose file skipped', [body('A', { poseMtimeMs: null })], base, null],
  ['empty', [], base, null]
]
for (const [name, bodies, opts, expected] of pickCases) {
  test(`pickNext: ${name}`, () => assert.equal(pickNext(bodies, opts), expected))
}

const setup = names => {
  const stateDir = fs.mkdtempSync(path.join(os.tmpdir(), 'thumbs-'))
  const pose = n => path.join(stateDir, 'agents', n, 'view', 'pose.json')
  const touch = (n, sec) => {
    fs.mkdirSync(path.dirname(pose(n)), { recursive: true })
    if (!fs.existsSync(pose(n))) fs.writeFileSync(pose(n), '{}')
    fs.utimesSync(pose(n), sec, sec)
  }
  names.forEach(n => touch(n, 1000))
  return { stateDir, touch }
}

const fakeRenderer = ms => {
  const calls = []
  const fn = async name => {
    calls.push(name)
    return { png: Buffer.from(`png-${name}-${calls.length}`), ms }
  }
  fn.calls = calls
  return fn
}

const make = (stateDir, renderer, clock, over = {}) =>
  createThumbnailer({ stateDir, renderer, now: () => clock.t, tickMs: 1e9, minIntervalMs: 2000, budget: 0.25, windowMs: 10000, ...over })

test('get renders once, then serves the cache', async () => {
  const { stateDir } = setup(['A'])
  const r = fakeRenderer(50)
  const clock = { t: 10000 }
  const th = make(stateDir, r, clock)
  const a = await th.get('A')
  const b = await th.get('A')
  th.close()
  assert.equal(a.png.toString(), 'png-A-1')
  assert.equal(b, a)
  assert.equal(r.calls.length, 1)
  assert.equal(a.poseMtimeMs, 1000 * 1000)
  assert.equal(a.renderedAt, 10000)
})

test('get returns null for a body without pose.json', async () => {
  const { stateDir } = setup([])
  const th = make(stateDir, fakeRenderer(5), { t: 0 })
  assert.equal(await th.get('Nobody'), null)
  th.close()
})

test('get returns null when the renderer throws PoseError', async () => {
  const { stateDir } = setup(['A'])
  const th = make(stateDir, async () => { throw Object.assign(new Error('no position'), { name: 'PoseError' }) }, { t: 0 })
  assert.equal(await th.get('A'), null)
  th.close()
})

test('tick skips an unchanged pose', async () => {
  const { stateDir } = setup(['A'])
  const r = fakeRenderer(50)
  const clock = { t: 10000 }
  const th = make(stateDir, r, clock)
  await th.get('A')
  clock.t += 5000
  await th.tick()
  th.close()
  assert.equal(r.calls.length, 1)
})

test('tick re-renders a changed pose and updates the cache', async () => {
  const { stateDir, touch } = setup(['A'])
  const r = fakeRenderer(50)
  const clock = { t: 10000 }
  const th = make(stateDir, r, clock)
  await th.get('A')
  touch('A', 2000)
  clock.t += 500
  await th.tick()
  assert.equal(r.calls.length, 1, 'interval 500 < 2000')
  clock.t += 2000
  await th.tick()
  const again = await th.get('A')
  th.close()
  assert.equal(r.calls.length, 2)
  assert.equal(again.png.toString(), 'png-A-2')
  assert.equal(again.poseMtimeMs, 2000 * 1000)
})

test('tick picks the stalest of several changed bodies, one per tick', async () => {
  const { stateDir, touch } = setup(['A', 'B'])
  const r = fakeRenderer(10)
  const clock = { t: 10000 }
  const th = make(stateDir, r, clock)
  await th.get('B')
  clock.t += 100
  await th.get('A')
  touch('A', 2000)
  touch('B', 2000)
  clock.t += 5000
  await th.tick()
  assert.deepEqual(r.calls.slice(2), ['B'])
  await th.tick()
  assert.deepEqual(r.calls.slice(2), ['B', 'A'])
  th.close()
})

test('tick stops rendering once the budget is spent, and resumes after the window', async () => {
  const { stateDir, touch } = setup(['A'])
  const r = fakeRenderer(3000)
  const clock = { t: 10000 }
  const th = make(stateDir, r, clock, { minIntervalMs: 0 })
  await th.get('A') // 3000 ms busy, budget 2500
  touch('A', 2000)
  clock.t += 100
  await th.tick()
  assert.equal(r.calls.length, 1, 'over budget')
  assert.equal(th.stats().busyMsInWindow, 3000)
  clock.t += 10000
  await th.tick()
  assert.equal(r.calls.length, 2, 'window moved on')
  th.close()
})

test('bodies not requested within interestMs are not re-rendered', async () => {
  const { stateDir, touch } = setup(['A'])
  const r = fakeRenderer(10)
  const clock = { t: 10000 }
  const th = make(stateDir, r, clock, { interestMs: 60000 })
  await th.get('A')
  touch('A', 2000)
  clock.t += 61000
  await th.tick()
  assert.equal(r.calls.length, 1)
  th.close()
})

test('stats reports bodies, window and timings', async () => {
  const { stateDir } = setup(['A', 'B'])
  const clock = { t: 10000 }
  const th = make(stateDir, fakeRenderer(40), clock)
  await Promise.all([th.get('A'), th.get('B')])
  const s = th.stats()
  th.close()
  assert.deepEqual(s, { bodies: 2, rendersInWindow: 2, busyMsInWindow: 80, budgetMs: 2500, lastMs: 40, meanMs: 40, queue: 0 })
})

test('concurrent gets for one body share a render and renders never overlap', async () => {
  const { stateDir } = setup(['A', 'B'])
  let active = 0
  let peak = 0
  const renderer = async name => {
    peak = Math.max(peak, ++active)
    await new Promise(resolve => setTimeout(resolve, 10))
    active--
    return { png: Buffer.from(name), ms: 10 }
  }
  const th = make(stateDir, renderer, { t: 0 })
  await Promise.all([th.get('A'), th.get('A'), th.get('B')])
  th.close()
  assert.equal(peak, 1)
})
