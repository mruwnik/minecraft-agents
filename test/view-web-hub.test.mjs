import { test } from 'node:test'
import assert from 'node:assert/strict'
import { dueScenes, nextDueAt } from '../tools/view/web/hub.mjs'

const entry = (id, dueAt, visible = true) => ({ id, dueAt, visible })

const dueCases = [
  ['only visible scenes whose time has come', [entry('a', 100), entry('b', 100, false), entry('c', 300)], 200, Infinity, ['a']],
  ['longest overdue first', [entry('a', 150), entry('b', 50), entry('c', 100)], 200, Infinity, ['b', 'c', 'a']],
  ['ties keep their order', [entry('a', 10), entry('b', 10), entry('c', 10)], 20, Infinity, ['a', 'b', 'c']],
  ['at most max', [entry('a', 1), entry('b', 2), entry('c', 3)], 10, 2, ['a', 'b']],
  ['due exactly now', [entry('a', 200)], 200, Infinity, ['a']],
  ['nothing', [], 200, Infinity, []]
]
for (const [name, entries, now, max, expected] of dueCases) {
  test(`dueScenes: ${name}`, () => assert.deepEqual(dueScenes(entries, now, max), expected))
}

const nextCases = [
  ['one period after it was due', [1000, 1010, 6], 1000 + 1000 / 6],
  ['a little late still keeps the cadence', [1000, 1100, 6], 1000 + 1000 / 6],
  ['more than a period behind restarts from now', [1000, 1300, 6], 1300 + 1000 / 6]
]
for (const [name, [dueAt, now, fps], expected] of nextCases) {
  test(`nextDueAt: ${name}`, () => assert.ok(Math.abs(nextDueAt(dueAt, now, fps) - expected) < 1e-9))
}

// 60 Hz animation frames for `seconds`; each frame renders what is due, at most `perFrame`; returns the renders per scene
const simulate = ({ scenes, fps, seconds, perFrame, frameMs = 1000 / 60 }) => {
  const state = scenes.map(s => ({ ...s, dueAt: s.start ?? 0, renders: [] }))
  for (let now = 0; now < seconds * 1000; now += frameMs) {
    for (const id of dueScenes(state, now, perFrame)) {
      const s = state.find(e => e.id === id)
      s.renders.push(now)
      s.dueAt = nextDueAt(s.dueAt, now, fps)
    }
  }
  return state
}

test('eleven visible scenes at 6 fps each get 6 fps, never more than 2 per frame, and a late scene does not starve the others', () => {
  const scenes = Array.from({ length: 11 }, (_, i) => ({ id: `s${i}`, visible: true, start: i * (1000 / 6 / 11) }))
  const perFrameCounts = new Map()
  const result = simulate({ scenes, fps: 6, seconds: 10, perFrame: 2 })
  for (const s of result) for (const t of s.renders) perFrameCounts.set(Math.round(t), (perFrameCounts.get(Math.round(t)) ?? 0) + 1)
  for (const s of result) assert.ok(Math.abs(s.renders.length / 10 - 6) < 0.3, `${s.id}: ${s.renders.length / 10} fps`)
  assert.ok(Math.max(...perFrameCounts.values()) <= 2)
})

test('invisible scenes are never rendered and a scene that becomes visible renders at once', () => {
  const scenes = [{ id: 'on', visible: true }, { id: 'off', visible: false }]
  const result = simulate({ scenes, fps: 6, seconds: 2, perFrame: 2 })
  assert.equal(result[1].renders.length, 0)
  assert.ok(result[0].renders.length >= 11)
  assert.deepEqual(dueScenes([{ id: 'off', visible: true, dueAt: 5 }], 1_000_000), ['off'])
})

test('11 scenes that all start due at once still settle to 6 fps with at most 2 renders per frame', () => {
  const scenes = Array.from({ length: 11 }, (_, i) => ({ id: `s${i}`, visible: true }))
  const result = simulate({ scenes, fps: 6, seconds: 10, perFrame: 2 })
  for (const s of result) assert.ok(s.renders.length / 10 >= 5.8, `${s.id}: ${s.renders.length / 10} fps`)
})
