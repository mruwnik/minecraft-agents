import { test } from 'node:test'
import assert from 'node:assert/strict'
import { createViewHub, dueScenes, nextDueAt, planFrame, eventCache } from '../tools/view/web/hub.mjs'

const entry = (id, dueAt, visible = true) => ({ id, dueAt, visible })

const dueCases = [
  ['only visible scenes whose time has come', [entry('a', 100), entry('b', 100, false), entry('c', 300)], 200, Infinity, ['a']],
  ['longest overdue first', [entry('a', 150), entry('b', 50), entry('c', 100)], 200, Infinity, ['b', 'c', 'a']],
  ['ties keep their order', [entry('a', 10), entry('b', 10), entry('c', 10)], 20, Infinity, ['a', 'b', 'c']],
  ['at most max', [entry('a', 1), entry('b', 2), entry('c', 3)], 10, 2, ['a', 'b']],
  ['due exactly now', [entry('a', 200)], 200, Infinity, ['a']],
  ['nothing', [], 200, Infinity, []],
  ['a raf target is always due and goes first, outside max', [entry('a', 1), { ...entry('big', 9999), raf: true }, entry('b', 2)], 10, 1, ['big', 'a']],
  ['an invisible raf target is not due', [{ ...entry('big', 0, false), raf: true }], 10, Infinity, []]
]
for (const [name, entries, now, max, expected] of dueCases) {
  test(`dueScenes: ${name}`, () => assert.deepEqual(dueScenes(entries, now, max), expected))
}

const nextCases = [
  ['one period after it was due', [1000, 1010, 6], 1000 + 1000 / 6],
  ['a little late still keeps the cadence', [1000, 1100, 6], 1000 + 1000 / 6],
  ['more than a period behind restarts from now', [1000, 1300, 6], 1300 + 1000 / 6],
  ['raf is due again at the next frame', [1000, 1016, 'raf'], 1016]
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

test('without WebGL2 the hub does not throw, says supported: false and leaves the canvas alone', async () => {
  const warn = console.warn
  console.warn = () => {}
  globalThis.document = { createElement: () => ({ getContext: () => null }) }
  try {
    const hub = createViewHub({})
    const scene = hub.addScene({ agent: 'Bob' })
    const canvas = { width: 300, height: 150, getContext: () => assert.fail('the canvas must not be drawn on') }
    scene.attach(canvas, { width: 320, height: 180 })
    assert.equal(hub.supported, false)
    assert.deepEqual([canvas.width, canvas.height], [300, 150])
    assert.equal(scene.stats().status, 'unsupported')
    await assert.rejects(scene.snapshot({ width: 8, height: 8 }))
    scene.detach(canvas)
    scene.close()
    hub.close()
  } finally {
    console.warn = warn
    delete globalThis.document
  }
})

// 60 Hz frames for `seconds` through planFrame: one raf target `big` (bigMs per render) and `cards` cards at 6 fps (cardMs per render)
const simulatePlan = ({ cards, bigMs, cardMs, seconds = 10, budgetMs = 8, withBig = true }) => {
  const targets = [
    ...(withBig ? [{ id: 'big', raf: true, fps: 'raf', visible: true, dueAt: 0, cost: bigMs, renders: [] }] : []),
    ...Array.from({ length: cards }, (_, i) => ({ id: `c${i}`, raf: false, fps: 6, visible: true, dueAt: i * (1000 / 6 / cards), cost: cardMs, renders: [] }))
  ]
  const frameCosts = []
  for (let n = 0; n < seconds * 60; n++) {
    const now = n * 1000 / 60
    let frame = 0
    planFrame({
      entries: targets, now, budgetMs,
      run: id => {
        const t = targets.find(x => x.id === id)
        t.renders.push(now)
        t.dueAt = nextDueAt(t.dueAt, now, t.fps)
        frame += t.cost
        return t.cost
      }
    })
    frameCosts.push(frame)
  }
  return { targets, frameCosts }
}

const planCases = [
  ['cheap big view', { cards: 11, bigMs: 3, cardMs: 2 }],
  ['big view with a few cards', { cards: 4, bigMs: 8, cardMs: 2 }]
]
for (const [name, args] of planCases) {
  test(`planFrame, ${name}: the raf target renders every frame and the cards keep 6 fps`, () => {
    const { targets } = simulatePlan(args)
    const [big, ...cards] = targets
    assert.equal(big.renders.length, 600)
    for (const c of cards) assert.ok(c.renders.length / 10 >= 5.8, `${c.id}: ${c.renders.length / 10} fps`)
  })
}

test('planFrame: cards stay within the budget next to the big view', () => {
  const { frameCosts } = simulatePlan({ cards: 11, bigMs: 3, cardMs: 2 })
  assert.ok(Math.max(...frameCosts) <= 8 + 2, `worst frame ${Math.max(...frameCosts)} ms`)
})

test('planFrame: without the big view the cards use the same budget', () => {
  const { frameCosts, targets } = simulatePlan({ cards: 11, bigMs: 0, cardMs: 2, withBig: false })
  assert.ok(Math.max(...frameCosts) <= 8 + 2)
  for (const c of targets) assert.ok(c.renders.length / 10 >= 5.8)
})

test('planFrame: a big view that eats the whole frame slows the cards but never starves them or itself', () => {
  const { targets } = simulatePlan({ cards: 11, bigMs: 12, cardMs: 2 })
  const [big, ...cards] = targets
  assert.equal(big.renders.length, 600)
  for (const c of cards) assert.ok(c.renders.length / 10 >= 3.5, `${c.id}: ${c.renders.length / 10} fps`)
})

test('planFrame: an invisible raf target does not render and the cards are unaffected', () => {
  const rendered = planFrame({ entries: [{ id: 'big', raf: true, fps: 'raf', visible: false, dueAt: 0 }, { id: 'c', raf: false, fps: 6, visible: true, dueAt: 0 }], now: 5, budgetMs: 8, run: () => 1 })
  assert.deepEqual(rendered, ['c'])
})

// the hub's replay of the last pose and hud of each agent to scenes added after the stream opened
const poseOf = (agent, n) => ({ agent, pose: { n } })
const replayed = (cache, agent) => {
  const got = []
  cache.replay(agent, (event, data) => got.push([event, data]))
  return got
}

test('a scene added for an already-streamed agent is fed the latest pose and hud', () => {
  const cache = eventCache()
  cache.record('pose', poseOf('Bob', 1))
  cache.record('pose', poseOf('Bob', 2))
  cache.record('hud', { agent: 'Bob', hud: 7 })
  cache.record('column', { agent: 'Bob', cx: 0, cz: 0 })
  assert.deepEqual(replayed(cache, 'Bob'), [['pose', poseOf('Bob', 2)], ['hud', { agent: 'Bob', hud: 7 }]])
})

test('replay is per agent and empty for an agent never streamed; two scenes of one agent each get it', () => {
  const cache = eventCache()
  cache.record('pose', poseOf('Bob', 1))
  cache.record('pose', poseOf('Ann', 5))
  assert.deepEqual(replayed(cache, 'Cy'), [])
  assert.deepEqual(replayed(cache, 'Bob'), [['pose', poseOf('Bob', 1)]])
  assert.deepEqual(replayed(cache, 'Bob'), [['pose', poseOf('Bob', 1)]])
})

test('a scene closed and re-added (the stream not reopened in between) is fed the last pose at once', () => {
  const cache = eventCache()
  cache.record('pose', poseOf('Bob', 1))
  cache.keepOnly(['Bob'])
  assert.deepEqual(replayed(cache, 'Bob'), [['pose', poseOf('Bob', 1)]])
})

test('keepOnly forgets the agents a reopened stream no longer carries, and only those', () => {
  const cache = eventCache()
  cache.record('pose', poseOf('Bob', 1))
  cache.record('hud', { agent: 'Bob', hud: 1 })
  cache.record('pose', poseOf('Ann', 5))
  cache.keepOnly(['Ann'])
  assert.deepEqual(replayed(cache, 'Bob'), [])
  assert.deepEqual(replayed(cache, 'Ann'), [['pose', poseOf('Ann', 5)]])
})
