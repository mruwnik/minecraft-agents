// Pure key mapping for driving a body from the view page.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { controlFor, lookStepFor, mouseLook, mergeLook, bannerText, serialQueue } from '../tools/view/web/drive-keys.mjs'

const controlCases = [
  ['KeyW', 'forward'], ['KeyS', 'back'], ['KeyA', 'left'], ['KeyD', 'right'], ['Space', 'jump'],
  ['ShiftLeft', 'sneak'], ['ShiftRight', 'sneak'], ['KeyR', 'sprint'],
  ['ControlLeft', null], ['KeyF', null], ['ArrowUp', null]
]
for (const [code, expected] of controlCases) {
  test(`controlFor ${code}`, () => assert.equal(controlFor(code), expected))
}

const lookCases = [
  ['ArrowLeft', { dyaw: -15 }], ['ArrowRight', { dyaw: 15 }], ['ArrowUp', { dpitch: -10 }], ['ArrowDown', { dpitch: 10 }],
  ['KeyW', null]
]
for (const [code, expected] of lookCases) {
  test(`lookStepFor ${code}`, () => assert.deepEqual(lookStepFor(code), expected))
}

const mouseCases = [
  [10, 20, undefined, { dyaw: 1.5, dpitch: 3 }],
  [-10, -20, undefined, { dyaw: -1.5, dpitch: -3 }],
  [10, 10, 0.5, { dyaw: 5, dpitch: 5 }],
  [0, 0, undefined, { dyaw: 0, dpitch: 0 }]
]
for (const [mx, my, sens, expected] of mouseCases) {
  test(`mouseLook ${mx},${my},${sens}`, () => {
    const got = mouseLook(mx, my, sens)
    assert.ok(Math.abs(got.dyaw - expected.dyaw) < 1e-9)
    assert.ok(Math.abs(got.dpitch - expected.dpitch) < 1e-9)
  })
}

const mergeCases = [
  [{ dyaw: 1, dpitch: 2 }, { dyaw: 3, dpitch: -1 }, { dyaw: 4, dpitch: 1 }],
  [{ dyaw: 1 }, { dpitch: 2 }, { dyaw: 1, dpitch: 2 }],
  [{}, {}, { dyaw: 0, dpitch: 0 }]
]
for (const [a, b, expected] of mergeCases) {
  test(`mergeLook ${JSON.stringify(a)} ${JSON.stringify(b)}`, () => assert.deepEqual(mergeLook(a, b), expected))
}

const bannerCases = [
  [null, 'view', null],
  [undefined, 'view', null],
  [{ who: 'view', why: 'x' }, 'view', 'MANUAL CONTROL (you) — WASD move, space jump, shift sneak, R sprint, arrows/mouse look, G release'],
  [{ who: 'claude', why: 'testing' }, 'view', 'MANUAL CONTROL by claude: testing']
]
for (const [manual, me, expected] of bannerCases) {
  test(`bannerText ${JSON.stringify(manual)}`, () => assert.equal(bannerText(manual, me), expected))
}

const deferred = () => {
  const d = {}
  d.promise = new Promise((resolve, reject) => Object.assign(d, { resolve, reject }))
  return d
}
const tick = () => new Promise(resolve => setImmediate(resolve))

test('serialQueue starts the second call only after the first resolves', async () => {
  const log = []
  const first = deferred()
  const enqueue = serialQueue()
  enqueue(() => { log.push('start1'); return first.promise })
  const second = enqueue(() => { log.push('start2'); return 'two' })
  await tick()
  assert.deepEqual(log, ['start1'])
  first.resolve('one')
  assert.equal(await second, 'two')
  assert.deepEqual(log, ['start1', 'start2'])
})

test('serialQueue still runs the next call after a rejection, and passes the result through', async () => {
  const enqueue = serialQueue()
  const failed = enqueue(() => Promise.reject(new Error('boom')))
  const next = enqueue(() => 'ok')
  await assert.rejects(failed, /boom/)
  assert.equal(await next, 'ok')
})
