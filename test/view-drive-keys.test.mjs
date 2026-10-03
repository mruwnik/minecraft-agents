// Pure key mapping for driving a body from the view page.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { controlFor, lookStepFor, mouseLook, mergeLook, bannerText, serialQueue, whoFrom, shouldTakeOnClick, shouldReleaseOnEscape, withTimeout } from '../tools/view/web/drive-keys.mjs'

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

for (const [search, expected] of [
  ['?agent=Bob', 'view'], ['?agent=Bob&who=dash:Bob', 'dash:Bob'], ['?who=a_b-C1', 'a_b-C1'],
  ['?who=', 'view'], ['?who=has space', 'view'], ['?who=a/b', 'view'], ['?who=' + 'x'.repeat(41), 'view'],
  ['?who=' + 'x'.repeat(40), 'x'.repeat(40)], ['', 'view']
]) {
  test(`whoFrom ${search}`, () => assert.equal(whoFrom(search), expected))
}

for (const [args, expected] of [
  [{ embed: true, driving: false, manual: null, me: 'v' }, true],
  [{ embed: true, driving: false, manual: { who: 'v' }, me: 'v' }, true],
  [{ embed: true, driving: false, manual: { who: 'claude' }, me: 'v' }, false],
  [{ embed: true, driving: true, manual: { who: 'v' }, me: 'v' }, false],
  [{ embed: false, driving: false, manual: null, me: 'v' }, false]
]) {
  test(`shouldTakeOnClick ${JSON.stringify(args)}`, () => assert.equal(shouldTakeOnClick(args), expected))
}

for (const [args, expected] of [
  [{ code: 'Escape', driving: true, pointerLocked: false }, true],
  [{ code: 'Escape', driving: true, pointerLocked: true }, false],
  [{ code: 'Escape', driving: false, pointerLocked: false }, false],
  [{ code: 'KeyG', driving: true, pointerLocked: false }, false]
]) {
  test(`shouldReleaseOnEscape ${JSON.stringify(args)}`, () => assert.equal(shouldReleaseOnEscape(args), expected))
}

const hangingFetch = (log) => (url, init) => new Promise((resolve, reject) => {
  log.push({ url, init })
  init.signal.addEventListener('abort', () => reject(new Error('aborted')))
})

test('withTimeout aborts a hung request after ms and passes url and init through', async () => {
  const log = []
  const f = withTimeout(hangingFetch(log), 10)
  await assert.rejects(f('/x', { method: 'POST' }), /aborted/)
  assert.equal(log[0].url, '/x')
  assert.equal(log[0].init.method, 'POST')
})

test('withTimeout returns the response of a fast request and does not abort it later', async () => {
  const signals = []
  const f = withTimeout((url, init) => { signals.push(init.signal); return Promise.resolve('ok') }, 10)
  assert.equal(await f('/x'), 'ok')
  await new Promise(resolve => setTimeout(resolve, 30))
  assert.equal(signals[0].aborted, false)
})

test('a timed-out request does not hold the serial queue', async () => {
  const log = []
  const f = withTimeout(hangingFetch(log), 10)
  const enqueue = serialQueue()
  const first = enqueue(() => f('/a', {}).catch(() => 'timeout'))
  const second = enqueue(() => 'stop')
  assert.equal(await first, 'timeout')
  assert.equal(await second, 'stop')
})
