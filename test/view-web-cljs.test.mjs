// The viewer's cljs build as the pages load it (tools/view/web/cljs/viewer.mjs, :advanced): every export is there and the
// JS objects in and out keep their property names. The behaviour is tested in cljs (dashboard/test/view).
import { test } from 'node:test'
import assert from 'node:assert/strict'
import * as viewer from '../tools/view/web/cljs/viewer.mjs'

const pose = (t, x, extra = {}) => ({ t, status: 'online', eye: { x, y: 65.62, z: 0 }, pos: { x, y: 64, z: 0 }, yaw: 1, pitch: 0, world: 'w', ...extra })

test('every export drive.mjs and scene.mjs import is a function', () => {
  for (const name of ['poseInterpolator', 'viewHub', 'createSceneCore', 'decodePriority', 'controlFor', 'lookStepFor', 'mouseLook', 'mergeLook', 'bannerText', 'serialQueue', 'whoFrom',
    'shouldTakeOnClick', 'shouldReleaseOnEscape', 'leaveAction', 'withTimeout', 'isStale', 'shouldDrop']) {
    assert.equal(typeof viewer[name], 'function', name)
  }
})

test('poseInterpolator: methods by name, a blended pose keeps the fields of the newer pose', () => {
  const interp = viewer.poseInterpolator({ teleport: 8 })
  const ent = (x) => [{ id: 7, pos: { x, y: 64, z: 0 }, yaw: 0, kind: 'cow' }]
  interp.push(pose(1000, 0, { entities: ent(0) }), 6000)
  interp.push(pose(1100, 1, { entities: ent(1), extra: 'kept' }), 6100)
  const shown = interp.sample(6100 + interp.delay() - 50)
  assert.ok(shown.eye.x > 0 && shown.eye.x < 1, `x ${shown.eye.x}`)
  assert.equal(shown.extra, 'kept')
  assert.equal(shown.world, 'w')
  assert.equal(shown.entities[0].kind, 'cow')
  assert.ok(shown.entities[0].pos.x > 0 && shown.entities[0].pos.x < 1)
  for (const name of ['interval', 'offset', 'underruns']) assert.equal(typeof interp[name](), 'number', name)
  assert.equal(typeof interp.playhead(7000), 'number')
  assert.equal(viewer.poseInterpolator().sample(0), null)
})

// view.scene over fakes, through the advanced build: the scene object and its metrics keep the names app.mjs, hub.mjs and the
// measurement tools read
test('createSceneCore: the scene and its metrics keep their property names', () => {
  const world = { allocate () {}, uploadColumn () {}, clearSlot () {}, setBiomes: () => true, dispose () {} }
  const scene = viewer.createSceneCore({ agent: 'w/Bob', radius: 1, ownStream: false, world, decoder: { decode: () => new Promise(() => {}), cancel () {} }, tables: { ensure: () => new Promise(() => {}) }, fetch: () => new Promise(() => {}) })
  for (const name of ['frame', 'drew', 'stats', 'close', 'pose', 'hud', 'counts', 'isReady', 'feed']) assert.equal(typeof scene[name], 'function', name)
  for (const name of ['latencies', 'shownLatencies', 'camTrace', 'decodeMs', 'lightMs', 'uploadMs', 'columnDrawn', 'retargetMs', 'slowUploads']) assert.ok(Array.isArray(scene.metrics[name]), name)
  assert.deepEqual(scene.counts(), { inFlight: 0, decoding: 0, uploads: 0, needs: 0 })
  assert.deepEqual(scene.stats(), { loaded: 0, wanted: 0, poseAge: null, status: 'connecting' })
  assert.equal(scene.frame(0), null)
  assert.equal(viewer.decodePriority(`${scene.id}|0.0`), Infinity)
  scene.close()
})

test('viewHub without a surface: the unsupported hub', () => {
  const hub = viewer.viewHub({}, null)
  assert.equal(hub.supported, false)
  assert.deepEqual(hub.stats(), { supported: false, scenes: {} })
})

test('drive rules take and give JS values', () => {
  assert.equal(viewer.controlFor('KeyW'), 'forward')
  assert.equal(viewer.controlFor('KeyF'), null)
  assert.deepEqual(viewer.lookStepFor('ArrowUp'), { dpitch: -10 })
  assert.deepEqual(viewer.mergeLook(viewer.mouseLook(10, 20), { dyaw: 1 }), { dyaw: 2.5, dpitch: 3 })
  assert.equal(viewer.bannerText({ who: 'other', why: 'testing', expiresAt: 5 }, 'view'), 'MANUAL CONTROL by other: testing')
  assert.equal(viewer.whoFrom('?who=dash:Bob'), 'dash:Bob')
  assert.equal(viewer.shouldTakeOnClick({ embed: true, driving: false, manual: null, me: 'v' }), true)
  assert.equal(viewer.shouldReleaseOnEscape({ code: 'Escape', driving: true, pointerLocked: false }), true)
  assert.equal(viewer.leaveAction('pointerlock-lost'), 'stop-release')
  assert.equal(viewer.isStale({ startedGen: 0, currentGen: 1 }), true)
  assert.equal(viewer.shouldDrop({ driving: true, reply: { ok: true, manual: { who: 'me' } }, me: 'me', startedGen: 1, currentGen: 1 }), false)
  assert.equal(viewer.shouldDrop({ driving: true, reply: { ok: false, reason: 'not-driver', manual: { who: 'me' } }, me: 'me', startedGen: 1, currentGen: 1 }), true)
})

test('serialQueue and withTimeout run under their exported names', async () => {
  const enqueue = viewer.serialQueue()
  const f = viewer.withTimeout((url, init) => Promise.resolve([url, init.method, init.signal.aborted]), 50)
  assert.deepEqual(await enqueue(() => f('/x', { method: 'POST' })), ['/x', 'POST', false])
})
