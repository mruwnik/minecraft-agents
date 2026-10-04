import test from 'node:test'
import assert from 'node:assert/strict'
import {
  initial, onRestartRequest, onBuildDone, onServerExit, onQuit, buildSteps, buildOutcome, stopsBuild, stepCommand, parseMemAvailableMb, enoughMemory, minMemoryMb,
} from './launcher.mjs'

test('a request while idle starts a build', () => {
  const r = onRestartRequest(initial)
  assert.deepEqual(r.actions, ['build'])
  assert.equal(r.state.phase, 'building')
  assert.equal(r.state.pending, false)
})

test('requests during a build coalesce into one more build', () => {
  const building = onRestartRequest(initial).state
  const a = onRestartRequest(building)
  const b = onRestartRequest(a.state)
  assert.deepEqual(b.actions, [])
  assert.equal(b.state.pending, true)
  const done = onBuildDone(b.state, true)
  assert.deepEqual(done.actions, ['swap', 'build'])
  assert.equal(done.state.phase, 'building')
  assert.equal(done.state.pending, false)
  const last = onBuildDone(done.state, true)
  assert.deepEqual(last.actions, ['swap'])
  assert.equal(last.state.phase, 'idle')
})

test('a failed build keeps the old server', () => {
  const done = onBuildDone(onRestartRequest(initial).state, false)
  assert.deepEqual(done.actions, ['report-failure'])
  assert.equal(done.state.phase, 'idle')
})

test('a failed build with a pending request builds again', () => {
  const pending = onRestartRequest(onRestartRequest(initial).state).state
  assert.deepEqual(onBuildDone(pending, false).actions, ['report-failure', 'build'])
})

test('a swap makes the next server exit expected, then clears it', () => {
  const swapping = { ...initial, stopping: 'swap' }
  const r = onServerExit(swapping, 0)
  assert.deepEqual(r.actions, ['start'])
  assert.equal(r.state.stopping, null)
})

test('an unrequested server exit ends the launcher with its code', () => {
  assert.deepEqual(onServerExit(initial, 3).actions, ['exit:3'])
  assert.deepEqual(onServerExit(initial, null, 'SIGKILL').actions, ['exit:1'])
})

test('a quit makes the server exit end the launcher with 0', () => {
  assert.deepEqual(onServerExit({ ...initial, stopping: 'quit' }, 0).actions, ['exit:0'])
})

test('parseMemAvailableMb reads MemAvailable in MB', () => {
  const text = 'MemTotal:       32722348 kB\nMemFree:         4806492 kB\nMemAvailable:    8837304 kB\nBuffers: 1 kB\n'
  assert.equal(parseMemAvailableMb(text), Math.floor(8837304 / 1024))
  assert.equal(parseMemAvailableMb('nothing here'), null)
})

test('enoughMemory needs at least the minimum available', () => {
  assert.equal(minMemoryMb, 3500)
  assert.equal(enoughMemory(3500), true)
  assert.equal(enoughMemory(3499), false)
  assert.equal(enoughMemory(null), false)
})

test('a quit during a build with a pending request starts no further build and does not swap', () => {
  const pending = onRestartRequest(onRestartRequest(initial).state).state
  const quit = onQuit(pending, true)
  assert.deepEqual(quit.actions, ['kill-build', 'stop-server'])
  assert.equal(quit.state.stopping, 'quit')
  const done = onBuildDone(quit.state, false)
  assert.deepEqual(done.actions, [])
  assert.deepEqual(onBuildDone(quit.state, true).actions, [])
})

test('requests after a quit are ignored', () => {
  const quit = onQuit(initial, true)
  assert.deepEqual(onRestartRequest(quit.state).actions, [])
})

test('a quit without a server exits at once', () => {
  assert.deepEqual(onQuit(initial, false).actions, ['kill-build', 'exit:0'])
})

test('a quit during a swap stops the replacement from starting', () => {
  const quit = onQuit({ ...initial, stopping: 'swap' }, true)
  assert.deepEqual(onServerExit(quit.state, 0).actions, ['exit:0'])
})

test('the ui builds before the server so a failed build leaves the old server file; the viewer comes last', () => {
  assert.deepEqual(buildSteps, ['ui', 'server', 'viewer'])
})

test('buildOutcome stops at the first failed required step', () => {
  assert.deepEqual(buildOutcome([0, 0, 0]), { ok: true, failed: null, warned: [] })
  assert.deepEqual(buildOutcome([1]), { ok: false, failed: 'ui', warned: [] })
  assert.deepEqual(buildOutcome([0, 2]), { ok: false, failed: 'server', warned: [] })
})

test('a failed viewer build is a warning: the build is still ok', () => {
  assert.deepEqual(buildOutcome([0, 0, 1]), { ok: true, failed: null, warned: ['viewer'] })
})

for (const [step, code, expected] of [
  ['ui', 0, false], ['ui', 1, true], ['server', 1, true], ['viewer', 1, false], ['viewer', 0, false]
]) {
  test(`stopsBuild ${step} exit ${code}`, () => assert.equal(stopsBuild(step, code), expected))
}

for (const [step, expected] of [
  ['viewer', ['node', ['../tools/view/build-cljs.mjs']]],
  ['ui', ['flock', ['/tmp/mc-compile.lock', 'npx', 'shadow-cljs', 'compile', 'ui']]],
  ['server', ['flock', ['/tmp/mc-compile.lock', 'npx', 'shadow-cljs', 'compile', 'server']]]
]) {
  test(`stepCommand ${step}`, () => assert.deepEqual(stepCommand(step), expected))
}
