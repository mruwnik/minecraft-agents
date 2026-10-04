// Why JavaScript: tests the engine/tools launchers, which stay JS; a JS test is the honest check of a JS entry point (process, argv, exit code).
import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import os from 'node:os'
import path from 'node:path'
import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { clock, options, execute } from '../../tools/time.mjs'
import { readEDN } from './edn.mjs'

const cli = fileURLToPath(new URL('../../tools/time.mjs', import.meta.url))
function fixture(t) {
  const state = fs.mkdtempSync(path.join(os.tmpdir(), 'world-time-'))
  t.after(() => fs.rmSync(state, { recursive: true, force: true }))
  const worldDir = path.join(state, 'worlds', 'w')
  fs.mkdirSync(worldDir, { recursive: true })
  fs.writeFileSync(path.join(worldDir, 'world.json'), '{}')
  const args = ['--world', 'w', '--state', state]
  const pose = (body, values = {}) => {
    const dir = path.join(worldDir, 'agents', body, 'view')
    fs.mkdirSync(dir, { recursive: true })
    fs.writeFileSync(path.join(dir, 'pose.json'), JSON.stringify({ world: 'w', status: 'online', dimension: 'overworld', t: Date.now(), timeOfDay: 6000, ...values }))
  }
  return { state, worldDir, args, pose, ctx: options(args).ctx }
}

test('world clock needs an explicit world, never an addressed body', t => {
  const {args} = fixture(t)
  assert.throws(() => options([]), /world/)
  assert.throws(() => options([...args, 'Bob', 'clock']), /time.mjs/)
  for (const flags of [['clock', '--timeout', '1'], ['dawn', '--timeout', '-1'], ['dawn', '--timeout', 'NaN'], ['dawn', '--poll-ms', '0']]) assert.throws(() => options([...args, ...flags]))
})

test('newest fresh online Overworld observation wins, with stable engine day boundaries', t => {
  const {pose, ctx} = fixture(t), now = Date.now()
  pose('Old', {t: now - 100, timeOfDay: 18000})
  pose('New', {t: now - 1, timeOfDay: 6000})
  pose('Nether', {t: now, dimension: 'the_nether', timeOfDay: 0})
  pose('Offline', {t: now, status: 'offline'})
  pose('WrongWorld', {t: now, world: 'other'})
  assert.equal(clock(ctx, now).by, 'New')
  assert.equal(clock(ctx, now)['age-ms'], 1)
  for (const [tick, day] of [[12541,true], [12542,false], [23460,false], [23461,true]]) {
    pose('New', {t: now, timeOfDay: tick, dimension: 'minecraft:overworld'})
    assert.equal(clock(ctx, now)['day?'], day)
  }
})

test('missing, malformed, stale, future, wrong-dimension and disconnected reports are unknown', t => {
  const {pose, ctx, worldDir} = fixture(t), now = Date.now()
  assert.equal(clock(ctx, now).reason.key, 'time-unknown')
  pose('Stale', {t: now - 90001})
  pose('Future', {t: now + 1})
  pose('Offline', {status: 'offline'})
  pose('End', {dimension: 'the_end'})
  pose('NoTime', {timeOfDay: null})
  pose('Broken')
  fs.writeFileSync(path.join(worldDir, 'agents', 'Broken', 'view', 'pose.json'), '{')
  assert.equal(clock(ctx, now).reason.key, 'time-unknown')
})

test('dawn waits for an observed transition and never predicts server time', async t => {
  const {pose, args} = fixture(t)
  pose('Observer', {timeOfDay: 18000})
  const timer = setTimeout(() => pose('Observer'), 35)
  t.after(() => clearTimeout(timer))
  const result = await execute(options([...args, 'dawn', '--timeout', '1', '--poll-ms', '10']))
  assert.equal(result.ok, true)
  assert.equal(result.wake.key, 'day')
  assert.equal(result.by, 'Observer')
  assert.ok(result['waited-ms'] >= 30)
})

test('dawn stops at timeout or observer loss', async t => {
  const {pose, args} = fixture(t)
  pose('Observer', {timeOfDay: 18000})
  const timed = await execute(options([...args, 'dawn', '--timeout', '0']))
  assert.equal(timed.ok, false)
  assert.equal(timed.reason.key, 'timeout')
  const timer = setTimeout(() => pose('Observer', {status: 'offline'}), 20)
  t.after(() => clearTimeout(timer))
  const lost = await execute(options([...args, 'dawn', '--timeout', '1', '--poll-ms', '10']))
  assert.equal(lost.ok, false)
  assert.equal(lost.reason.key, 'time-unknown')
})

test('CLI emits compact EDN and handles unknown time without creating a body', t => {
  const {pose, args, worldDir} = fixture(t)
  const run = () => spawnSync(process.execPath, [cli, ...args], {encoding: 'utf8'})
  const absent = run()
  assert.equal(absent.status, 1, absent.stderr)
  assert.equal(readEDN(absent.stdout).reason.key, 'time-unknown')
  assert.equal(fs.existsSync(path.join(worldDir, 'agents')), false)
  pose('Observer')
  const found = run()
  assert.equal(found.status, 0, found.stderr)
  assert.equal(readEDN(found.stdout).by, 'Observer')
  assert.ok(found.stdout.length < 300)
})
