import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { walkStandstill, walkProgress, WALK_PROGRESS_MS } from '../src/navigation/stall.mjs'
import { isStalled, deadWalk, droppedWalk } from '../src/lib.mjs'
import { createRequire } from 'node:module'
import { EventEmitter } from 'node:events'
import search from '../library/forage/search.mjs'
import { recoverableNavigationTarget, navigationTargetKey } from '../src/composite.mjs'
const require = createRequire(import.meta.url)

const pos = { x: 20, y: 65, z: -90 }
const goal = { x: 22, y: 65, z: -90 }
const state = (previous, now, target = goal, task = 7, position = pos) => walkStandstill(previous, { task, goal: target, pos: position, now })
const sample = (still, now) => ({ hasGoal: Boolean(still.goal), moved: 0, digging: false, seconds: (now - still.at) / 1000, path: { status: 'success', nodes: ['21.5,65,-89.5'], at: now } })

test('a long-idle routine begins a fresh walk without inheriting its 12-second alarm', () => {
  let still = state(null, 0, null)
  still = state(still, 60_000, null)
  still = state(still, 120_000)
  assert.equal(still.at, 120_000)
  assert.equal(isStalled(sample(still, 120_500)), false)
  assert.equal(deadWalk(sample(still, 120_500)), false)
  assert.equal(droppedWalk({ ...sample(still, 120_500), moving: false, pathAgeMs: 500 }), false)
  assert.equal(isStalled(sample(still, 132_000)), true, 'a genuinely motionless new walk still times out')
})

test('same-goal replans preserve stall and one-kick episode identity', () => {
  const first = state(null, 1_000)
  const kickedFor = first
  for (const now of [3_000, 7_000, 10_000, 13_000]) {
    const next = state(first, now)
    assert.equal(next, kickedFor, 'replanning cannot authorize another watchdog kick')
    assert.equal(next.at, 1_000)
  }
  assert.equal(isStalled(sample(first, 13_000)), true)
})

test('new goal or task resets the episode even without physical movement', () => {
  const previous = state(null, 0)
  assert.equal(state(previous, 60_000, { ...goal }).at, 60_000)
  assert.equal(state(previous, 60_000, goal, 8).at, 60_000)
  assert.equal(state(previous, 60_000, null).at, 60_000)
  assert.equal(state(previous, 60_000, goal, null).at, 60_000)
})

test('movement still resets the clock while sub-threshold jitter does not', () => {
  const previous = state(null, 0)
  assert.equal(state(previous, 5_000, goal, 7, { ...pos, x: pos.x + 0.1 }), previous)
  assert.equal(state(previous, 5_000, goal, 7, { ...pos, x: pos.x + 0.3 }).at, 5_000)
})

test('actual goal-update listener catches no-goal transitions between watchdog samples without rearming same-goal kicks', () => {
  const source = fs.readFileSync(new URL('../src/body/connection.mjs', import.meta.url), 'utf8')
  const start = source.indexOf("    bot.on('goal_updated', goal => {")
  const end = source.indexOf("    bot.on('path_reset'", start)
  assert.ok(start >= 0 && end > start)
  const install = new Function('bot', 'initial', `let stillFrom = initial, goalSetAt = 0; ${source.slice(start, end)}; return () => ({ stillFrom, goalSetAt })`)
  let onGoal
  const episode = state(null, 0)
  const read = install({ on: (name, fn) => { assert.equal(name, 'goal_updated'); onGoal = fn } }, episode)
  onGoal(goal)
  assert.equal(read().stillFrom, episode)
  onGoal(null)
  assert.equal(read().stillFrom, null)
  onGoal(goal)
  assert.equal(read().stillFrom, null, 'reusing the same object after clearing still starts fresh')
  assert.ok(read().goalSetAt > 0, 'path freshness timestamp remains updated')
})

test('water oscillation and same-target replanning cannot renew an awaited leg progress allowance', () => {
  const leg = {}, goal = { x: 16, y: 65, z: 0 }
  let progress
  for (let seconds = 0; seconds <= 41; seconds++) {
    progress = walkProgress(progress, { leg, goal: { ...goal }, pos: { x: seconds % 2 ? 0.3 : 0, y: 65 + Math.sin(seconds) * 0.4, z: 0 }, now: seconds * 1000 })
    assert.equal(progress.stalled, seconds >= 40)
  }
  assert.equal(progress.at, 0)
})

test('real swimming and vertical climbing keep progressing; unknown and entity-follow goals are excluded', () => {
  for (const vertical of [true, false]) {
    const leg = {}, goal = vertical ? { x: 0, y: 100, z: 0 } : { x: 100, y: 65, z: 0 }
    let progress
    for (let seconds = 0; seconds < 180; seconds++) {
      progress = walkProgress(progress, { leg, goal, pos: { x: vertical ? 0 : seconds / 4, y: vertical ? 65 + seconds / 8 : 65, z: 0 }, now: seconds * 1000 })
      assert.equal(progress.stalled, false)
    }
  }
  for (const goal of [{}, { x: 1, z: 1, entity: {} }]) assert.equal(walkProgress(null, { leg: {}, goal, pos, now: 0 }), null)
})

test('working in place pauses the movement allowance and a new awaited leg starts fresh', () => {
  const leg = {}
  let progress = walkProgress(null, { leg, goal, pos, now: 0 })
  for (let now = 1000; now <= 100_000; now += 1000) {
    progress = walkProgress(progress, { leg, goal, pos, now, busy: now >= 10_000 && now < 90_000 })
    assert.equal(progress.stalled, false)
  }
  assert.equal(walkProgress(progress, { leg, goal, pos, now: 130_000 }).stalled, true)
  assert.equal(walkProgress(progress, { leg: {}, goal, pos, now: 130_000 }).stalled, false)
})

test('production watchdog settles native nested goto before forage recovers to a different leg', async () => {
  const source = fs.readFileSync(new URL('../src/body/connection.mjs', import.meta.url), 'utf8')
  const walkStart = source.indexOf('    const walk = goal => {')
  const walkEnd = source.indexOf('    const arrived = goal => {', walkStart)
  const watchStart = source.indexOf('  // Every awaited static walk owns this watchdog')
  const watchEnd = source.indexOf('  if (deadWalk(sample)', watchStart)
  const gotoStart = source.indexOf('    bot.pathfinder.goto = async goal => {')
  const gotoEnd = source.indexOf('      if (!failure && arrived(goal)) return', gotoStart)
  assert.ok(walkStart >= 0 && walkEnd > walkStart && watchStart >= 0 && watchEnd > watchStart && gotoEnd > gotoStart)
  const bot = Object.assign(new EventEmitter(), { entity: { position: { x: 0, y: 64, z: 0 } }, pathfinder: {
    goal: null, setGoal (goal) { this.goal = goal; bot.emit('goal_updated', goal) }, isMining: () => false, isBuilding: () => false
  } })
  const nativeGoto = require('mineflayer-pathfinder/lib/goto')
  const install = new Function('bot', 'plainWalk', 'walkProgress', 'WALK_PROGRESS_MS', `
    let activeWalk = null, nearest = null, lastWalkGoal, walkEndedAt = -Infinity, walkEndedBy, stillFrom;
    let clock = 0; const Date = { now: () => clock };
    const task = { id: 7, name: 'forage.search' };
    const thinkBudget = () => 5000, goalDistance = () => 16, THINK_CAP_MS = 5000, sprintFor = () => {};
    const emit = () => {}, pos = () => bot.entity.position, stallEvidence = () => ({});
    ${source.slice(walkStart, walkEnd)}
    ${source.slice(gotoStart, gotoEnd)}
      if (failure) throw failure;
    };
    return { tick(now) { clock = now; const here = bot.entity.position; ${source.slice(watchStart, watchEnd)} },
      active: () => activeWalk };
  `)
  const runtime = install(bot, goal => nativeGoto(bot, goal), walkProgress, WALK_PROGRESS_MS)
  let walks = 0, acknowledged = 0, pendingFailure = null
  const targets = []
  const api = { pos: () => bot.entity.position, block: (x, y) => ({ name: y === 63 ? 'stone' : 'air', solid: y === 63 }), checkpoint: async () => {}, report: () => {},
    recoverNavigationFailure: target => {
      acknowledged++; assert.equal(runtime.active(), null); assert.equal(bot.pathfinder.goal, null)
      const permitted = pendingFailure !== null && pendingFailure === navigationTargetKey(target)
      pendingFailure = null
      return permitted
    },
    async act (name, args) {
      if (name !== 'goto') return { positions: [] }
      assert.equal(runtime.active(), null, 'the old leg has settled before another starts')
      targets.push(args)
      const pending = bot.pathfinder.goto({ ...args })
      if (++walks === 1) {
        for (let ms = 0; ms <= WALK_PROGRESS_MS; ms += 1000) {
          bot.entity.position = { x: ms % 2000 ? 0.3 : 0, y: 64 + Math.sin(ms / 1000) * 0.2, z: 0 }
          runtime.tick(ms)
        }
        assert.equal(bot.listenerCount('goal_updated'), 0, 'native goto removes its listeners synchronously')
      } else {
        bot.entity.position = { x: args.x, y: args.y, z: args.z }
        bot.emit('goal_reached')
        bot.pathfinder.setGoal(null)
      }
      try { await pending } catch (error) { pendingFailure = recoverableNavigationTarget(error, args); throw error }
      return {}
    } }
  const result = await search.run(api, { block: 'bamboo', steps: 1 })
  assert.equal(result.failures.length, 1)
  assert.match(result.failures[0].reason, /no net progress.*40s/)
  assert.equal(acknowledged, 1)
  assert.equal(walks, 2)
  assert.notDeepEqual(targets[0], targets[1])
  assert.equal(runtime.active(), null)
})
