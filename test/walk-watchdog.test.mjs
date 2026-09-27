import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import { walkStandstill } from '../src/navigation/stall.mjs'
import { isStalled, deadWalk, droppedWalk } from '../src/lib.mjs'

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
  const source = fs.readFileSync(new URL('../src/bot.mjs', import.meta.url), 'utf8')
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
