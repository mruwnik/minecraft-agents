import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { controlTrace } from '../src/body/control-trace.mjs'

test('control diagnostics preserve exact positions, bound samples and remove listeners', () => {
  const bot = new EventEmitter()
  let time = 0
  let x = -1.3001
  bot.entity = { position: { toArray: () => [x, 60, -85.6999] }, velocity: { toArray: () => [0, 0.04, 0] }, yaw: 1.5, onGround: false, isInWater: true }
  bot.getControlState = k => k === 'jump'
  const finish = controlTrace(bot, { now: () => time, duration: 1000 })
  for (time = 1; time <= 3000; time++) {
    x -= 0.001
    bot.emit('physicsTick')
  }
  const result = finish()
  assert.equal(result.samples[0].position[0], -1.3001)
  assert.equal(result.samples.length, 20)
  assert.deepEqual(result.samples[1].keys, ['jump'])
  assert.equal(result.samples[1].inWater, true)
  assert.equal(bot.listenerCount('physicsTick'), 0)
  assert.equal(result.elapsedMs, 3001)
  finish()
  assert.equal(bot.listenerCount('physicsTick'), 0)
})
