// Why JavaScript: tests the JS step-trace hook against a fake Mineflayer bot.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { traceSteps, createRing } from './step-trace.mjs'

const fakeBot = () => Object.assign(new EventEmitter(), {
  entity: { position: { x: 1, y: 64, z: 2 }, velocity: { x: 0, y: -0.08, z: 0 }, yaw: 1, pitch: 0, onGround: true },
  controlState: { forward: true, jump: false },
  dig: async () => 'dug'
})

test('off: nothing is installed', () => {
  const bot = fakeBot()
  const dig = bot.dig
  traceSteps(bot, undefined)()
  assert.equal(bot.listenerCount('physicsTick'), 0)
  assert.equal(bot.dig, dig)
})

test('on: ticks and acts are written, stopping restores the bot', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'step-trace-'))
  const file = join(dir, 'steps.log')
  const bot = fakeBot()
  const dig = bot.dig
  const stop = traceSteps(bot, file)
  bot.emit('physicsTick')
  assert.equal(await bot.dig({ name: 'stone', position: { x: 1, y: 63, z: 2 } }), 'dug')
  stop()
  const lines = readFileSync(file, 'utf8').trim().split('\n')
  assert.match(lines[0], /^1 \d+ T 1,64,2 v 0,-0.08,0 y1 p0 G forward$/)
  assert.match(lines[1], /^1 \d+ A dig stone@1,63,2$/)
  assert.equal(bot.dig, dig)
  assert.equal(bot.listenerCount('physicsTick'), 0)
  rmSync(dir, { recursive: true })
})

test('the ring keeps only the latest lines', () => {
  const ring = createRing(3)
  ;[1, 2, 3, 4, 5].forEach(n => ring.push(`l${n}`))
  assert.equal(ring.text(), 'l3\nl4\nl5\n')
})
