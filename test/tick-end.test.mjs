import test from 'node:test'
import assert from 'node:assert/strict'
import { EventEmitter } from 'node:events'
import { createRequire } from 'node:module'
import { installTickEnd } from '../src/tick-end.mjs'
const require = createRequire(import.meta.url)
const data = require('minecraft-data')

const fixture = version => {
  const sent = []
  const bot = Object.assign(new EventEmitter(), { registry: data(version), _client: { write: (name, params) => sent.push([name, params]) } })
  installTickEnd(bot)
  return { bot, sent }
}

test('every physics tick ends with an empty tick_end once the protocol has one', () => {
  const { bot, sent } = fixture('26.1')
  bot.emit('physicsTick'); bot.emit('physicsTick')
  assert.deepEqual(sent, [['tick_end', {}], ['tick_end', {}]])
})

test('a protocol without tick_end gets nothing extra', () => {
  const { bot, sent } = fixture('1.21.1')
  bot.emit('physicsTick')
  assert.deepEqual(sent, [])
})
