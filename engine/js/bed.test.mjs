// Why JavaScript: tests bed.mjs, which stays JS: Mineflayer boundary; calls the bot's bed/sleep API.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { leaveBed, ensureAwake } from './bed.mjs'

const tick = () => new Promise(resolve => setTimeout(resolve, 1))

const fakeBot = ({ sleeping = true, wakes = true } = {}) => {
  const writes = []
  const bot = {
    isSleeping: sleeping,
    entity: { id: 7 },
    _client: {
      write: (name, data) => {
        writes.push({ name, data })
        if (wakes && data.actionId === 'leave_bed') tick().then(() => { bot.isSleeping = false })
      }
    }
  }
  return { bot, writes }
}

test('leaveBed writes entity_action leave_bed by name', () => {
  const { bot, writes } = fakeBot()
  leaveBed(bot)
  assert.deepEqual(writes, [{ name: 'entity_action', data: { entityId: 7, actionId: 'leave_bed', jumpBoost: 0 } }])
})

test('ensureAwake resolves true at once for a body that is up', async () => {
  const { bot, writes } = fakeBot({ sleeping: false })
  assert.equal(await ensureAwake(bot), true)
  assert.deepEqual(writes, [])
})

test('ensureAwake leaves the bed and resolves true once the body is awake', async () => {
  const { bot, writes } = fakeBot()
  assert.equal(await ensureAwake(bot, { pollMs: 5 }), true)
  assert.equal(writes.length, 1)
  assert.equal(bot.isSleeping, false)
})

test('ensureAwake resolves false after the timeout when the body never wakes', async () => {
  const { bot } = fakeBot({ wakes: false })
  const t0 = Date.now()
  assert.equal(await ensureAwake(bot, { timeoutMs: 60, pollMs: 10 }), false)
  assert.ok(Date.now() - t0 >= 50)
})
