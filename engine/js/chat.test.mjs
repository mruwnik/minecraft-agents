import { test } from 'node:test'
import assert from 'node:assert/strict'
import { stubBot } from './stub-bot.mjs'
import { CHAT_MAX, splitSay, chatRefusal, say } from './chat.mjs'

const OPTS = { timeScale: 0.01 }
const ctx = () => ({ alive: () => {}, onAbort: () => {} })

const chatBot = ({ players = { Steve: {} }, refuse = null, position = 'system' } = {}) => {
  const bot = stubBot()
  bot.sent = []
  bot.players = players
  const send = (kind, args) => {
    bot.sent.push({ kind, args })
    if (refuse) bot.emit('messagestr', refuse, position)
  }
  bot.chat = (...args) => send('chat', args)
  bot.whisper = (...args) => send('whisper', args)
  return bot
}

const sentence = 'This is a sentence of moderate length. '
const long = sentence.repeat(20).trim()

test('splitSay cases', () => {
  const cases = [
    ['short', 'hello there', 50, ['hello there']],
    ['sentence end', 'One two. Three four five', 15, ['One two.', 'Three four five']],
    ['space', 'aaa bbb ccc ddd', 8, ['aaa bbb', 'ccc ddd']],
    ['hard', 'abcdefghij', 4, ['abcd', 'efgh', 'ij']],
    ['semicolon', 'ab; cd ef gh', 6, ['ab;', 'cd ef', 'gh']]
  ]
  for (const [name, text, budget, want] of cases) assert.deepEqual(splitSay(text, budget), want, name)
})

test('splitSay pieces are within budget and never empty', () => {
  for (const budget of [10, 37, 100]) {
    const parts = splitSay(long, budget)
    assert.ok(parts.every(p => p.length > 0 && p.length <= budget && p === p.trim()), `budget ${budget}`)
  }
})

test('chatRefusal cases', () => {
  const cases = [
    ['Command had invalid signature x', 'Command had invalid signature x'],
    ['No player was found', 'No player was found'],
    ['Unknown or incomplete command, see below', 'Unknown or incomplete command, see below'],
    ['An unexpected error occurred trying to execute that command', 'An unexpected error occurred trying to execute that command'],
    ['That player cannot be found', 'That player cannot be found'],
    ['that player cannot be found', 'that player cannot be found'],
    ['hello No player was found', null],
    ['fine', null]
  ]
  for (const [text, want] of cases) assert.equal(chatRefusal(text), want, text)
})

test('say to all, one part', async () => {
  const bot = chatBot()
  assert.deepEqual(await say(bot, ctx(), { message: '  hi all ' }, OPTS), { status: 'sent', parts: 1 })
  assert.deepEqual(bot.sent, [{ kind: 'chat', args: ['hi all'] }])
})

test('whisper to an online player', async () => {
  const bot = chatBot()
  assert.deepEqual(await say(bot, ctx(), { message: 'psst', to: 'Steve' }, OPTS), { status: 'sent', parts: 1, to: 'Steve' })
  assert.deepEqual(bot.sent, [{ kind: 'whisper', args: ['Steve', 'psst'] }])
})

test('whisper to an offline player is gone and sends nothing', async () => {
  const bot = chatBot()
  assert.deepEqual(await say(bot, ctx(), { message: 'psst', to: 'Alex' }, OPTS), { status: 'gone', to: 'Alex' })
  assert.deepEqual(bot.sent, [])
})

test('a long text gives several parts, each within budget, cut at sentence ends', async () => {
  const bot = chatBot()
  const r = await say(bot, ctx(), { message: long }, OPTS)
  assert.equal(r.parts, bot.sent.length)
  assert.ok(r.parts > 1)
  assert.ok(bot.sent.every(s => s.args[0].length <= CHAT_MAX && s.args[0].endsWith('.')))
})

test('the whisper budget is smaller than the chat budget', async () => {
  const text = 'x'.repeat(CHAT_MAX)
  const chat = chatBot()
  const whisper = chatBot()
  await say(chat, ctx(), { message: text }, OPTS)
  await say(whisper, ctx(), { message: text, to: 'Steve' }, OPTS)
  assert.equal(chat.sent.length, 1)
  assert.equal(whisper.sent.length, 2)
  assert.ok(whisper.sent.every(s => s.args[1].length <= CHAT_MAX - '/tell Steve '.length))
})

test('a refusal line gives failed', async () => {
  for (const position of ['system', 'game_info']) {
    const bot = chatBot({ refuse: 'No player was found', position })
    const r = await say(bot, ctx(), { message: 'hi', to: 'Steve' }, OPTS)
    assert.deepEqual(r, { status: 'failed', reason: 'No player was found', parts: 1 }, position)
  }
})

test('a chat-position line matching the pattern is ignored', async () => {
  const bot = chatBot({ refuse: 'No player was found', position: 'chat' })
  assert.equal((await say(bot, ctx(), { message: 'hi' }, OPTS)).status, 'sent')
})

test('the listener is removed afterwards', async () => {
  const bot = chatBot({ refuse: 'No player was found' })
  const before = bot.listenerCount('messagestr')
  await say(bot, ctx(), { message: 'hi' }, OPTS)
  assert.equal(bot.listenerCount('messagestr'), before)
})

test('a cut rejects and removes the listener', async () => {
  const bot = chatBot()
  const before = bot.listenerCount('messagestr')
  const cut = { alive: () => { throw new Error('cut') }, onAbort: () => {} }
  await assert.rejects(say(bot, cut, { message: 'hi' }, OPTS), /cut/)
  assert.equal(bot.listenerCount('messagestr'), before)
})
