import { test } from 'node:test'
import assert from 'node:assert/strict'
import { stubBot } from './stub-bot.mjs'
import { CHAT_MAX, splitSay, chatRefusal, say, cleanMessage, createLimiter } from './chat.mjs'

const OPTS = () => ({ timeScale: 0.01, limiter: createLimiter({ gapMs: 0, max: 100 }) })
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
  assert.deepEqual(await say(bot, ctx(), { message: '  hi all ' }, OPTS()), { status: 'sent', parts: 1 })
  assert.deepEqual(bot.sent, [{ kind: 'chat', args: ['hi all'] }])
})

test('whisper to an online player', async () => {
  const bot = chatBot()
  assert.deepEqual(await say(bot, ctx(), { message: 'psst', to: 'Steve' }, OPTS()), { status: 'sent', parts: 1, to: 'Steve' })
  assert.deepEqual(bot.sent, [{ kind: 'whisper', args: ['Steve', 'psst'] }])
})

test('whisper to an offline player is gone and sends nothing', async () => {
  const bot = chatBot()
  assert.deepEqual(await say(bot, ctx(), { message: 'psst', to: 'Alex' }, OPTS()), { status: 'gone', to: 'Alex' })
  assert.deepEqual(bot.sent, [])
})

test('a long text gives several parts, each within budget, cut at sentence ends', async () => {
  const bot = chatBot()
  const r = await say(bot, ctx(), { message: long }, OPTS())
  assert.equal(r.parts, bot.sent.length)
  assert.ok(r.parts > 1)
  assert.ok(bot.sent.every(s => s.args[0].length <= CHAT_MAX && s.args[0].endsWith('.')))
})

test('the whisper budget is smaller than the chat budget', async () => {
  const text = 'x'.repeat(CHAT_MAX)
  const chat = chatBot()
  const whisper = chatBot()
  await say(chat, ctx(), { message: text }, OPTS())
  await say(whisper, ctx(), { message: text, to: 'Steve' }, OPTS())
  assert.equal(chat.sent.length, 1)
  assert.equal(whisper.sent.length, 2)
  assert.ok(whisper.sent.every(s => s.args[1].length <= CHAT_MAX - '/tell Steve '.length))
})

test('a refusal line gives failed', async () => {
  for (const position of ['system', 'game_info']) {
    const bot = chatBot({ refuse: 'No player was found', position })
    const r = await say(bot, ctx(), { message: 'hi', to: 'Steve' }, OPTS())
    assert.deepEqual(r, { status: 'failed', reason: 'No player was found', parts: 1 }, position)
  }
})

test('a chat-position line matching the pattern is ignored', async () => {
  const bot = chatBot({ refuse: 'No player was found', position: 'chat' })
  assert.equal((await say(bot, ctx(), { message: 'hi' }, OPTS())).status, 'sent')
})

test('the listener is removed afterwards', async () => {
  const bot = chatBot({ refuse: 'No player was found' })
  const before = bot.listenerCount('messagestr')
  await say(bot, ctx(), { message: 'hi' }, OPTS())
  assert.equal(bot.listenerCount('messagestr'), before)
})

test('a cut rejects and removes the listener', async () => {
  const bot = chatBot()
  const before = bot.listenerCount('messagestr')
  const cut = { alive: () => { throw new Error('cut') }, onAbort: () => {} }
  await assert.rejects(say(bot, cut, { message: 'hi' }, OPTS()), /cut/)
  assert.equal(bot.listenerCount('messagestr'), before)
})

test('cleanMessage strips control characters and the section sign', () => {
  const cases = [['a\nb', 'a b'], ['  hi\x00there\x7f ', 'hi there'], ['§cred', 'cred'], ['\n\n', ''], ['/say x', '/say x']]
  for (const [text, want] of cases) assert.equal(cleanMessage(text), want, JSON.stringify(text))
})

test('a message that starts with a slash is a command and is never sent', async () => {
  for (const [message, to] of [['/op me', undefined], ['/op me', 'Steve'], ['\n/stop', undefined], ['§/stop', 'Steve']]) {
    const bot = chatBot()
    assert.deepEqual(await say(bot, ctx(), { message, to }, OPTS()), { status: 'cannot', reason: 'command' }, message)
    assert.deepEqual(bot.sent, [])
  }
})

test('newlines in the message are sent as spaces', async () => {
  const bot = chatBot()
  await say(bot, ctx(), { message: 'a\n/b' }, OPTS())
  assert.deepEqual(bot.sent, [{ kind: 'chat', args: ['a /b'] }])
})

test('five single-line messages in 30 s pass and the sixth is blocked with retryMs', async () => {
  let t = 1000
  const limiter = createLimiter({ gapMs: 0, now: () => t })
  const bot = chatBot()
  for (let i = 0; i < 5; i++) {
    assert.equal((await say(bot, ctx(), { message: `m${i}` }, { timeScale: 0.01, limiter })).status, 'sent')
    t += 1000
  }
  const r = await say(bot, ctx(), { message: 'm5' }, { timeScale: 0.01, limiter })
  assert.deepEqual(r, { status: 'blocked', reason: 'rate', retryMs: 25000 })
  assert.equal(bot.sent.length, 5)
  t += 25000
  assert.equal((await say(bot, ctx(), { message: 'm6' }, { timeScale: 0.01, limiter })).status, 'sent')
})

test('a message with more parts than the window holds is too-long and sends nothing', async () => {
  const bot = chatBot()
  const r = await say(bot, ctx(), { message: 'x'.repeat(CHAT_MAX * 5 + 1) }, { timeScale: 0.01, limiter: createLimiter({ gapMs: 0 }) })
  assert.deepEqual(r, { status: 'cannot', reason: 'too-long', parts: 6 })
  assert.deepEqual(bot.sent, [])
})

test('parts that do not fit the window right now are blocked and send nothing', async () => {
  let t = 0
  const limiter = createLimiter({ gapMs: 0, now: () => t })
  const bot = chatBot()
  for (let i = 0; i < 3; i++) await say(bot, ctx(), { message: 'a' }, { timeScale: 0.01, limiter })
  const sentBefore = bot.sent.length
  const r = await say(bot, ctx(), { message: 'x'.repeat(CHAT_MAX * 3) }, { timeScale: 0.01, limiter })
  assert.deepEqual(r, { status: 'blocked', reason: 'rate', retryMs: 30000 })
  assert.equal(bot.sent.length, sentBefore)
})

test('the gap between lines is waited for, across calls too', async () => {
  const stamps = []
  const bot = chatBot()
  const chat = bot.chat
  bot.chat = (...args) => { stamps.push(Date.now()); chat(...args) }
  const limiter = createLimiter({ gapMs: 80 })
  await say(bot, ctx(), { message: 'x'.repeat(CHAT_MAX + 5) }, { timeScale: 0.01, limiter })
  await say(bot, ctx(), { message: 'next' }, { timeScale: 0.01, limiter })
  assert.equal(stamps.length, 3)
  assert.ok(stamps[1] - stamps[0] >= 78 && stamps[2] - stamps[1] >= 78, stamps.join())
})

test('createLimiter wouldAllow counts only lines inside the window', () => {
  let t = 0
  const l = createLimiter({ now: () => t })
  for (let i = 0; i < 4; i++) l.record()
  assert.deepEqual([l.wouldAllow(1), l.wouldAllow(2)], [true, false])
  t = 30000
  assert.equal(l.wouldAllow(5), true)
})

test('a later part that starts with a slash refuses the whole public message and sends nothing', async () => {
  const bot = chatBot()
  const text = `${'a'.repeat(254)} /tell Someone hi`
  assert.ok(text.length > CHAT_MAX)
  assert.deepEqual(await say(bot, ctx(), { message: text }, OPTS()), { status: 'cannot', reason: 'command' })
  assert.deepEqual(bot.sent, [])
})

test('a later whisper part that starts with a slash is refused too', async () => {
  const bot = chatBot()
  const text = `${'a'.repeat(244)} /op me`
  assert.deepEqual(await say(bot, ctx(), { message: text, to: 'Steve' }, OPTS()), { status: 'cannot', reason: 'command' })
  assert.deepEqual(bot.sent, [])
})

test('no public part ever begins with a slash across split messages', async () => {
  const bot = chatBot()
  const messages = [long, 'word '.repeat(120), 'x'.repeat(500), `${'a'.repeat(100)}. ${'b'.repeat(200)}`]
  for (const message of messages) await say(bot, ctx(), { message }, OPTS())
  assert.ok(bot.sent.length > 4)
  assert.ok(bot.sent.every(s => !s.args[0].startsWith('/')))
})

test('say refuses bad whisper targets and prototype names', async () => {
  const cases = [
    ['constructor', { status: 'gone', to: 'constructor' }],
    ['toString', { status: 'gone', to: 'toString' }],
    ['__proto__', { status: 'gone', to: '__proto__' }],
    ['@a', { status: 'cannot', reason: 'bad-name' }],
    ['Name extra', { status: 'cannot', reason: 'bad-name' }],
    ['ab', { status: 'cannot', reason: 'bad-name' }]
  ]
  for (const [to, expected] of cases) {
    const bot = chatBot()
    assert.deepEqual(await say(bot, ctx(), { message: 'hi', to }, OPTS()), expected, to)
    assert.deepEqual(bot.sent, [], to)
  }
})

test('every whisper part fits the budget with the validated name', async () => {
  const bot = chatBot({ players: { Steve_1234567890: {} } })
  await say(bot, ctx(), { message: 'word '.repeat(100), to: 'Steve_1234567890' }, OPTS())
  assert.ok(bot.sent.length > 1)
  assert.ok(bot.sent.every(s => `/tell ${s.args[0]} ${s.args[1]}`.length <= CHAT_MAX))
})
