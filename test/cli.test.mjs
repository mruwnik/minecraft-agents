// The one-line result renderer that ./mc prints (src/cli.mjs, imported by nothing but tools/mc.mjs and lib.mjs)
import test from 'node:test'
import assert from 'node:assert/strict'
import { terse, parseCliArgs, mapArgErrors, MAP_ARG_HELP, waitReport } from '../src/cli.mjs'

// `eat` answers ate= and gained= (AGENT_GUIDE), the same names a long result renders as +item:n and ate=item:n. The short
// renderer dropped both as long-result bookkeeping: Chani ate at food 7 and read `ok food=10 health=10`, then reported
// "eat returns ok without eating anything" (13:53Z); my carrot went 3 -> 2 and the line said only `ok food=20 health=20`
for (const [name, result, expected] of [
  ['eat says what it ate and what it gained', { ok: true, ate: 'carrot', gained: 3, food: 20, health: 20 }, 'ok ate=carrot gained=3 food=20 health=20'],
  ['a meal the plugin chose still names the food', { ok: true, ate: 'bread', gained: 5, food: 12, health: 9 }, 'ok ate=bread gained=5 food=12 health=9'],
  ['a meal that gained nothing says so (the food number can lag the meal)', { ok: true, ate: 'carrot', gained: 0, food: 20, health: 20 }, 'ok ate=carrot gained=0 food=20 health=20']
]) {
  test(`terse (short result): ${name}`, () => assert.equal(terse(result), expected))
}
// a long result's counts keep their signs and their place
test('terse (long result): gained, lost and ate keep the +/- notation', () =>
  assert.equal(terse({ ok: true, task: 3, action: 'goto', seconds: 4, gained: { mutton: 1 }, lost: { bread: 1 }, ate: { bread: 1 }, pos: { x: 1, y: 2, z: 3 } }), 'ok goto 4s +mutton:1 -bread:1 ate=bread:1 @1,2,3'))

// ---------------------------------------------------------------- items= and its shorthand
// `withdraw items=bread:7,oak_planks:2` used to reach the body as a string, which withdrawPlan walked character by
// character ("6:0/7 19:0/2"). The CLI now reads that shorthand as the JSON it stands for, and a string that is
// neither is refused before anything is sent.
const argRows = [
  ['the JSON spelling', ["items={\"bread\":7,\"oak_planks\":2}"], { items: { bread: 7, oak_planks: 2 } }],
  ['the shorthand spelling', ['items=bread:7,oak_planks:2'], { items: { bread: 7, oak_planks: 2 } }],
  ['one item in shorthand', ['items=bread:1'], { items: { bread: 1 } }],
  ['shorthand without a count means all of it', ['items=cobblestone,dirt:4'], { items: { cobblestone: 'all', dirt: 4 } }],
  ['shorthand with all spelled out', ['items=cobblestone:all'], { items: { cobblestone: 'all' } }],
  ['the long list form passes through', ['items=[{"name":"coal","count":4}]'], { items: [{ name: 'coal', count: 4 }] }],
  ['other arguments are untouched', ['x=1', 'item=coal', 'count=4', 'topicword'], { x: 1, item: 'coal', count: 4, topic: 'topicword' }],
  ['a garbage string stays a string for mapArgErrors to refuse', ['items=bread 7 please'], { items: 'bread 7 please' }]
]
for (const [name, argv, expected] of argRows) {
  test(`parseCliArgs: ${name}`, () => { assert.deepEqual(parseCliArgs(argv), expected) })
}

const errorRows = [
  ['a map', { items: { bread: 7 } }, null],
  ['a list', { items: [{ name: 'bread', count: 7 }] }, null],
  ['no items at all', { item: 'bread', count: 7 }, null],
  ['a garbage string', { items: 'bread 7 please' }, MAP_ARG_HELP('items')],
  ['a number', { items: 7 }, MAP_ARG_HELP('items')],
  ['a shorthand that never went through the CLI', { items: 'bread:7' }, MAP_ARG_HELP('items')]
]
for (const [name, args, expected] of errorRows) {
  test(`mapArgErrors: ${name}`, () => { assert.equal(mapArgErrors(args), expected) })
}

test('MAP_ARG_HELP: says what items= wants, with an example of both spellings', () => {
  assert.equal(MAP_ARG_HELP('items'), `items= wants JSON, e.g. items='{"bread":7,"oak_planks":2}' (or the shorthand items=bread:7,oak_planks:2)`)
})

// ---------------------------------------------------------------- waitReport: chattiness (card 2e032c4a)
// no chat= at all is today's behaviour: chattiness defaults to 1, which hears everything (the lowest weight is 0.1)
const chatLine = (from, message, type = 'chat') => JSON.stringify({ seq: 1, t: '2026-09-26T18:00:00Z', type, from, message }) + '\n'
const at = Date.parse('2026-09-26T18:00:01Z')

test('waitReport: with no chat config, a greeting still wakes the wait (unchanged default behaviour)', () =>
  assert.deepEqual(waitReport(chatLine('Chani', 'good morning'), 'Jizo', at).lines, ['chat from=Chani message=good morning']))

const chattinessRows = [
  ['chattiness 1 hears a greeting', 'good morning', { chattiness: 1 }, ['chat from=Chani message=good morning']],
  ['chattiness 0.5 does not hear a greeting, and says so', 'good morning', { chattiness: 0.5 },
    ['skipped 1 chat line below your chattiness (0.5): ./mc events type=chat n=1']],
  ['chattiness 0.5 hears a question naming me', 'Jizo, are you there?', { chattiness: 0.5 }, ['chat from=Chani message=Jizo, are you there?']],
  ['chattiness 0 hears nothing from open chat', 'creeper!', { chattiness: 0 },
    ['skipped 1 chat line below your chattiness (0): ./mc events type=chat n=1']],
  ['a denied sender is skipped even asking a question', 'help, where are you?', { chattiness: 1, deny: ['Chani'] },
    ['skipped 1 chat line below your chattiness (1): ./mc events type=chat n=1']]
]
for (const [name, message, chat, lines] of chattinessRows) {
  test(`waitReport: ${name}`, () => assert.deepEqual(waitReport(chatLine('Chani', message), 'Jizo', at, chat).lines, lines))
}

test('waitReport: a whisper still wakes at chattiness 0 (whisper to me always weighs 1)', () =>
  assert.deepEqual(waitReport(chatLine('Chani', 'hi', 'whisper'), 'Jizo', at, { chattiness: 0 }).lines, ['whisper from=Chani message=hi']))

test('waitReport: deny reaches a whisper too, since that is what deny is for', () =>
  assert.deepEqual(waitReport(chatLine('Chani', 'hi', 'whisper'), 'Jizo', at, { chattiness: 1, deny: ['Chani'] }).lines,
    ['skipped 1 chat line below your chattiness (1): ./mc events type=chat n=1']))

test('waitReport: a plain sender not in the agent roster gets the human bonus (0.8), heard at chattiness 0.5', () =>
  assert.deepEqual(waitReport(chatLine('Steve', 'the wheat is ripe'), 'Jizo', at, { chattiness: 0.5 }, ['Jizo', 'Chani']).lines,
    ['chat from=Steve message=the wheat is ripe']))

test('waitReport: the same plain line from an agent (in the roster) is not heard at chattiness 0.5', () =>
  assert.deepEqual(waitReport(chatLine('Chani', 'the wheat is ripe'), 'Jizo', at, { chattiness: 0.5 }, ['Jizo', 'Chani']).lines,
    ['skipped 1 chat line below your chattiness (0.5): ./mc events type=chat n=1']))

test('waitReport: skipped chat lines are counted alongside a wake-worthy line that is not chat at all', () => {
  const text = chatLine('Chani', 'good morning') + JSON.stringify({ seq: 2, t: '2026-09-26T18:00:02Z', type: 'dawn' }) + '\n'
  assert.deepEqual(waitReport(text, 'Jizo', at, { chattiness: 0.5 }).lines,
    ['dawn', 'skipped 1 chat line below your chattiness (0.5): ./mc events type=chat n=1'])
})

// ---------------------------------------------------------------- autopilot: a routine that stopped and a body that is stuck wake the wait (autopilot card)
const autopilotLine = (type, data) => JSON.stringify({ seq: 1, t: new Date(at).toISOString(), type, ...data }) + '\n'
for (const [name, type, data, expected] of [
  ['routine_stopped', 'routine_stopped', { reason: 'health 6', step: 'farm.tidy place=a', place: 'a', advice: 'eat' }, ['routine_stopped reason=health 6 step=farm.tidy place=a place=a advice=eat']],
  ['stuck', 'stuck', { pos: { x: 1, y: 2, z: 3 }, reason: 'boxed in for 3 min', advice: 'dig out' }, ['stuck pos={"x":1,"y":2,"z":3} reason=boxed in for 3 min advice=dig out']],
  ['routine_day is news, not a wake-up', 'routine_day', { day: 2, places: {} }, []]
]) {
  test(`waitReport: ${name}`, () => assert.deepEqual(waitReport(autopilotLine(type, data), 'Jizo', at).lines, expected))
}
