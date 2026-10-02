// The one-line result renderer that ./mc prints (src/cli.mjs, imported by nothing but tools/mc.mjs and lib.mjs)
import test from 'node:test'
import assert from 'node:assert/strict'
import { terse, parseCliArgs, mapArgErrors, MAP_ARG_HELP, waitReport, wakeWorthy, eventLines, renderVerbose, VERBOSE_LIMIT, capOutput } from '../src/cli.mjs'

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

// `./mc eat` queues a job and carries `action` on its result once it completes (executeAcceptedJob), but never
// `seconds`: it is not a composite task. terse() used to treat any `action` as the long-result shape and ran
// signed() (an Object.entries map walk) over eat's plain ate= string, reading "bread" as {0:'b',1:'r',...} and
// printing "undefineds ate=0:b,1:r,...": a driver reading `./mc job id=` or `./mc wait job=` for an eat job saw
// garbage instead of what it ate (2026-10-02)
test('terse (job result): a quick job action (eat) carries `action` with no `seconds`, and ate/gained stay scalars', () =>
  assert.equal(terse({ ok: true, job: 7, action: 'eat', ate: 'bread', gained: 2, food: 12, health: 9 }), 'ok job=7 ate=bread gained=2 food=12 health=9'))

// farm.harvest IS a composite task (real `seconds`), but `lost` there is a sentence naming what never reached the
// pockets (#142), not a count map like mealTally's. The same signed() walk read it character by character too:
// "-0:2 -1:  -2:s -3:t ..." for "2 still lying (...)". A non-map lost/gained/ate reads as key=value instead
test('terse (long result): a prose `lost` (farm.harvest) reads as lost=, not exploded character by character', () =>
  assert.equal(
    terse({ ok: true, task: 9, action: 'farm.harvest', seconds: 12, harvested: { wheat: 3 }, lost: '2 still lying (wheat at 0,64,0): I stood on it and it did not come' }),
    'ok farm.harvest 12s lost=2 still lying (wheat at 0,64,0): I stood on it and it did not come harvested(wheat:3)'
  ))

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

test('job wake policy keeps routine progress quiet, exposes verbose progress, and avoids duplicate success wakeups', () => {
  assert.equal(wakeWorthy({ type: 'job_progress', verbose: false }), false)
  assert.equal(wakeWorthy({ type: 'job_waiting', verbose: false }), false)
  assert.equal(wakeWorthy({ type: 'job_progress', verbose: true }), true)
  assert.equal(wakeWorthy({ type: 'job_waiting', verbose: true }), true)
  assert.equal(wakeWorthy({ type: 'task_done', job: 9, notify: false }), false) // scheduler compatibility alias
  assert.equal(wakeWorthy({ type: 'task_cancelled', job: 9, notify: false }), false) // cancellation has not settled yet
  assert.equal(wakeWorthy({ type: 'task_done', action: 'legacy-long' }), true) // older non-job lifecycle
  assert.equal(wakeWorthy({ type: 'job_completed', notify: false }), false) // automatic bedtime success
  assert.equal(wakeWorthy({ type: 'job_completed', notify: true }), true)
  assert.equal(wakeWorthy({ type: 'job_failed' }), true)
  assert.equal(wakeWorthy({ type: 'job_cancelled' }), true)
  assert.equal(wakeWorthy({ type: 'whisper', from: 'Steve', to: 'Jizo' }, 'Jizo'), true)
  assert.equal(wakeWorthy({ type: 'jobs_discard_refused', reason: 'job 4 was interrupted by disconnected: EPIPE' }), true)
})

test('waitReport lists only notifications that can end a wait', () => {
  const line = (seq, type, extra = {}) => `${JSON.stringify({ seq, t: new Date(at).toISOString(), type, ...extra })}\n`
  const events = line(1, 'job_progress', { id: 8, verbose: false, progress: { round: 10 } }) +
    line(2, 'job_waiting', { id: 8, verbose: false, reason: 'night' }) +
    line(3, 'task_done', { job: 8, notify: false }) +
    line(4, 'job_progress', { id: 9, verbose: true, progress: { round: 1 } }) +
    line(5, 'job_completed', { id: 8, notify: true }) +
    line(6, 'job_completed', { id: 10, notify: false })
  assert.deepEqual(waitReport(events, 'Jizo', at).lines, ['job_progress id=9 verbose progress={"round":1}', 'job_completed id=8 notify'])
})

test('waitReport does not wake an owned long job for ordinary night/day transitions', () => {
  const line = (seq, type) => `${JSON.stringify({ seq, t: new Date(at).toISOString(), type })}\n`
  const events = line(1, 'night_fell') + line(2, 'dawn') + line(3, 'woke_up') + line(4, 'job_progress') + line(5, 'job_failed')
  assert.deepEqual(waitReport(events, 'Jizo', at, {}, [], { jobActive: true }).lines, ['job_failed'])
  assert.deepEqual(waitReport(line(6, 'dawn'), 'Jizo', at).lines, ['dawn'])
})

// ---------------------------------------------------------------- ./mc jobs / ./mc events: last=20 by default, capped -v (card: a 34k-char jobs -v)
test('eventLines defaults to the last 20, respects a type filter, and all=true returns the whole tail', () => {
  const events = Array.from({ length: 25 }, (_, i) => ({ seq: i, t: '2026-10-02T12:00:00Z', type: i % 5 === 0 ? 'died' : 'tick', note: `e${i}` }))
  const defaulted = eventLines(events)
  assert.equal(defaulted.length, 20)
  assert.equal(defaulted[0], '12:00:00 died note=e5') // the last 20 of 25: index 5 is where it starts
  assert.equal(eventLines(events, { all: true }).length, 25)
  assert.deepEqual(eventLines(events, { type: 'died' }), [0, 5, 10, 15, 20].map(i => `12:00:00 died note=e${i}`))
  assert.equal(eventLines(events, { last: 3 }).length, 3)
})

test('eventLines leaves an empty tail as an empty list, not an error', () => assert.deepEqual(eventLines([]), []))

test('renderVerbose caps at VERBOSE_LIMIT with the same cut-and-say-so tail as capOutput, never unbounded', () => {
  const huge = { jobs: Array.from({ length: 2000 }, (_, i) => ({ id: i, name: 'goto', args: { x: i } })) }
  const rendered = renderVerbose(huge)
  assert.ok(JSON.stringify(huge, null, 1).length > VERBOSE_LIMIT) // the input really is bigger than the cap
  assert.equal(rendered, capOutput(JSON.stringify(huge, null, 1), VERBOSE_LIMIT))
  assert.ok(rendered.length <= VERBOSE_LIMIT + 200) // the cap plus its own short "[+N cut]" tail, never the raw size
  assert.match(rendered, /chars cut/)
})

test('renderVerbose leaves small output alone', () => assert.equal(renderVerbose({ ok: true }), '{\n "ok": true\n}'))

// ---------------------------------------------------------------- autopilot: a routine that stopped and a body that is stuck wake the wait (autopilot card)
const autopilotLine = (type, data) => JSON.stringify({ seq: 1, t: new Date(at).toISOString(), type, ...data }) + '\n'
for (const [name, type, data, expected] of [
  ['routine_stopped', 'routine_stopped', { reason: 'health 6', step: 'farm.tidy place=a', place: 'a', advice: 'eat' }, ['routine_stopped reason=health 6 step=farm.tidy place=a place=a advice=eat']],
  ['stuck', 'stuck', { pos: { x: 1, y: 2, z: 3 }, reason: 'boxed in for 3 min', advice: 'dig out' }, ['stuck pos={"x":1,"y":2,"z":3} reason=boxed in for 3 min advice=dig out']],
  ['farm storage needs an agent decision', 'farm_attention', { place: 'field', reason: 'storage unavailable', advice: 'choose storage' }, ['farm_attention place=field reason=storage unavailable advice=choose storage']],
  ['routine_day is news, not a wake-up', 'routine_day', { day: 2, places: {} }, []]
]) {
  test(`waitReport: ${name}`, () => assert.deepEqual(waitReport(autopilotLine(type, data), 'Jizo', at).lines, expected))
}
