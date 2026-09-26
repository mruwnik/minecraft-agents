// The body-side stuck watch (autopilot card): a sample a second, a verdict over the rolling window, one event and one
// chat line per episode. Pure: bot.mjs takes the samples and hands them in.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { addSample, stuckVerdict, nextEpisode, stuckField, stuckLine, WINDOW_MS, END_MS, REPEAT_MS } from '../src/stuck.mjs'

const T0 = 1_000_000_000
const sec = n => T0 + n * 1000
const base = {
  pos: { x: 10, y: 64, z: -20 },
  taskId: null,
  taskName: null,
  taskProgress: 0,
  sleeping: false,
  night: false,
  health: 20,
  food: 20,
  edible: true,
  oxygen: 20,
  holedUp: false,
  buried: false,
  boxed: false,
  frozenWalks: 0,
  routine: null
}
// a sample a second from second 0 to `seconds`, each one `base` with the overrides `at(i)` gives for second i
const series = (seconds, at = () => ({})) => Array.from({ length: seconds + 1 }, (_, i) => ({ ...base, t: sec(i), ...at(i) }))
const working = { taskId: 7, taskName: 'farm.maintain' }
const routineAt = (day, failedSteps = [], phase = 'steps') => ({ taskId: 9, taskName: 'routine', routine: { lastDayStartedAt: sec(0), day, failedSteps, phase } })

const kinds = samples => stuckVerdict(samples)?.kind ?? null

for (const [name, samples, expected] of [
  ['an idle body standing still for an hour', series(3600), null],
  ['a task walking on', series(200, i => ({ ...working, pos: { x: 10 + i * 0.2, y: 64, z: -20 } })), null],
  ['a task standing still, 179 s', series(179, () => working), null],
  ['a task standing still, 180 s', series(180, () => working), 'still'],
  ['a task standing still but stepping through composite steps', series(300, i => ({ ...working, taskProgress: Math.floor(i / 30) })), null],
  ['a task standing still that began 100 s ago (the earlier still time was another task)', series(300, i => (i < 200 ? { taskId: 6, taskName: 'goto' } : working)), null],
  ['a task that only bobs on the spot (under 0.5 of a block)', series(200, i => ({ ...working, pos: { x: 10 + (i % 2) * 0.3, y: 64, z: -20 } })), 'still'],
  ['a body asleep in a task (the night checkpoint)', series(600, () => ({ ...working, sleeping: true, night: true })), null],
  ['a body that woke 100 s ago and stands still since', series(600, i => (i < 500 ? { ...working, sleeping: true } : working)), null],
  ['a routine waiting for dusk stands still by design', series(900, () => routineAt(1, [], 'dusk')), null],
  ['a routine waiting for dawn stands still by design', series(900, () => routineAt(1, [], 'dawn')), null],
  ['a routine standing still mid-steps', series(180, () => routineAt(1, [], 'steps')), 'still'],
  ['an idle body boxed in for 179 s', series(179, () => ({ boxed: true })), null],
  ['an idle body boxed in for 180 s', series(180, () => ({ boxed: true })), 'boxed'],
  ['a routine waiting for dusk, boxed in', series(300, () => ({ ...routineAt(1, [], 'dusk'), boxed: true })), 'boxed'],
  ['boxed in at night in its own hole (a shelter, not a trap)', series(600, () => ({ boxed: true, holedUp: true, night: true })), null],
  ['boxed in at night in a hole dug by hand and capped over (no holedUp flag: the driver did it)', series(600, () => ({ boxed: true, night: true })), null],
  ['boxed in through dusk: the daylight part alone is under 3 minutes', series(300, i => ({ boxed: true, night: i > 120 })), null],
  ['boxed in at night, then still boxed 3 minutes after dawn', series(600, i => ({ boxed: true, night: i < 400 })), 'boxed'],
  ['boxed in and asleep', series(600, () => ({ boxed: true, sleeping: true, night: true })), null],
  ['boxed in while digging down a shaft (the position changes)', series(300, i => ({ ...working, boxed: true, pos: { x: 10, y: 64 - Math.floor(i / 20), z: -20 } })), null],
  ['boxed in while walling itself in with one-second place tasks (a new task id every few seconds)', series(400, i => ({ boxed: true, ...(i % 40 < 2 ? { taskId: 40 + Math.floor(i / 40), taskName: 'place' } : {}) })), 'boxed'],
  ['still for 5 minutes, then walled in 10 s ago: the boxed count starts when the wall closed', series(300, i => ({ boxed: i >= 290 })), null],
  ['boxed in 5 minutes with one step sideways 60 s ago (a 2-cell pen): the count starts again', series(300, i => ({ boxed: true, pos: { x: i < 240 ? 11 : 10, y: 64, z: -20 } })), null],
  ['a task still for 5 minutes with one nudge of a block 100 s ago', series(300, i => ({ ...working, pos: { x: i < 200 ? 11 : 10, y: 64, z: -20 } })), null],
  ['two frozen walks in 5 minutes', series(300, i => ({ ...working, frozenWalks: i < 100 ? 0 : i < 200 ? 1 : 2, pos: { x: 10 + i, y: 64, z: -20 } })), null],
  ['three frozen walks in 5 minutes', series(300, i => ({ ...working, frozenWalks: i < 100 ? 0 : i < 200 ? 1 : i < 250 ? 2 : 3, pos: { x: 10 + i, y: 64, z: -20 } })), 'frozen'],
  ['three frozen walks, but spread over 8 minutes', series(480, i => ({ ...working, frozenWalks: i < 10 ? 0 : i < 200 ? 1 : i < 400 ? 2 : 3, pos: { x: 10 + i, y: 64, z: -20 } })), null],
  ['three frozen walks in the first 4 minutes of a young window', series(240, i => ({ ...working, frozenWalks: Math.min(3, Math.floor(i / 60)), pos: { x: 10 + i, y: 64, z: -20 } })), 'frozen'],
  ['holed up by day for 299 s', series(299, () => ({ holedUp: true })), null],
  ['holed up by day for 300 s', series(300, () => ({ holedUp: true })), 'holed'],
  ['holed up by night for 20 minutes (the hole is the point)', series(1200, () => ({ holedUp: true, night: true })), null],
  ['holed up over dusk: only the daylight part counts', series(400, i => ({ holedUp: true, night: i > 200 })), null],
  ['buried by day for 300 s', series(300, () => ({ buried: true })), 'buried'],
  ['health 7 with nothing edible', series(1, () => ({ health: 7, edible: false })), null],
  ['health 6 with nothing edible', series(1, () => ({ health: 6, edible: false })), 'health'],
  ['health 6 with bread in the pockets (the body eats by itself)', series(1, () => ({ health: 6, edible: true })), null],
  ['health 2 with nothing edible, at once', series(0, () => ({ health: 2, edible: false })), 'health'],
  ['oxygen falling for 19 s', series(19, i => ({ oxygen: 20 - i })), null],
  ['oxygen falling for 20 s', series(20, i => ({ oxygen: 20 - i })), 'oxygen'],
  ['oxygen falling for 25 s with the same reading twice (the bar drops in steps)', series(25, i => ({ oxygen: 20 - Math.floor(i / 2) })), 'oxygen'],
  ['oxygen that dipped and came back (a swim up for air)', series(25, i => ({ oxygen: i < 12 ? 20 - i : 8 + (i - 12) })), null],
  ['oxygen steady at full for a minute', series(60), null],
  ['oxygen low but steady (standing in a bubble column?)', series(60, () => ({ oxygen: 5 })), null],
  ['the same step failing two days running', series(1, () => routineAt(2, [{ day: 1, step: 'farm.tidy place=a' }, { day: 2, step: 'farm.tidy place=a' }])), null],
  ['the same step failing three days running', series(1, () => routineAt(3, [{ day: 1, step: 'farm.tidy place=a' }, { day: 2, step: 'farm.tidy place=a' }, { day: 3, step: 'farm.tidy place=a' }])), 'step'],
  ['three different steps failing on three days', series(1, () => routineAt(3, [{ day: 1, step: 'farm.tidy place=a' }, { day: 2, step: 'farm.compost place=a' }, { day: 3, step: 'farm.maintain place=a' }])), null],
  ['the same step failing on days 1, 2 and 4 (day 3 went)', series(1, () => routineAt(4, [{ day: 1, step: 'farm.tidy place=a' }, { day: 2, step: 'farm.tidy place=a' }, { day: 4, step: 'farm.tidy place=a' }])), null],
  ['the same step failing on days 2, 3 and 4 after a good day 1', series(1, () => routineAt(4, [{ day: 2, step: 'farm.tidy place=b' }, { day: 3, step: 'farm.tidy place=b' }, { day: 4, step: 'farm.tidy place=b' }])), 'step'],
  ['a routine whose last day began 29 minutes ago, still stepping', series(1740, i => ({ ...routineAt(1), pos: { x: 10 + i, y: 64, z: -20 } })), null],
  ['a routine whose last day began 30 minutes ago', series(1800, i => ({ ...routineAt(1), pos: { x: 10 + i, y: 64, z: -20 } })), 'day'],
  ['a routine whose day began 30 minutes ago while waiting for dusk', series(1800, () => ({ ...routineAt(1, [], 'dusk') })), 'day'],
  ['no samples at all', [], null]
]) {
  test(`stuckVerdict: ${name}`, () => assert.equal(kinds(samples), expected))
}

// the most urgent reason wins when several hold
for (const [name, samples, expected] of [
  ['drowning beats standing still', series(200, i => ({ ...working, oxygen: 20 - i * 0.05 })), 'oxygen'],
  ['low health beats a frozen walk', series(300, i => ({ ...working, health: 4, edible: false, frozenWalks: Math.min(3, Math.floor(i / 60)) })), 'health'],
  ['buried beats standing still', series(400, () => ({ ...working, buried: true })), 'buried']
]) {
  test(`stuckVerdict: ${name}`, () => assert.equal(kinds(samples), expected))
}

// what the verdict says: a reason short enough for a chat line and the state field, and advice a driver can act on
for (const [name, samples, reason, advice] of [
  ['still', series(180, () => working), 'no movement and no progress in farm.maintain for 3 min', /stop it, step two blocks away/],
  ['boxed', series(180, () => ({ boxed: true })), 'boxed in for 3 min', /dig or open a way out/],
  ['frozen', series(300, i => ({ ...working, frozenWalks: Math.min(3, Math.floor(i / 60)), pos: { x: 10 + i, y: 64, z: -20 } })), '3 walks froze in 5 min', /events type=frozen_walk last=3/],
  ['holed', series(300, () => ({ holedUp: true })), 'holed up 5 min by day', /dig the block over the head/],
  ['buried', series(300, () => ({ buried: true })), 'buried 5 min by day', /dig the block over the head/],
  ['health', series(0, () => ({ health: 5, edible: false })), 'health 5, nothing edible carried', /food before anything else/],
  ['oxygen', series(20, i => ({ oxygen: 20 - i })), 'oxygen falling 20 s', /surface/],
  ['step', series(1, () => routineAt(3, [1, 2, 3].map(day => ({ day, step: 'farm.compost place=a' })))), 'farm.compost place=a failed 3 days running', /by hand/],
  ['day', series(1800, i => ({ ...routineAt(1), pos: { x: 10 + i, y: 64, z: -20 } })), 'no routine day started in 30 min', /stop and start it again/]
]) {
  test(`stuckVerdict: ${name} reason and advice`, () => {
    const verdict = stuckVerdict(samples)
    assert.equal(verdict.reason, reason)
    assert.match(verdict.advice, advice)
  })
}

// one episode per condition: said once when it starts, held while it lasts and through a blip of the verdict, over
// (stuck_end) once the verdict has been clear for END_MS, and not said again for a repeat within REPEAT_MS of its start
const still = { kind: 'still', reason: 'no movement and no progress in goto for 3 min', advice: 'a' }
const boxed = { kind: 'boxed', reason: 'boxed in for 3 min', advice: 'b' }
const live = (v, since, lastSeen = since) => ({ ...v, since, lastSeen, quiet: false })
const over = (v, since, at) => ({ ...live(v, since, at - END_MS), over: at })
const none = { started: false, ended: false }
for (const [name, episode, verdict, now, expected] of [
  ['nothing before, nothing now', null, null, sec(5), { episode: null, ...none }],
  ['an episode begins', null, still, sec(5), { episode: live(still, sec(5)), started: true, ended: false }],
  ['the same condition holds: nothing new to say, the episode is seen again', live(still, sec(1)), still, sec(5), { episode: live(still, sec(1), sec(5)), ...none }],
  ['the reason wording changes within the kind (health 6 -> 5): the same episode', live(still, sec(1)), { ...still, reason: 'other words' }, sec(5), { episode: live(still, sec(1), sec(5)), ...none }],
  ['another kind takes over: a new episode', live(still, sec(1)), boxed, sec(5), { episode: live(boxed, sec(5)), started: true, ended: false }],
  ['the verdict clears for a moment (a nudge, a task a second long): the episode holds', live(boxed, sec(1), sec(10)), null, sec(20), { episode: live(boxed, sec(1), sec(10)), ...none }],
  ['the verdict has been clear for END_MS: the episode is over, said once', live(boxed, sec(1), sec(10)), null, sec(10) + END_MS, { episode: over(boxed, sec(1), sec(10) + END_MS), started: false, ended: true }],
  ['an episode already over stays over, quietly', over(boxed, sec(1), sec(40)), null, sec(90), { episode: over(boxed, sec(1), sec(40)), ...none }],
  ['the same kind returns within REPEAT_MS of the last start: held, not announced', over(boxed, sec(1), sec(40)), boxed, sec(60), { episode: { ...live(boxed, sec(60)), quiet: true }, ...none }],
  ['a quiet episode that ends says nothing either', { ...live(boxed, sec(60), sec(70)), quiet: true }, null, sec(70) + END_MS, { episode: { ...live(boxed, sec(60), sec(70)), quiet: true, over: sec(70) + END_MS }, ...none }],
  ['the same kind returns after REPEAT_MS: a new episode, announced', over(boxed, sec(1), sec(40)), boxed, sec(1) + REPEAT_MS, { episode: live(boxed, sec(1) + REPEAT_MS), started: true, ended: false }],
  ['another kind after an episode is over: announced at once', over(boxed, sec(1), sec(40)), still, sec(60), { episode: live(still, sec(60)), started: true, ended: false }]
]) {
  test(`nextEpisode: ${name}`, () => assert.deepEqual(nextEpisode(episode, verdict, now), expected))
}

// ClaudeProbe's three alerts in 15 s (09-26 21:58Z), replayed tick by tick: boxed and still throughout, a one-second
// place task every 40 s. One `stuck`, no repeat, and no stuck_end while it lasts
test('nextEpisode: a run of verdicts over a body walling itself in says stuck exactly once', () => {
  let episode = null
  const said = []
  for (let i = 0; i <= 900; i++) {
    const samples = series(i, j => ({ boxed: true, ...(j % 40 < 2 ? { taskId: 40 + Math.floor(j / 40), taskName: 'place' } : {}) }))
    const next = nextEpisode(episode, stuckVerdict(samples), sec(i))
    episode = next.episode
    if (next.started) said.push(`stuck ${i}`)
    if (next.ended) said.push(`stuck_end ${i}`)
  }
  assert.deepEqual(said, ['stuck 180'])
})

for (const [name, episode, expected] of [
  ['no episode', null, undefined],
  ['a live episode', live(boxed, sec(1)), 'boxed in for 3 min'],
  ['a quiet repeat still shows', { ...live(boxed, sec(1)), quiet: true }, 'boxed in for 3 min'],
  ['an episode that is over', over(boxed, sec(1), sec(40)), undefined]
]) {
  test(`stuckField: ${name}`, () => assert.equal(stuckField(episode), expected))
}

test('stuckLine: the one chat line, with the position floored', () =>
  assert.equal(stuckLine({ x: 10.7, y: 64.2, z: -20.3 }, 'boxed in for 3 min'), 'stuck at 10,64,-21: boxed in for 3 min'))

for (const [name, samples, sample, expectedTs] of [
  ['the first sample', [], { t: sec(0) }, [sec(0)]],
  ['samples within the window are kept', series(10), { t: sec(11) }, series(11).map(s => s.t)],
  ['a sample older than the window falls off', [{ t: sec(0) }, { t: sec(1) }], { t: sec(0) + WINDOW_MS + 1 }, [sec(1), sec(0) + WINDOW_MS + 1]]
]) {
  test(`addSample: ${name}`, () => assert.deepEqual(addSample(samples, sample).map(s => s.t), expectedTs))
}
