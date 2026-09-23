import test from 'node:test'
import assert from 'node:assert/strict'
import fs from 'node:fs'
import path from 'node:path'
import { fakeApi } from './helpers.mjs'
import maintainFarm from '../library/farm/maintain.mjs'
import buildFarm from '../library/farm/build.mjs'
import hunt from '../library/hunt.mjs'
import farmFindSpot from '../library/farm/find_spot.mjs'
import buildPen from '../library/pen/build.mjs'
import bringPair from '../library/flock/bring_pair.mjs'
import flockMaintain from '../library/flock/maintain.mjs'
import compost from '../library/farm/compost.mjs'
import routine from '../library/routine.mjs'
import getSeeds from '../library/farm/get_seeds.mjs'
import farmPlan from '../library/farm/plan.mjs'
import farmFields from '../library/farm/fields.mjs'
import collect from '../library/collect.mjs'
import farmHarvest from '../library/farm/harvest.mjs'
import mineGet from '../library/mine/get.mjs'
import flockBreed from '../library/flock/breed.mjs'
import flockLead from '../library/flock/lead.mjs'
import { tillWarning, planAnchor, planBeside, penOpenRefusal, penProbes, planStructure, PLAN_LEGEND, COMPOST_CHANCE, RENAMED, renamedList, placeMissed, strayFluid, penInside, insideCount, pairPlan, placeTarget, flockPlan, flockSurplus, billShortfall, jobsBill, jobCall, groundJobs, helpText, argsUsage, docText, parseCliArgs, PRIMITIVES, SECTIONS, routineSteps, seedSource, compostPlan, farmSurplus, parsePlan, planCells, planErrors, planLane, planBill, planSummary, fieldCensus, farmJobs, checkArgs, handBackReason, compositeError, patchItemEnchants, leadTargetError, blindGates, enchantNames, itemsArg, enchantChoice, mineFailure, fencedIn, gateChange, fencePush, realCell, besideNames, noFooting, pitAdvice, chatText, wedgeReplant, thicketCost, stalkReplant, leadPick, herdPassed, gatesByReach, holesLeft, penShaftRefusal, fullSide, staleKey, bedExit, gateStepCost, eatJammed, eatFailure, eatRefusal, foodSort, eatRetryDue, afterTheMeal, errorRepeat, deathBy, deathReport, deathUnannounced, deathKit, outOfSight, herdOrder, ledReport, tagalongs, ledExtra, waterWary, patchPathfinder, stackTop, isBaby, noHomeError, progressed, crowdSize, dryCells, openNow, strays, shutNow, didYouMean, scanCap, eatBelow, withDefaultItem, foodAway, parseClock, gateLeak, smeltWait, giveReport, wedgeBreakable, wakeStep, bedtimeReport, deepestCell, unpenned, penCensus, droppedWalk, hurtCause, scanWhere, craftRoom, coordsError, nextDrop, digRefusal, fluidsLeft, scaffoldNote, scaffoldTakeBack, scaffoldBuilt, bedChoice, bedTrap, idleNudge, isGroundCover, looksBuilt, mineTargets, craftShortfall, placeObstacle, deadWalk, parseEventTail, fillOutcome, penLeak, transferFix, gatesLeftOpen, oversleeping, replantSpot, isStalkCut, staleCode, leadVerdict, clampedOffset, nudgeAway, breedingFood, flushCells, airReflex, openAbove, surfacingStalled, breaksUnderfoot, furnaceReport, trackReads, ignoredParams, depositWanted, peacefulTool, chaseVerdict, chaseBroken, chargeLeash, breakOffDigs, attackRefusal, fleeUnwinnable, huntPick, spotScore, bestSpots, brokenSlot, placeOutcome, equipSlot, shouldFlee, rangedThreat, minedCount, plansFromOwnCell, missingTool, stepOffChoice, wakeWorthy, waitReport, bedtime, feetCell, overMemory, placeAgainst, leftLying, arrivalError, ripeCrop, harvestOrder, dawnVerdict, withdrawPlan, isNight, occupiedBy, nextSheep, describeClock, buriedIn, doorwayNode, mayDig, explainNoPath, refuseReason, isStalled, explainInterrupt, ignorableMob, canPlaceFromHere,  terse, compact, describePlaces, describePlace, matchPlaces, capOutput, renderScan, inAnyZone, minecraftName, parseChosenName, nextPort, newAgentArgs, pickFuel, isWedged, retryUntilCount, matchesProps, checkWatch, within, clientVersions, blockTextures } from '../src/lib.mjs'

const terseCases = [
  ['long action with inventory changes',
    { task: 7, action: 'goto', seconds: 14, gained: { mutton: 1 }, lost: { cobblestone: 2 }, pos: { x: 61.4, y: 69, z: -106.5 }, ok: true },
    'ok goto 14s +mutton:1 -cobblestone:2 @61,69,-107'],
  ['empty gained/lost are dropped',
    { task: 1, action: 'sleep', seconds: 1, gained: {}, lost: {}, pos: { x: 1, y: 2, z: 3 }, ok: true },
    'ok sleep 1s @1,2,3'],
  ['extras are kept as key=json',
    { task: 1, action: 'attack', seconds: 5, gained: {}, lost: {}, pos: { x: 0, y: 0, z: 0 }, ok: true, killed: true },
    'ok attack 5s @0,0,0 killed'],
  ['failure shows the error',
    { task: 1, action: 'place', seconds: 3, gained: {}, lost: { granite: 9 }, pos: { x: 0, y: 0, z: 0 }, ok: false, error: 'oak_leaves already at (1, 2, 3)' },
    'FAIL place 3s -granite:9 @0,0,0 error: oak_leaves already at (1, 2, 3)'],
  ['still-running task',
    { ok: true, status: 'running', task: 4, note: 'will emit task_done event when finished' },
    'ok running task=4 (still going: block on ./mc wait for its task_done, do not end your turn)'],
  ['quick action uses the compact notation',
    { ok: true, name: 'air', pos: { x: 1.26, y: 2, z: 3 } },
    'ok name=air pos=1,2,3'],
  ['low vitals ride along on a long action',
    { task: 1, action: 'goto', seconds: 2, gained: {}, lost: {}, pos: { x: 0, y: 0, z: 0 }, ok: true, hp: 6, food: 3 },
    'ok goto 2s @0,0,0 hp=6 food=3'],
  ['quick action failure',
    { ok: false, error: 'unknown action foo' },
    'FAIL error: unknown action foo'],
  ['a map is printed raw',
    { ok: true, map: 'y=1\n..\n' },
    'y=1\n..\n']
]

for (const [name, input, expected] of terseCases) {
  test(`terse: ${name}`, () => assert.equal(terse(input), expected))
}

test('renderScan draws one grid per layer, top down, with a legend', () => {
  const world = { '0,1,0': 'stone', '1,1,0': 'oak_log', '0,0,0': 'stone', '1,0,1': 'sand' }
  const nameAt = (x, y, z) => world[`${x},${y},${z}`] ?? 'air'
  assert.equal(renderScan(nameAt, { x1: 0, y1: 0, z1: 0, x2: 1, y2: 1, z2: 1 }), [
    'x 0..1 across (ruler: last digit of x), z down',
    '  01',
    'y=1',
    '0 so',
    '1 ..',
    'y=0',
    '0 s.',
    '1 .a',
    's=stone o=oak_log a=sand'
  ].join('\n'))
})

test('renderScan keeps columns aligned when z labels differ in width, and rules negative x by its last digit', () => {
  const out = renderScan((x, y, z) => x === -11 && z === -10 ? 'stone' : 'air', { x1: -12, y1: 0, z1: -10, x2: -9, y2: 0, z2: -9 })
  assert.deepEqual(out.split('\n').slice(1, 5), ['    2109', 'y=0', '-10 .s..', ' -9 ....'])
})

test('renderScan gives clashing initials distinct symbols', () => {
  const names = ['stone', 'sand', 'sandstone']
  const out = renderScan((x) => names[x], { x1: 0, y1: 0, z1: 0, x2: 2, y2: 0, z2: 0 })
  assert.equal(out.split('\n').at(-1), 's=stone a=sand n=sandstone')
})

test('renderScan accepts corners in any order', () => {
  const a = renderScan(() => 'air', { x1: 2, y1: 1, z1: 1, x2: 0, y2: 0, z2: 0 })
  const b = renderScan(() => 'air', { x1: 0, y1: 0, z1: 0, x2: 2, y2: 1, z2: 1 })
  assert.equal(a, b)
})

const zones = [{ name: 'hut', x1: 118, y1: 72, z1: -139, x2: 114, y2: 69, z2: -143 }]
const zoneCases = [
  ['inside', { x: 116, y: 70, z: -141 }, true],
  ['on a corner', { x: 114, y: 69, z: -143 }, true],
  ['on the opposite corner', { x: 118, y: 72, z: -139 }, true],
  ['one block east', { x: 119, y: 70, z: -141 }, false],
  ['one block above', { x: 116, y: 73, z: -141 }, false],
  ['one block south', { x: 116, y: 70, z: -138 }, false]
]
for (const [name, pos, expected] of zoneCases) {
  test(`inAnyZone: ${name}`, () => assert.equal(inAnyZone(zones, pos), expected))
}
test('inAnyZone: no zones protects nothing', () => assert.equal(inAnyZone([], { x: 0, y: 0, z: 0 }), false))

test('renderScan collapses layers that are nothing but air', () => {
  const nameAt = (x, y, z) => y === 0 ? 'stone' : 'air'
  assert.equal(renderScan(nameAt, { x1: 0, y1: 0, z1: 0, x2: 1, y2: 2, z2: 0 }), [
    'x 0..1 across (ruler: last digit of x), z down',
    '  01',
    'y=2 all air',
    'y=1 all air',
    'y=0',
    '0 ss',
    's=stone'
  ].join('\n'))
})

const nameCases = [
  ['Lightsong', 'Lightsong'],
  ['Mahit Dzmare', 'MahitDzmare'],
  ["Mor'du", 'Mordu'],
  ['Éowyn', 'Eowyn'],
  ['Jean-Luc', 'JeanLuc'],
  ['GSV Sleeper Service', null], // 17 characters once squeezed: too long for Minecraft
  ['Al', null],
  ['名前', null]
]
for (const [raw, expected] of nameCases) {
  test(`minecraftName: ${raw}`, () => assert.equal(minecraftName(raw), expected))
}

test('parseChosenName reads name, source and note', () => {
  assert.deepEqual(parseChosenName('\u2728 Lightsong\n   Source: Warbreaker\n   god of bravery\n'),
    { name: 'Lightsong', source: 'Warbreaker', note: 'god of bravery' })
})
test('parseChosenName copes with a missing note', () => {
  assert.deepEqual(parseChosenName('\u2728 Nona\n   Source: Red Sister\n'), { name: 'Nona', source: 'Red Sister', note: '' })
})

const portCases = [[[], 3777], [[3777], 3778], [[3777, 3779], 3778], [[3778, 3777], 3779]]
for (const [used, expected] of portCases) {
  test(`nextPort after ${JSON.stringify(used)}`, () => assert.equal(nextPort(used), expected))
}

const harnesses = ['claude-code', 'codex']
const newAgentArgCases = [
  ['nothing: a drawn name on the default harness', [], { name: null, harness: 'claude-code' }],
  ['a name', ['Lightsong'], { name: 'Lightsong', harness: 'claude-code' }],
  ['a harness', ['--harness', 'codex'], { name: null, harness: 'codex' }],
  ['name then harness', ['Lightsong', '--harness', 'codex'], { name: 'Lightsong', harness: 'codex' }],
  ['harness then name', ['--harness', 'codex', 'Lightsong'], { name: 'Lightsong', harness: 'codex' }],
  ['--harness=name', ['--harness=codex'], { name: null, harness: 'codex' }],
  ['a harness with no notes file', ['--harness', 'gemini'], { error: 'unknown harness "gemini": the ones with notes in harness/ are claude-code, codex' }],
  ['--harness without a value', ['Lightsong', '--harness'], { error: '--harness needs a name: one of claude-code, codex' }],
  ['an option that is not --harness', ['--model', 'sonnet'], { error: 'unknown option --model (only --harness <name> is understood)' }],
  ['two names', ['Lightsong', 'Nona'], { error: 'one name only: got "Lightsong" and "Nona"' }]
]
for (const [what, argv, expected] of newAgentArgCases) {
  test(`newAgentArgs: ${what}`, () => assert.deepEqual(newAgentArgs(argv, harnesses), expected))
}
test('newAgentArgs: the default harness must have notes too', () => {
  assert.deepEqual(newAgentArgs([], ['codex']), { error: 'unknown harness "claude-code": the ones with notes in harness/ are codex' })
})

const compactCases = [
  ['numbers are rounded', 1.26, '1.3'],
  ['whole numbers stay whole', 19, '19'],
  ['a position is x,y,z of the block', { x: 61.4, y: 69, z: -106.5 }, '61,69,-107'],
  ['counts read name:count, and a count of 1 is just the name', { granite: 43, iron_pickaxe: 1, coal: 6 }, 'granite:43 iron_pickaxe coal:6'],
  ['key=value, true is a bare key, empty things vanish',
    { hp: 19, holding: null, raining: false, sleeping: true, task: '', armor: {}, seen: [] }, 'hp=19 sleeping'],
  ['nested objects are bracketed', { players: { mruwnik: { x: 15.4, y: 63, z: -78.8 } }, time: 'day 873' }, 'players(mruwnik=15,63,-79) time=day 873'],
  ['lists of scalars and positions are space separated', { positions: [{ x: 1, y: 2, z: 3 }, { x: 4, y: 5, z: 6 }], quick: ['state', 'look'] }, 'positions=1,2,3 4,5,6 quick=state look'],
  ['lists of objects are bracketed one by one', { results: [{ action: 'goto' }, { action: 'block_at', name: 'oak_door' }] }, 'results=(action=goto) (action=block_at name=oak_door)']
]
for (const [name, input, expected] of compactCases) {
  test(`compact: ${name}`, () => assert.equal(compact(input), expected))
}

test('capOutput leaves short output alone', () => assert.equal(capOutput('ok', 10), 'ok'))
test('capOutput cuts a flood and says how much was cut', () =>
  assert.equal(capOutput('x'.repeat(30), 10), 'xxxxxxxxxx\n[+20 chars cut: narrow the query, or delegate reading the full output (-v) to a subagent]'))

const places = [
  { name: 'dan-farm', kind: 'farm', x: 17, y: 63, z: -85, by: 'mruwnik', note: 'wheat' },
  { name: 'claude-hut', kind: 'base', x: 116, y: 69, z: -141, by: 'Claude', note: '' },
  { name: 'east-hill', kind: 'mine', x: 140, y: 75, z: -140, by: 'Claude', note: 'exposed stone' }
]
test('describePlaces lists nearest first, one line each', () => {
  assert.deepEqual(describePlaces(places, { x: 116, y: 69, z: -137 }), [
    'claude-hut base 4m @116,69,-141 (Claude)',
    'east-hill mine 25m @140,75,-140 (Claude: exposed stone)',
    'dan-farm farm 112m @17,63,-85 (mruwnik: wheat)'
  ])
})
test('describePlaces filters by kind and limits', () => {
  assert.deepEqual(describePlaces(places, { x: 0, y: 64, z: 0 }, { kind: 'mine' }), ['east-hill mine 198m @140,75,-140 (Claude: exposed stone)'])
  assert.equal(describePlaces(places, { x: 0, y: 64, z: 0 }, { limit: 2 }).length, 2)
  assert.deepEqual(describePlaces(places, { x: 116, y: 69, z: -137 }, { maxDist: 30, notes: false }), ['claude-hut base 4m @116,69,-141', 'east-hill mine 25m @140,75,-140'])
})

// 7c (Dan): places.json passed 60 entries, and a nearest-12 list is no way to find the one you want. Nobody should
// ever cat the file: the filters are the search, and a name asked for by name comes back whole.
const searchPlaces = [
  ...places,
  { name: 'trial-chest', kind: 'chest', x: 120, y: 69, z: -140, by: 'Chani', note: 'spare seed and a hoe' },
  { name: 'chani-wheat-field', kind: 'farm', x: 118, y: 68, z: -139, by: 'Chani', note: 'irrigated', plan: 'w~w\nwww' }
]
for (const [title, opts, expected] of [
  ['q matches a name', { q: 'hut' }, ['claude-hut']],
  ['q matches a note too, and ignores case', { q: 'HOE' }, ['trial-chest']],
  ['q is a substring, not a whole word', { q: 'wheat' }, ['chani-wheat-field', 'dan-farm']],
  ['q that matches nothing gives nothing', { q: 'diamond' }, []],
  ['by names the agent who marked it', { by: 'Chani' }, ['chani-wheat-field', 'trial-chest']],
  ['by ignores case as well', { by: 'chani' }, ['chani-wheat-field', 'trial-chest']],
  ['within cuts by distance in blocks', { within: 6 }, ['chani-wheat-field', 'claude-hut', 'trial-chest']],
  ['filters combine', { q: 'a', by: 'Chani', kind: 'farm' }, ['chani-wheat-field']],
  ['kind still works beside the rest', { kind: 'chest' }, ['trial-chest']]
]) {
  test(`describePlaces search: ${title}`, () =>
    assert.deepEqual(describePlaces(searchPlaces, { x: 116, y: 69, z: -137 }, opts).map(l => l.split(' ')[0]), expected))
}

test('matchPlaces counts every hit, so places can say how many it did not show', () => {
  assert.equal(matchPlaces(searchPlaces, { x: 116, y: 69, z: -137 }, { by: 'Claude' }).length, 2)
  assert.equal(describePlaces(searchPlaces, { x: 116, y: 69, z: -137 }, { by: 'Claude', limit: 1 }).length, 1)
})

for (const [title, name, expected] of [
  ['a plain place comes back whole', 'claude-hut',
    { name: 'claude-hut', kind: 'base', at: '116,69,-141', away: '4m', by: 'Claude' }],
  ['a note is kept', 'east-hill',
    { name: 'east-hill', kind: 'mine', at: '140,75,-140', away: '25m', by: 'Claude', note: 'exposed stone' }],
  ['a plan is reported by its size, not printed', 'chani-wheat-field',
    { name: 'chani-wheat-field', kind: 'farm', at: '118,68,-139', away: '3m', by: 'Chani', note: 'irrigated', plan: '3x2' }],
  ['a name nobody marked is null', 'no-such-place', null]
]) {
  test(`describePlace: ${title}`, () =>
    assert.deepEqual(describePlace(searchPlaces, name, { x: 116, y: 69, z: -137 }), expected))
}

const fuelCases = [
  ['coal covers 8 items each, rounded up', [{ name: 'coal', count: 22 }], 18, { name: 'coal', count: 3 }],
  ['prefers coal over planks', [{ name: 'oak_planks', count: 10 }, { name: 'charcoal', count: 1 }], 3, { name: 'charcoal', count: 1 }],
  ['planks burn 1.5 items each', [{ name: 'oak_planks', count: 10 }], 4, { name: 'oak_planks', count: 3 }],
  ['gives what it has when short', [{ name: 'coal', count: 1 }], 18, { name: 'coal', count: 1 }],
  ['nothing burnable', [{ name: 'cobblestone', count: 9 }], 2, null]
]
for (const [name, items, count, expected] of fuelCases) {
  test(`pickFuel: ${name}`, () => assert.deepEqual(pickFuel(items, count), expected))
}

const wedgedCases = [
  ['many resets and no movement', { resets: 40, moved: 0.1 }, true],
  ['many resets but moving (teleport, knockback)', { resets: 40, moved: 3 }, false],
  ['a few resets while standing still', { resets: 3, moved: 0 }, false],
  ['standing still, no resets', { resets: 0, moved: 0 }, false]
]
for (const [name, sample, expected] of wedgedCases) {
  test(`isWedged: ${name}`, () => assert.equal(isWedged(sample), expected))
}

// attempt(wanted) resolves to how many it got, or rejects; scripts below are per-round outcomes
const retryCases = [
  ['stops when the count is reached', [3, 'path', 5], 8, { got: 8, rounds: 3 }],
  ['gives up after two empty rounds in a row', [2, 'path', 0, 4], 10, { got: 2, rounds: 3, gaveUp: 'path' }],
  ['one empty round is forgiven', [0, 3], 3, { got: 3, rounds: 2 }],
  ['respects the round limit', [1, 1, 1, 1, 1, 1, 1, 1], 20, { got: 6, rounds: 6, gaveUp: 'round limit' }]
]
for (const [name, script, count, expected] of retryCases) {
  test(`retryUntilCount: ${name}`, async () => {
    const outcomes = [...script]
    const attempt = async () => { const o = outcomes.shift(); if (typeof o === 'string') throw new Error(o); return o }
    assert.deepEqual(await retryUntilCount(attempt, count, 6), expected)
  })
}

const propCases = [
  ['no condition matches anything', { age: 3 }, undefined, true],
  ['equal value', { age: 7 }, { age: 7 }, true],
  ['numbers and strings compare alike', { age: '7', half: 'lower' }, { age: 7 }, true],
  ['different value', { age: 3 }, { age: 7 }, false],
  ['missing property', {}, { age: 7 }, false]
]
for (const [name, props, where, expected] of propCases) {
  test(`matchesProps: ${name}`, () => assert.equal(matchesProps(props, where), expected))
}

// a watch fires on the edge: when its condition becomes true, not on every check while it stays true
const watchCases = [
  ['fires when the count is reached', { count: 5 }, 5, { fire: true, met: true }],
  ['not yet', { count: 5 }, 4, { fire: false, met: false }],
  ['does not fire again while still true', { count: 5, met: true }, 9, { fire: false, met: true }],
  ['re-arms after going false', { count: 5, met: true }, 0, { fire: false, met: false }],
  ['atMost fires when few are left', { count: 0, atMost: true }, 0, { fire: true, met: true }],
  ['atMost not met', { count: 2, atMost: true }, 3, { fire: false, met: false }],
  ['count defaults to 1', {}, 1, { fire: true, met: true }]
]
for (const [name, watch, seen, expected] of watchCases) {
  test(`checkWatch: ${name}`, () => assert.deepEqual(checkWatch(watch, seen), expected))
}

test('terse: state shows what the body is doing (a running task must be visible before starting another)', () => {
  assert.equal(terse({ ok: true, hp: 20, doing: 'clear 12s' }), 'ok hp=20 doing=clear 12s')
})

test('within: passes a fast result through', async () => assert.equal(await within(50, Promise.resolve(4), 'x'), 4))
test('within: rejects with what was being waited for when it takes too long', async () => {
  await assert.rejects(within(10, new Promise(() => {}), 'opening the crafting table'), /opening the crafting table took longer than 0.01s/)
})

const refusalCases = [
  ['mining at low health is refused', { name: 'mine.get', health: 6 }, /health is 6/],
  ['and so is the dig it is made of', { name: 'dig', health: 6 }, /health is 6/],
  ['force overrides low health', { name: 'dig', health: 6, force: true }, /^null$/],
  ['walking at low health is fine', { name: 'goto', health: 6 }, /^null$/],
  ['anything while asleep is refused', { name: 'goto', health: 20, sleeping: true }, /asleep/],
  ['force does not override sleep', { name: 'mine.get', health: 20, sleeping: true, force: true }, /asleep/],
  ['a run may start asleep (its steps are checked one by one, and the first may be wake)', { name: 'run', health: 20, sleeping: true }, /^null$/],
  ['healthy and awake', { name: 'mine.get', health: 20 }, /^null$/]
]
for (const [title, input, expected] of refusalCases) {
  test(`refuseReason: ${title}`, () => {
    assert.match(String(refuseReason(input)), expected)
  })
}

const reachCases = [
  ['block beside my feet', { x: 10.5, y: 64, z: 10.5 }, { x: 11, y: 64, z: 10 }, true],
  ['block three away', { x: 10.5, y: 64, z: 10.5 }, { x: 13, y: 65, z: 10 }, true],
  ['block six away', { x: 10.5, y: 64, z: 10.5 }, { x: 16, y: 64, z: 10 }, false],
  ['the block my feet are in', { x: 10.5, y: 64, z: 10.5 }, { x: 10, y: 64, z: 10 }, false],
  ['the block my head is in', { x: 10.5, y: 64, z: 10.5 }, { x: 10, y: 65, z: 10 }, false],
  ['the block under me', { x: 10.5, y: 64, z: 10.5 }, { x: 10, y: 63, z: 10 }, true],
  ['a neighbour my hitbox overlaps', { x: 10.9, y: 64, z: 10.5 }, { x: 11, y: 64, z: 10 }, false]
]
for (const [title, feet, block, expected] of reachCases) {
  test(`canPlaceFromHere: ${title}`, () => assert.equal(canPlaceFromHere(feet, block), expected))
}

const ignorableCases = [
  ['endermen are never worth fleeing', 'enderman', { day: false, skyLight: 0 }, true],
  ['a spider in daylight is neutral', 'spider', { day: true, skyLight: 15 }, true],
  ['a spider at night is not', 'spider', { day: false, skyLight: 15 }, false],
  ['a spider in a cave by day is not', 'spider', { day: true, skyLight: 3 }, false],
  ['cave spiders never are', 'cave_spider', { day: true, skyLight: 15 }, false],
  ['zombies never are', 'zombie', { day: true, skyLight: 15 }, false]
]
for (const [title, name, where, expected] of ignorableCases) {
  test(`ignorableMob: ${title}`, () => assert.equal(ignorableMob(name, where), expected))
}

const interruptCases = [
  ['a flee explains a changed goal', 'The goal was changed before it could be completed!', { kind: 'fleeing', mob: 'zombie', agoMs: 1500 }, /interrupted: fleeing from zombie.*retry/],
  ['a fight explains a stopped path', 'Path was stopped before it could be completed! Thus, the desired goal was not reached.', { kind: 'fighting', mob: 'spider', agoMs: 200 }, /interrupted: fighting spider/],
  ['an old reflex explains nothing', 'The goal was changed before it could be completed!', { kind: 'fleeing', mob: 'zombie', agoMs: 60000 }, /^The goal was changed/],
  ['no reflex, error kept', 'The goal was changed before it could be completed!', null, /^The goal was changed/],
  ['everyone flees creepers, so no sword advice', 'The goal was changed before it could be completed!', { kind: 'fleeing', mob: 'creeper', agoMs: 500 }, /^interrupted: fleeing from creeper\. Wait until it is over, then retry$/],
  ['other errors are kept', 'no place called x', { kind: 'fleeing', mob: 'zombie', agoMs: 100 }, /^no place called x$/],
  // the leash walks the body back to where the fight began, which changes the goal under whatever task was running
  ['a capped chase says it is walking back', 'The goal was changed before it could be completed!', { kind: 'leashed', mob: 'spider', agoMs: 300 }, /^interrupted: breaking off a fight with spider that pulled me too far: walking back to where it started\. Wait until it is over, then retry$/]
]
for (const [title, error, reflex, expected] of interruptCases) {
  test(`explainInterrupt: ${title}`, () => assert.match(explainInterrupt(error, reflex), expected))
}

const stallCases = [
  ['walking goal, no movement, not digging, long enough', { hasGoal: true, moved: 0.05, digging: false, seconds: 12 }, true],
  ['still moving', { hasGoal: true, moved: 2, digging: false, seconds: 12 }, false],
  ['digging its way through', { hasGoal: true, moved: 0, digging: true, seconds: 30 }, false],
  ['standing still without a goal (smelting, sleeping)', { hasGoal: false, moved: 0, digging: false, seconds: 60 }, false],
  ['not long enough yet', { hasGoal: true, moved: 0, digging: false, seconds: 6 }, false]
]
for (const [title, sample, expected] of stallCases) {
  test(`isStalled: ${title}`, () => assert.equal(isStalled(sample), expected))
}

const mayDigCases = [
  ['a plain walk never digs', 'goto', { x: 1, z: 2 }, false],
  ['a walk may dig when asked', 'goto', { x: 1, z: 2, dig: true }, true],
  ['only a real true counts', 'goto', { dig: 'no' }, false],
  ['a dig may tunnel its way to the block when asked', 'dig', { x: 1, y: 2, z: 3, dig: true }, true],
  ['a dig on its own does not', 'dig', { x: 1, y: 2, z: 3 }, false],
  ['fetching from a chest does not', 'withdraw', {}, false]
]
for (const [title, name, args, expected] of mayDigCases) {
  test(`mayDig: ${title}`, () => assert.equal(mayDig(name, args), expected))
}

const noPathCases = [
  ['walk-only no path gets the dig hint', 'No path to the goal!', false, /no walkable path.*dig=true/],
  ['walk-only timeout gets it too', 'Took to long to decide path to goal!', false, /no walkable path.*dig=true/],
  ['a digging walk keeps the plain error', 'No path to the goal!', true, /^No path to the goal!$/],
  ['other errors are kept', 'no place called x', false, /^no place called x$/]
]
for (const [title, error, dig, expected] of noPathCases) {
  test(`explainNoPath: ${title}`, () => assert.match(explainNoPath(error, dig), expected))
}

test('arrivalError: a goal that was met is no error', () => assert.equal(arrivalError(true), null))

const arrivalCases = [
  ['a walk that ended short reads as no path, with the walk advice', false, /no walkable path/],
  ['a dig walk that ended short is a plain no path', true, /^no path to the goal/]
]
for (const [title, dig, expected] of arrivalCases) {
  test(`arrivalError: ${title}`, () => assert.match(explainNoPath(arrivalError(false), dig), expected))
}

const doorwayCases = [
  ['a waypoint lifted onto the upper door half drops to the floor, centred', { x: 117.296875, y: 70, z: -137.5 }, { x: 117, y: 70, z: -138, half: 'upper' }, { x: 117.5, y: 69, z: -137.5 }],
  ['a waypoint in the lower half is centred', { x: 117.3, y: 69, z: -137.5 }, { x: 117, y: 69, z: -138, half: 'lower' }, { x: 117.5, y: 69, z: -137.5 }],
  ['no door, no change', { x: 4.5, y: 64, z: 9.5 }, null, { x: 4.5, y: 64, z: 9.5 }]
]
for (const [title, node, door, expected] of doorwayCases) {
  test(`doorwayNode: ${title}`, () => assert.deepEqual(doorwayNode(node, door), expected))
}

const solid = name => ({ name, boundingBox: 'block' })
const open = name => ({ name, boundingBox: 'empty' })
const buriedCases = [
  ['gravel on the head is dug first', [solid('gravel'), solid('gravel')], 'gravel'],
  ['head free, nothing to do (standing in tall grass)', [open('air'), open('short_grass')], undefined],
  ['sand round the head', [solid('sand'), open('air')], 'sand'],
  ['unloaded chunk', [null, null], undefined],
  ['walking through a doorway is not being buried', [solid('oak_door'), solid('oak_door')], undefined],
  ['concrete powder falls too', [solid('red_concrete_powder'), open('air')], 'red_concrete_powder']
]
for (const [title, headThenFeet, expected] of buriedCases) {
  test(`buriedIn: ${title}`, () => assert.equal(buriedIn(headThenFeet)?.name, expected))
}

const clockCases = [
  ['fresh day', { day: true, timeOfDay: 2300, by: 'Claude', at: 100000 }, 108000, /^time=day 2300 \(seen 8s ago by Claude\)$/],
  ['fresh night', { day: false, timeOfDay: 15000, by: 'Aviendha', at: 100000 }, 101000, /^time=night 15000 \(seen 1s ago by Aviendha\)$/],
  ['stale: nobody online to tell', { day: false, timeOfDay: 15000, by: 'Claude', at: 100000 }, 100000 + 120000, /^time unknown: .*120s.*start your body/],
  ['never written', null, 5, /^time unknown: .*start your body/]
]
for (const [title, clock, now, expected] of clockCases) {
  test(`describeClock: ${title}`, () => assert.match(describeClock(clock, now), expected))
}

const sheepCases = [
  ['the nearest sheep first', [{ id: 1, dist: 9 }, { id: 2, dist: 3 }], [], 40, 2],
  ['not one I have just shorn', [{ id: 1, dist: 9 }, { id: 2, dist: 3 }], [2], 40, 1],
  ['not one too far away', [{ id: 1, dist: 50 }], [], 40, undefined],
  ['none left', [{ id: 2, dist: 3 }], [2], 40, undefined]
]
for (const [title, sheep, shorn, within, expected] of sheepCases) {
  test(`nextSheep: ${title}`, () => assert.equal(nextSheep(sheep, new Set(shorn), within)?.id, expected))
}

const occupiedCases = [
  ['empty spot', null, 'oak_planks', 'free'],
  ['air-like plants do not block', { name: 'short_grass', boundingBox: 'empty' }, 'oak_planks', 'free'],
  ['the same block is already there (re-running a build repairs it)', { name: 'oak_planks', boundingBox: 'block' }, 'oak_planks', 'skip'],
  ['leaves are cleared out of the way', { name: 'birch_leaves', boundingBox: 'block' }, 'oak_planks', 'clear'],
  ['anything else is somebody\'s block: stop', { name: 'cobblestone', boundingBox: 'block' }, 'oak_planks', 'blocked']
]
for (const [title, existing, wanted, expected] of occupiedCases) {
  test(`occupiedBy: ${title}`, () => assert.equal(occupiedBy(existing, wanted), expected))
}

const nightCases = [[0, false], [12542, false], [12543, true], [12713, true], [18000, true], [23459, true], [23460, false], [23999, false]]
for (const [tick, expected] of nightCases) {
  test(`isNight: tick ${tick} (night = when beds work)`, () => assert.equal(isNight(tick), expected))
}

const nb = (name, boundingBox = 'block') => ({ name, boundingBox })
const againstCases = [
  ['first solid neighbour wins', [nb('air', 'empty'), nb('stone'), nb('dirt')], { index: 1, sneak: false }],
  ['a bed below is skipped for a plain wall: clicking a bed uses it', [nb('white_bed'), nb('air', 'empty'), nb('cherry_planks')], { index: 2, sneak: false }],
  ['only a chest to place against: sneak', [nb('air', 'empty'), nb('chest')], { index: 1, sneak: true }],
  ['doors, tables and furnaces count as clickable too', [nb('oak_door'), nb('crafting_table'), nb('furnace'), nb('cobblestone')], { index: 3, sneak: false }],
  ['nothing solid around', [nb('air', 'empty'), nb('water', 'empty')], null],
  ['unloaded neighbours are ignored', [null, nb('stone')], { index: 1, sneak: false }]
]
for (const [title, neighbours, expected] of againstCases) {
  test(`placeAgainst: ${title}`, () => assert.deepEqual(placeAgainst(neighbours), expected))
}

const cell = (feet, head, ground) => ({ feet, head, ground })
const pickaxes = { 701: 'stone_pickaxe', 706: 'iron_pickaxe' }
const toolCases = [
  ['dirt needs nothing', undefined, [], null],
  ['iron ore with a stone pickaxe', { 701: true, 706: true }, [12, 701], null],
  ['iron ore with bare hands or a wooden pickaxe: say what is needed', { 701: true, 706: true }, [12, 696], 'stone_pickaxe'],
  ['empty inventory', { 701: true }, [], 'stone_pickaxe']
]
for (const [title, harvestTools, carried, expected] of toolCases) {
  test(`missingTool: ${title}`, () => assert.equal(missingTool(harvestTools, carried, id => pickaxes[id]), expected))
}

const ownCellCases = [['white_bed', true], ['red_bed', true], ['bedrock', false], ['farmland', false], ['stone_slab', false]]
for (const [name, expected] of ownCellCases) {
  test(`plansFromOwnCell: ${name}`, () => assert.equal(plansFromOwnCell(name), expected))
}

const minedCases = [
  ['one block gone, one item gained', 1, 1, 1],
  ['dirt dug on the way is not progress', 0, 4, 0],
  ['one stone gone, plus dirt picked up on the way', 1, 5, 1],
  ['blocks gone but nothing picked up (full inventory)', 3, 0, 0],
  ['something was tossed meanwhile', 2, -1, 0],
  ['drops still falling when we count (tree tops): the blocks that are gone count', 36, 25, 36]
]
for (const [title, gone, gained, expected] of minedCases) {
  test(`minedCount: ${title}`, () => assert.equal(minedCount(gone, gained), expected))
}

const shot = { hurtMsAgo: 1000, fighting: false, armed: true, health: 16, archerNear: true, meleeNear: false }
const rangedCases = [
  ['shot by a skeleton out of melee reach, armed and healthy: charge it', {}, 'charge'],
  ['unarmed: run', { armed: false }, 'flee'],
  ['badly hurt: run', { health: 8 }, 'flee'],
  // Ganesha charged at 13 hp, through a lake, and came out with 2: a charge costs 2 or 3 arrows even when it goes well
  ['13 hp is too little to pay for a charge', { health: 13 }, 'flee'],
  ['14 hp will do', { health: 14 }, 'charge'],
  ['in water we are slow and an easy target: run', { inWater: true }, 'flee'],
  ['already in a fight: leave it be', { fighting: true }, null],
  ['a spider is on us: that is what hurt, deal with it before running off after a skeleton', { meleeNear: true }, null],
  ['not hurt lately: an archer far off is not our business', { hurtMsAgo: 9000 }, null],
  ['hurt, but no archer about (fall, hunger)', { archerNear: false }, null]
]
for (const [title, change, expected] of rangedCases) {
  test(`rangedThreat: ${title}`, () => assert.equal(rangedThreat({ ...shot, ...change }), expected))
}

const slotCases = [
  ['iron_helmet', 'head'], ['turtle_helmet', 'head'], ['carved_pumpkin', 'head'], ['leather_chestplate', 'torso'], ['elytra', 'torso'],
  ['iron_leggings', 'legs'], ['golden_boots', 'feet'], ['shield', 'off-hand'], ['stone_sword', 'hand'], ['bread', 'hand']
]
for (const [item, expected] of slotCases) {
  test(`equipSlot: ${item}`, () => assert.equal(equipSlot(item), expected))
}

const brawl = { armed: true, health: 20, attackers: 1, armorPieces: 4 }
const fleeCases = [
  ['armed, healthy, one attacker: fight', {}, false],
  ['unarmed: run', { armed: false }, true],
  ['badly hurt: run', { health: 8 }, true],
  ['no armour, two zombies: run while there is health left to run with', { armorPieces: 0, attackers: 2, health: 15 }, true],
  ['no armour, two zombies, still fresh: fight', { armorPieces: 0, attackers: 2, health: 18 }, false],
  ['no armour, one zombie at 12: run', { armorPieces: 0, health: 12 }, true],
  ['full armour, two zombies at 12: fight on', { attackers: 2, health: 12 }, false],
  ['a crowd never pushes the threshold past 16', { armorPieces: 0, attackers: 6, health: 17 }, false]
]
for (const [title, change, expected] of fleeCases) {
  test(`shouldFlee: ${title}`, () => assert.equal(shouldFlee({ ...brawl, ...change }), expected))
}

const miss = (x, why) => ({ at: `${x},65,-135`, why })
const outcomeCases = [
  ['everything placed', 9, [], { placed: 9 }],
  ['some cells out of reach: say how many, and where the first is', 40, [miss(112, 'cannot get within reach'), miss(113, 'cannot get within reach')],
    { placed: 40, skipped: 2, why: '2 cannot get within reach (first 112,65,-135)' }],
  ['every different reason is told, not only the first', 3, [miss(112, 'grass_block is already there'), miss(113, 'nothing to place against'), miss(114, 'nothing to place against')],
    { placed: 3, skipped: 3, why: '1 grass_block is already there (first 112,65,-135); 2 nothing to place against (first 113,65,-135)' }],
  ['nothing placed at all is a failure', 0, [miss(112, 'nothing to place against')], { error: 'placed nothing: 1 nothing to place against (first 112,65,-135)' }]
]
for (const [title, placed, skipped, expected] of outcomeCases) {
  test(`placeOutcome: ${title}`, () => assert.deepEqual(placeOutcome(placed, skipped), expected))
}

const brokenCases = [[47, 'hand'], [48, 'off-hand'], [49, 'head'], [50, 'torso'], [51, 'legs'], [52, 'feet'], [2, null], [35, null]]
for (const [status, expected] of brokenCases) {
  test(`brokenSlot: entity status ${status}`, () => assert.equal(brokenSlot(status), expected))
}
test('wakeWorthy: a tool that broke is worth waking for', () => assert.equal(wakeWorthy({ type: 'tool_broke', item: 'stone_axe' }, 'Jizo'), true))

const stepOffCases = [
  ['first side with room for feet and head and a floor', [cell('block', 'empty', 'block'), cell('empty', 'empty', 'block')], 1],
  ['no floor there (a drop): not that way', [cell('empty', 'empty', 'empty'), cell('empty', 'empty', 'block')], 1],
  ['head would hit something', [cell('empty', 'block', 'block')], -1],
  ['walled in on every side', [cell('block', 'block', 'block'), cell('block', 'empty', 'block')], -1],
  ['unloaded cells count as closed', [cell(undefined, undefined, undefined)], -1],
  ['a bed in a 1-wide room (Aviendha): no free floor beside me, but the other half of the bed leads out', [cell('block', 'block', 'block'), { ...cell('block', 'empty', 'block'), low: true }], 1],
  ['free floor still wins over the bed', [{ ...cell('block', 'empty', 'block'), low: true }, cell('empty', 'empty', 'block')], 1],
  ['a full block beside me is no way out', [{ ...cell('block', 'empty', 'block'), low: false }], -1],
  ['my own door beside the bed is the way out, before the bed\'s other half (Aviendha, every morning)', [{ ...cell('block', 'empty', 'block'), low: true }, { ...cell('block', 'block', 'block'), door: true }], 1],
  ['free floor still wins over a door', [{ ...cell('block', 'block', 'block'), door: true }, cell('empty', 'empty', 'block')], 1]
]
for (const [title, cells, expected] of stepOffCases) {
  test(`stepOffChoice: ${title}`, () => assert.equal(stepOffChoice(cells), expected))
}

const wakeCases = [
  ['a finished task', { type: 'task_done', ok: true }, true],
  ['someone else talking', { type: 'chat', from: 'mruwnik', message: 'hi' }, true],
  ['my own chat echo', { type: 'chat', from: 'Jizo', message: 'hi' }, false],
  ['morning', { type: 'dawn' }, true],
  ['the body died', { type: 'body_down', exit: 134 }, true],
  ['a watch fired', { type: 'watch_hit', name: 'wheat-ripe' }, true],
  ['a scratch', { type: 'hurt', health: 17 }, false],
  ['badly hurt', { type: 'hurt', health: 7 }, true],
  ['players coming and going', { type: 'player_joined', player: 'Miles' }, false],
  ['the body going to bed by itself', { type: 'bedtime' }, false],
  ['it could not go to bed', { type: 'bedtime_failed', error: 'no bed' }, true]
]
for (const [title, event, expected] of wakeCases) {
  test(`wakeWorthy: ${title}`, () => assert.equal(wakeWorthy(event, 'Jizo'), expected))
}

const reportCases = [
  ['events worth waking for, one line each, without seq and time',
    '{"seq":4,"t":"2026-09-19T04:00:00Z","type":"player_joined","player":"Miles"}\n{"seq":5,"t":"2026-09-19T04:00:01Z","type":"task_done","action":"mine","ok":true}\n',
    ['task_done action=mine ok']],
  ['half-written last line is left for next time', '{"seq":5,"t":"x","type":"dawn"}\n{"seq":6,"t":"x","ty', ['dawn']],
  ['nothing of note', '{"seq":4,"t":"x","type":"player_left","player":"Miles"}\n', []],
  ['old news says how old it is: a death from before the last commands was read as a second death',
    '{"seq":7,"t":"2026-09-19T03:57:00Z","type":"died"}\n{"seq":8,"t":"2026-09-19T03:59:50Z","type":"dawn"}\n', ['(3m ago) died', 'dawn']]
]
for (const [title, text, lines] of reportCases) {
  test(`waitReport: ${title}`, () => assert.deepEqual(waitReport(text, 'Jizo', Date.parse('2026-09-19T04:00:05Z')).lines, lines))
}
test('waitReport: consumed counts only whole lines', () => assert.equal(waitReport('{"type":"dawn"}\n{"ty', 'Jizo').consumed, 16))

const tired = { night: true, busy: false, asleep: false, bedNear: true, hostileNear: false, reflexes: true, idleMs: 120000, sinceTryMs: 60000, failures: 0 }
const bedtimeCases = [
  ['night, idle, a bed nearby: go to bed', {}, true],
  ['day', { night: false }, false],
  ['a task is running: the driver decides', { busy: true }, false],
  ['already asleep', { asleep: true }, false],
  ['no bed nearby', { bedNear: false }, false],
  ['a monster is close: beds refuse, fight first', { hostileNear: true }, false],
  ['reflexes switched off', { reflexes: false }, false],
  ['the driver was busy a moment ago: leave it time to act', { idleMs: 20000 }, false],
  ['tried a moment ago: do not hammer', { sinceTryMs: 5000 }, false],
  ['failed three times tonight (bed taken): wait 4 minutes before the next try', { failures: 3, sinceTryMs: 200000 }, false],
  ['... and then try again', { failures: 3, sinceTryMs: 250000 }, true]
]
for (const [title, change, expected] of bedtimeCases) {
  test(`bedtime: ${title}`, () => assert.equal(bedtime({ ...tired, ...change }), expected))
}

const feetCases = [
  ['on a full block: the floored cell', { x: 6.5, y: 63, z: -85.5 }, true, { x: 6, y: 63, z: -86 }],
  ['on farmland (15/16 high): the cell above it, as the pathfinder counts', { x: 6.5, y: 62.9375, z: -85.5 }, true, { x: 6, y: 63, z: -86 }],
  ['on a bottom slab', { x: 10.2, y: 63.5, z: -84.5 }, true, { x: 10, y: 64, z: -85 }],
  ['in mid-air (jumping, falling): just floored', { x: 6.5, y: 63.4, z: -85.5 }, false, { x: 6, y: 63, z: -86 }]
]
for (const [title, position, onGround, expected] of feetCases) {
  test(`feetCell: ${title}`, () => assert.deepEqual(feetCell(position, onGround), expected))
}

const memoryCases = [[200, false], [799, false], [800, true], [1500, true]]
for (const [heapMb, expected] of memoryCases) {
  test(`overMemory: ${heapMb} MB`, () => assert.equal(overMemory(heapMb), expected))
}

const fullCases = [
  ['free slots: nothing to say', 3, [], {}],
  ['full, and drops were left lying: say why', 0, ['crafting_table'], { inventoryFull: 'left lying: crafting_table. Deposit or toss something first' }],
  ['full but nothing left behind (it stacked)', 0, [], {}]
]
for (const [title, freeSlots, left, expected] of fullCases) {
  test(`leftLying: ${title}`, () => assert.deepEqual(leftLying(freeSlots, left), expected))
}

const withdrawCases = [
  ['everything is there', [{ name: 'bread', count: 3 }], { bread: 7 }, { take: [{ name: 'bread', count: 3 }], short: [] }],
  ['no count means all of it', [{ name: 'coal' }], { coal: 8 }, { take: [{ name: 'coal', count: 8 }], short: [] }],
  ['less than asked: take what there is and say so', [{ name: 'bread', count: 9 }], { bread: 7 }, { take: [{ name: 'bread', count: 7 }], short: ['bread:7/9'] }],
  ['the short form {name: count} works too', { bread: 3, coal: 2 }, { bread: 7, coal: 8 }, { take: [{ name: 'bread', count: 3 }, { name: 'coal', count: 2 }], short: [] }],
  ['not there at all: say so, take the rest', [{ name: 'white_bed', count: 1 }, { name: 'coal', count: 2 }], { coal: 8 }, { take: [{ name: 'coal', count: 2 }], short: ['white_bed:0/1'] }]
]
for (const [title, wanted, inChest, expected] of withdrawCases) {
  test(`withdrawPlan: ${title}`, () => assert.deepEqual(withdrawPlan(wanted, inChest), expected))
}

const dawnCases = [
  ['night, fresh clock: keep waiting', { day: false, timeOfDay: 15000, by: 'Claude', at: 100000 }, 103000, 0, 'wait'],
  ['day: wake up', { day: true, timeOfDay: 30, by: 'Claude', at: 100000 }, 103000, 0, 'day'],
  ['nobody has reported for 90 s: the world is empty and its time stands still, so waiting would never end', { day: false, timeOfDay: 15000, by: 'Claude', at: 100000 }, 100000 + 91000, 0, 'stale'],
  ['no clock at all', null, 5, 0, 'stale'],
  ['waited 8 minutes: report in rather than be killed silently', { day: false, timeOfDay: 22000, by: 'Claude', at: 100000 }, 101000, 8 * 60, 'long']
]
for (const [title, clock, now, waitedSeconds, expected] of dawnCases) {
  test(`dawnVerdict: ${title}`, () => assert.equal(dawnVerdict(clock, now, waitedSeconds), expected))
}

const ripeCases = [
  ['ripe wheat', 'wheat', 7, 'wheat_seeds'],
  ['young wheat', 'wheat', 6, null],
  ['ripe carrots replant with a carrot', 'carrots', 7, 'carrot'],
  ['ripe potatoes', 'potatoes', 7, 'potato'],
  ['beetroots ripen at 3', 'beetroots', 3, 'beetroot_seeds'],
  ['young beetroots', 'beetroots', 2, null],
  ['not a crop', 'short_grass', 7, null]
]
for (const [title, name, age, expected] of ripeCases) {
  test(`ripeCrop: ${title}`, () => assert.equal(ripeCrop(name, age), expected))
}

test('harvestOrder: walks the field row by row, there and back, instead of zig-zagging by distance', () => {
  const field = [{ x: 2, z: 1 }, { x: 0, z: 0 }, { x: 1, z: 1 }, { x: 1, z: 0 }, { x: 0, z: 1 }, { x: 2, z: 0 }]
  assert.deepEqual(harvestOrder(field).map(p => `${p.x},${p.z}`), ['0,0', '1,0', '2,0', '2,1', '1,1', '0,1'])
})

// attack mob= followed its target anywhere (40 blocks down a cave, then stalled): it now gives up past its leash
for (const [name, chase, expected] of [
  ['target dead', { targetValid: false, hunting: false, strayed: 3, leash: 24 }, { killed: true }],
  ['target dead far away still counts', { targetValid: false, hunting: false, strayed: 30, leash: 24 }, { killed: true }],
  // my attack on a creeper (22:03Z) answered a bare `ok`: terse drops killed=false, and ok reads as a kill
  ['fight stopped by a reflex', { targetValid: true, hunting: false, strayed: 3, leash: 24 }, { killed: false, gaveUp: 'the fight was broken off (the body fled, ate or was interrupted): it is still alive. Look around, then attack again or keep away' }],
  ['still fighting nearby', { targetValid: true, hunting: true, strayed: 23, leash: 24 }, null],
  ['chased too far', { targetValid: true, hunting: true, strayed: 25, leash: 24 }, { killed: false, gaveUp: 'it led me 25 blocks away (leash=24): let it go, or attack again from here' }]
]) test(`chaseVerdict: ${name}`, () => assert.deepEqual(chaseVerdict(chase), expected))

// #105: the fight REFLEX has no leash of its own. pvp walks the body after the mob and the mob stays beside the body,
// so a distance measured mob-to-body never grows: a spider walked Claude into a cave and the body died there. The
// reflex's leash is measured from where the fight started, and a drop counts before a walk does.
for (const [name, start, here, expected] of [
  ['standing where it began', { x: 10, y: 70, z: -5 }, { x: 10, y: 70, z: -5 }, null],
  ['a few steps after it is still the same fight', { x: 10, y: 70, z: -5 }, { x: 16, y: 70, z: -5 }, null],
  ['exactly at the leash is still allowed', { x: 10, y: 70, z: -5 }, { x: 18, y: 70, z: -5 }, null],
  ['walked off the leash', { x: 10, y: 70, z: -5 }, { x: 20, y: 70, z: -5 },
    'the fight pulled me 10 blocks from where it started (leash=8): broken off, and I am walking back'],
  ['height is not distance: a tower is not a chase', { x: 10, y: 70, z: -5 }, { x: 14, y: 82, z: -5 }, null],
  ['three blocks down is still the surface', { x: 10, y: 70, z: -5 }, { x: 11, y: 67, z: -5 }, null],
  ['pulled down a hole', { x: 10, y: 70, z: -5 }, { x: 11, y: 66, z: -5 },
    'the fight pulled me 4 blocks down (from y=70): broken off before it becomes a cave, and I am walking back'],
  ['down AND away: the drop is what killed me, so it is what is said', { x: 10, y: 70, z: -5 }, { x: 30, y: 50, z: -5 },
    'the fight pulled me 20 blocks down (from y=70): broken off before it becomes a cave, and I am walking back'],
  ['no fight, no leash', null, { x: 30, y: 50, z: -5 }, null]
]) test(`chaseBroken: ${name}`, () => assert.equal(chaseBroken(start, here), expected))

// the leash nearly broke the reflex it shares a body with: rangedThreat 'charge' answers a skeleton shooting from 20
// blocks by running at it, and fightStart is where the body stood when it was shot. A flat leash of 8 aborts that
// charge a third of the way there and walks the body back into the arrows. A fight that begins by crossing ground
// gets that ground added to its leash, so the charge can land and anything past the target still breaks off.
for (const [name, start, mob, expected] of [
  ['a melee mob within reach changes nothing much', { x: 0, y: 70, z: 0 }, { x: 2, y: 70, z: 0 }, 10],
  ['a mob underfoot is the plain leash', { x: 0, y: 70, z: 0 }, { x: 0, y: 70, z: 0 }, 8],
  ['a skeleton 20 blocks off buys the charge its 20 blocks', { x: 0, y: 70, z: 0 }, { x: 20, y: 70, z: 0 }, 28],
  ['height is not ground to cross', { x: 0, y: 70, z: 0 }, { x: 0, y: 90, z: 0 }, 8],
  ['diagonal counts once', { x: 0, y: 70, z: 0 }, { x: 3, y: 70, z: 4 }, 13]
]) test(`chargeLeash: ${name}`, () => assert.equal(chargeLeash(start, mob), expected))

test('chargeLeash: a charge that lands still breaks off once it is dragged a leash past the target', () => {
  const start = { x: 0, y: 70, z: 0 }
  const mob = { x: 20, y: 70, z: 0 }
  const leash = chargeLeash(start, mob)
  assert.equal(chaseBroken(start, { x: 20, y: 70, z: 0 }, { leash }), null, 'reaching the skeleton is not a chase')
  assert.match(chaseBroken(start, { x: 30, y: 70, z: 0 }, { leash }), /30 blocks from where it started \(leash=28\)/)
})

// verified the hard way (2026-09-23 02:26Z): the leash fired correctly 4 blocks down a shaft, the body stopped
// swinging, and then it could not climb the 4 blocks back because the walk-back goal walks and does not dig. It stood
// in the hole and a zombie beat it from 18 to 0 in nine seconds. A break-off that was a DROP has to be allowed to dig
// and bridge its way back up: the body dug its way down, and the same ground is in the way going up. A flat break-off
// walked there on its feet, so it can walk back on them, and is not given a licence to tunnel.
for (const [name, start, here, expected] of [
  ['pulled down a hole: dig back out', { x: 0, y: 62, z: 0 }, { x: 0, y: 58, z: 0 }, true],
  ['one block down still counts as up to climb', { x: 0, y: 62, z: 0 }, { x: 3, y: 61, z: 0 }, true],
  ['walked off on the flat: walk back', { x: 0, y: 62, z: 0 }, { x: 10, y: 62, z: 0 }, false],
  ['chased uphill: the way back is down', { x: 0, y: 62, z: 0 }, { x: 10, y: 70, z: 0 }, false]
]) test(`breakOffDigs: ${name}`, () => assert.equal(breakOffDigs(start, here), expected))

test('chaseBroken: the leash and the drop can be tightened or loosened together', () => {
  assert.equal(chaseBroken({ x: 0, y: 70, z: 0 }, { x: 10, y: 70, z: 0 }, { leash: 24 }), null)
  assert.match(chaseBroken({ x: 0, y: 70, z: 0 }, { x: 0, y: 69, z: 0 }, { drop: 0 }), /^the fight pulled me 1 blocks down/)
})

// `node --check` is the gate before every body restart and it only parses: a name imported from lib.mjs but never
// exported there passes the check and kills the body on its first line at startup (chaseLeash/CHASE_LEASH did, and
// the body was down until the log was read). Every named import of lib.mjs has to name something lib.mjs exports.
test('every module imports only names lib.mjs actually exports', async () => {
  const roots = ['src', 'library']
  const walk = dir => fs.readdirSync(dir, { withFileTypes: true }).flatMap(e =>
    e.isDirectory() ? walk(path.join(dir, e.name)) : e.name.endsWith('.mjs') ? [path.join(dir, e.name)] : [])
  const exported = new Set(Object.keys(await import('../src/lib.mjs')))
  const missing = roots.flatMap(walk).flatMap(file => {
    const src = fs.readFileSync(file, 'utf8')
    const imports = [...src.matchAll(/import\s*\{([^}]*)\}\s*from\s*'([^']*lib\.mjs)'/g)]
    return imports.flatMap(([, names]) => names.split(',')
      .map(n => n.trim().split(/\s+as\s+/)[0].trim())
      .filter(n => n && !exported.has(n))
      .map(n => `${file}: ${n}`))
  })
  assert.deepEqual(missing, [])
})

// the other half of the same crash: a lib helper USED but never imported. `node --check` parses it happily and the
// body dies with `ReferenceError: CHASE_LEASH is not defined` on its first line, which is only visible in bot.log.
test('every module that uses a lib.mjs helper imports it', async () => {
  const lib = await import('../src/lib.mjs')
  const exported = Object.keys(lib)
  const files = ['src', 'library'].flatMap(function walk (dir) {
    return fs.readdirSync(dir, { withFileTypes: true }).flatMap(e =>
      e.isDirectory() ? walk(path.join(dir, e.name)) : e.name.endsWith('.mjs') ? [path.join(dir, e.name)] : [])
  })
  const unimported = files.filter(f => !f.endsWith('lib.mjs')).flatMap(file => {
    const src = fs.readFileSync(file, 'utf8')
    const importBlocks = [...src.matchAll(/import\s*\{([^}]*)\}\s*from\s*'[^']*'/g)]
    const imported = new Set(importBlocks.flatMap(([, names]) => names.split(',').map(n => n.trim().split(/\s+as\s+/).pop().trim())))
    // comments and string bodies are prose: a helper NAMED in one is not a helper USED
    const code = src.replace(/\/\*[\s\S]*?\*\//g, '').replace(/(^|[^:'"`])\/\/.*$/gm, '$1')
      .replace(/`(?:\\.|[^`\\])*`|'(?:\\.|[^'\\])*'|"(?:\\.|[^"\\])*"/g, "''")
    const body = importBlocks.reduce((rest, [whole]) => rest.replace(whole, ''), code)
    return exported
      .filter(name => !imported.has(name))
      .filter(name => new RegExp(`(^|[^\\w.$])${name}(?![\\w$])`).test(body))
      .filter(name => !new RegExp(`(const|let|var|function|class)\\s+${name}\\b|\\b${name}\\s*[,}]?\\s*=>|${name}\\s*:`).test(body))
      .map(name => `${file}: uses ${name} without importing it`)
  })
  assert.deepEqual(unimported, [])
})

// #97: an enderman killed Ganesha's body at its own cabin in five seconds, because the fight reflex treated it as one
// more mob to beat. Aiming at its head is what starts that fight, so this body never attacks one and never chases it.
for (const [name, expected] of [
  ['enderman', /not a fight this body can win/],
  ['warden', /not a fight this body can win/],
  ['zombie', null],
  ['spider', null],
  ['creeper', null],
  [undefined, null]
]) {
  test(`attackRefusal: ${name}`, () => {
    const got = attackRefusal(name)
    assert.equal(expected === null, got === null)
    assert.match(got ?? '', expected ?? /^$/)
  })
}

for (const [title, mobs, expected] of [
  ['an enderman at arm\'s length is run from', [{ name: 'enderman', dist: 3 }], 'enderman'],
  ['one keeping its distance is left alone', [{ name: 'enderman', dist: 9 }], null],
  ['exactly at the range still counts', [{ name: 'enderman', dist: 5 }], 'enderman'],
  ['a zombie is a fight, not a flight', [{ name: 'zombie', dist: 1 }], null],
  ['the nearest of the unwinnable ones', [{ name: 'enderman', dist: 4 }, { name: 'warden', dist: 2 }], 'warden'],
  ['nothing about', [], null]
]) {
  test(`fleeUnwinnable: ${title}`, () => assert.equal(fleeUnwinnable(mobs)?.name ?? null, expected))
}

// `hunt` picks its own targets, and the house rule that keeps the starter pen alive (always leave a pair) is worth as
// much in the wild: a hunt that empties a valley has nothing to come back to. Monsters get no such mercy.
for (const [title, mob, found, expected] of [
  ['the nearest grown one', 'cow', [{ id: 1, dist: 9, grown: true }, { id: 2, dist: 3, grown: true }, { id: 3, dist: 5, grown: true }], 2],
  ['a calf is next year\'s herd', 'cow', [{ id: 1, dist: 1, grown: false }, { id: 2, dist: 8, grown: true }, { id: 3, dist: 9, grown: true }, { id: 4, dist: 12, grown: true }], 2],
  ['nothing in sight', 'cow', [], null],
  ['the last pair is left standing', 'cow', [{ id: 1, dist: 1, grown: true }, { id: 2, dist: 2, grown: true }], null],
  ['a pair plus a calf is still a pair', 'cow', [{ id: 1, dist: 1, grown: true }, { id: 2, dist: 2, grown: true }, { id: 3, dist: 1, grown: false }], null],
  ['three is one to spare', 'cow', [{ id: 1, dist: 5, grown: true }, { id: 2, dist: 2, grown: true }, { id: 3, dist: 9, grown: true }], 2],
  ['monsters are not a herd: the last one goes too', 'zombie', [{ id: 7, dist: 4, grown: true }], 7],
  ['goats breed, so goats are kept', 'goat', [{ id: 1, dist: 1, grown: true }, { id: 2, dist: 2, grown: true }], null]
]) {
  test(`huntPick: ${title}`, () => assert.equal(huntPick(mob, found).target?.id ?? null, expected))
}

for (const [title, mob, found, expected] of [
  ['nothing found says so', 'cow', [], /^no cow in sight/],
  ['the last pair says why it is spared', 'cow', [{ id: 1, dist: 1, grown: true }, { id: 2, dist: 2, grown: true }], /only 2 grown cow in sight and a breeding pair stays/],
  ['only calves', 'cow', [{ id: 1, dist: 1, grown: false }], /only 0 grown cow in sight/]
]) {
  test(`huntPick stop: ${title}`, () => assert.match(huntPick(mob, found).stop, expected))
}

test('huntPick: keep can be overridden for a cull', () =>
  assert.equal(huntPick('cow', [{ id: 1, dist: 1, grown: true }, { id: 2, dist: 2, grown: true }], { keep: 0 }).target.id, 1))

test('hunt: walks to the nearest grown one, kills it by id, and picks up after itself', async () => {
  const { api, calls } = fakeApi({
    answers: {
      animals: { found: [{ mob: 'cow', id: 5, at: '10,64,3', dist: 4, grown: true }, { mob: 'cow', id: 6, at: '2,64,2', dist: 9, grown: true }, { mob: 'cow', id: 7, at: '1,64,1', dist: 12, grown: true }] },
      attack: { killed: true }
    }
  })
  const out = await hunt.run(api, { mob: 'cow' })
  assert.deepEqual([out.mob, out.killed, out.at, out.stopped], ['cow', 1, '10,64,3', undefined])
  assert.deepEqual(calls, ['animals mob=cow within=48', 'goto x=10 y=64 z=3 range=3', 'attack mob=cow id=5 leash=24', 'collect range=12'])
})

test('hunt: a kind the body does not know as an animal is found by name and attacked by name', async () => {
  const { api, calls } = fakeApi({
    answers: { animals: { found: [] }, look_around: { each: '4,63,-2 20,63,-9' }, attack: { killed: true } }
  })
  const out = await hunt.run(api, { mob: 'zombie', range: 24 })
  assert.equal(out.killed, 1)
  assert.deepEqual(calls, ['animals mob=zombie within=24', 'look_around mob=zombie range=24', 'goto x=4 y=63 z=-2 range=3', 'attack mob=zombie leash=24', 'collect range=12'])
})

test('hunt: nothing in sight is looked for twice before it is an answer', async () => {
  const { api, calls } = fakeApi({ answers: { animals: { found: [] }, look_around: { each: 'none in range' } } })
  const out = await hunt.run(api, { mob: 'goat' })
  assert.deepEqual([out.killed, out.stopped], [0, 'no goat in sight'])
  assert.equal(calls.filter(c => c.startsWith('animals')).length, 2)
})

test('hunt: the last breeding pair stops it, and says so', async () => {
  const { api } = fakeApi({
    answers: { animals: { found: [{ mob: 'cow', id: 1, at: '1,64,1', dist: 2, grown: true }, { mob: 'cow', id: 2, at: '2,64,2', dist: 3, grown: true }] } }
  })
  const out = await hunt.run(api, { mob: 'cow', count: 4 })
  assert.equal(out.killed, 0)
  assert.match(out.stopped, /a breeding pair stays/)
})

test('hunt: home= is walked to when the hunting is over', async () => {
  const { api, calls } = fakeApi({
    answers: { animals: { found: [{ mob: 'cow', id: 5, at: '9,64,0', dist: 9, grown: true }, { mob: 'cow', id: 6, at: '2,64,2', dist: 3, grown: true }, { mob: 'cow', id: 7, at: '1,64,1', dist: 2, grown: true }] }, attack: { killed: true } }
  })
  await hunt.run(api, { mob: 'cow', home: 'claude-hut' })
  assert.equal(calls.at(-1), 'goto place=claude-hut')
})

// farm.find_spot: score a patch of ground the way someone choosing where to farm would. Flat first (every cell off the
// common level is a block to dig or fill), then water (farmland dries without it), then sky, then how far you walked.
const flat = Array(9).fill(70)
for (const [title, patch, expected] of [
  ['flat, watered and open is the best there is', { tops: flat, water: true, sky: true }, 140],
  ['the same ground without water', { tops: flat, water: false, sky: true }, 115],
  ['under a roof, crops do not grow', { tops: flat, water: true, sky: false }, 125],
  ['one cell a block high costs a dig and some flatness', { tops: [71, ...Array(8).fill(70)], water: true, sky: true }, 128],
  ['a slope is most of a day\'s work', { tops: [70, 71, 72, 70, 71, 72, 70, 71, 72], water: true, sky: true }, 64],
  ['far away is worth less', { tops: flat, water: true, sky: true, away: 80 }, 120]
]) {
  test(`spotScore: ${title}`, () => assert.equal(spotScore(patch).score, expected))
}

for (const [title, patch] of [
  ['ground inside a zone or a saved plan is somebody\'s', { tops: flat, taken: true, water: true, sky: true }],
  ['a lake is not a field', { tops: [70, 70, null, 70, 70, 70, 70, 70, 70], water: true, sky: true }]
]) {
  test(`spotScore: ${title}`, () => assert.equal(spotScore(patch), null))
}

test('spotScore reports the level it would build at, and the work to get there', () => {
  assert.deepEqual(spotScore({ tops: [70, 70, 70, 70, 70, 70, 70, 71, 68], water: true, sky: true, away: 12 }),
    { y: 70, level: 78, work: 3, water: true, sky: true, away: 12, score: 112 })
})

test('bestSpots takes the best few, nearest first when they tie', () => {
  const spots = [{ score: 80, away: 40 }, { score: 120, away: 5 }, { score: 120, away: 2 }, null, { score: 99, away: 1 }]
  assert.deepEqual(bestSpots(spots, 3), [{ score: 120, away: 2 }, { score: 120, away: 5 }, { score: 99, away: 1 }])
})

// farm.find_spot reads the world rather than walking it, so the test gives it a world: a flat shelf with a pond beside
// it, a bumpy field, and a patch of the flat shelf already claimed by a saved plan.
const groundWorld = () => {
  const world = {}
  for (let x = -6; x <= 6; x++) {
    for (let z = -6; z <= 6; z++) {
      const y = x >= 0 ? 64 : 64 + (Math.abs(z) % 3)
      world[`${x},${y},${z}`] = 'grass_block'
    }
  }
  world['3,64,3'] = 'water'
  return world
}

test('farm.find_spot names the flat watered corner, and stands on it', async () => {
  const { api, calls } = fakeApi({ world: groundWorld(), answers: { zones: { zones: [] } } })
  const out = await farmFindSpot.run(api, { w: 2, h: 2, range: 6 })
  assert.equal(out.size, '2x2')
  assert.match(out.best, /^[0-4],64,[-0-9]+$/)
  assert.match(out.spots, /score=\d+ level=100% work=0 water/)
  assert.equal(calls.filter(c => c.startsWith('goto')).length, 1)
})

test('farm.find_spot never offers ground a saved plan already claims', async () => {
  const open = await farmFindSpot.run(fakeApi({ world: groundWorld(), answers: { zones: { zones: [] } } }).api, { w: 2, h: 2, range: 6 })
  const [cx, , cz] = open.best.split(',').map(Number)
  // somebody saves a plan over exactly the patch it just picked: the same search must now offer somewhere else
  const theirs = { name: 'someones-field', kind: 'farm', x: cx, y: 64, z: cz, by: 'Chani', plan: 'ww\nww' }
  const { api } = fakeApi({ world: groundWorld(), places: [theirs], answers: { zones: { zones: [] } } })
  const out = await farmFindSpot.run(api, { w: 2, h: 2, range: 6 })
  const overlaps = ([x, , z]) => x <= cx + 1 && x + 1 >= cx && z <= cz + 1 && z + 1 >= cz
  assert.notEqual(out.best, open.best)
  assert.equal(out.spots.split('; ').map(line => line.split(' ')[0].split(',').map(Number)).some(overlaps), false)
})

test('farm.find_spot says so rather than guessing when there is no open ground', async () => {
  const { api } = fakeApi({ world: {}, answers: { zones: { zones: [] } } })
  await assert.rejects(farmFindSpot.run(api, { w: 3, h: 3, range: 4 }), /no 3x3 patch of open ground/)
})

// the tool picker takes whatever digs fastest, and a sword digs melons, pumpkins, leaves and cobwebs fastest: Jizo's sword wore out
// on melons and left the body unarmed. A weapon digs only when nothing else can harvest the block
for (const [name, tools, expected] of [
  ['axe beats sword on a melon', [{ name: 'wooden_sword', time: 100, harvests: true }, { name: 'stone_axe', time: 150, harvests: true }, { name: null, time: 1500, harvests: true }], 'stone_axe'],
  ['bare hand when only a sword is carried', [{ name: 'iron_sword', time: 100, harvests: true }, { name: null, time: 1500, harvests: true }], null],
  ['cobweb: only the sword harvests, so keep it', [{ name: 'iron_sword', time: 400, harvests: true }, { name: null, time: 20000, harvests: false }], undefined],
  ['fastest of the peaceful tools', [{ name: 'dirt', time: 900, harvests: true }, { name: 'stone_shovel', time: 200, harvests: true }, { name: 'trident', time: 100, harvests: true }], 'stone_shovel']
]) test(`peacefulTool: ${name}`, () => assert.equal(peacefulTool(tools)?.name, expected))

// deposit with a misspelt parameter (item= for items=) silently put the whole inventory, tools included, into the chest
const CARRIED = [{ name: 'stone_pickaxe', count: 1 }, { name: 'dirt', count: 40 }]
for (const [name, args, expected] of [
  ['named items', { items: { dirt: 4 } }, { dirt: 4 }],
  ['everything, when asked for', { all: true }, CARRIED],
  ['item= names one thing (all of it): since 22:20Z the chests take item= like every other action', { x: 1, y: 2, z: 3, item: 'dirt' }, [{ name: 'dirt', count: undefined }]],
  ['nothing named', { x: 1, y: 2, z: 3 }, { error: `deposit needs items='{"dirt":4}' (or all=true for everything you carry, tools included)` }]
]) test(`depositWanted: ${name}`, () => assert.deepEqual(depositWanted(args, CARRIED), expected))

// a misspelt parameter (deposit item=, place block=) was silently ignored: replies now name what the action never read
for (const [name, given, read, expected] of [
  ['all read', { x: 1, y: 2 }, a => a.x + a.y, []],
  ['one never read', { x: 1, item: 'dirt' }, a => a.x, ['item']],
  ['spread reads everything', { x: 1, item: 'dirt' }, a => ({ ...a }), []],
  ['a missing key that is asked for is no problem', { x: 1 }, a => a.items ?? a.x, []],
  ['nested values stay plain', { blocks: [{ x: 1 }] }, a => a.blocks[0].x, []]
]) {
  test(`trackReads: ${name}`, () => {
    const tracked = trackReads(given)
    read(tracked.args)
    assert.deepEqual(tracked.unread(), expected)
  })
}
for (const [name, unread, ok, source, expected] of [
  ['nothing unread', [], true, '', null],
  ['unread on success', ['item'], true, 'a.items', 'item (not a parameter here, or not used this time: check the name in the guide)'],
  ['a failure before a real parameter was read does not blame it', ['count'], false, 'a.count ?? 1', null],
  ['a failure still names a key the action never mentions', ['block', 'count'], false, 'a.count ?? a.item', 'block (not a parameter here, or not used this time: check the name in the guide)']
]) test(`ignoredParams: ${name}`, () => assert.equal(ignoredParams(unread, ok, source), expected))

// Vivenna's furnace "stuck" at stillCooking=20 for a whole day: furnace_take never said that it had no fuel
for (const [name, slots, expected] of [
  ['done and empty', { input: null, fuel: null, burning: false }, { stillCooking: 0 }],
  ['cooking', { input: { name: 'raw_iron', count: 3 }, fuel: { name: 'coal', count: 2 }, burning: true }, { stillCooking: 3 }],
  ['burning its last fuel', { input: { name: 'raw_iron', count: 3 }, fuel: null, burning: true }, { stillCooking: 3 }],
  ['out of fuel', { input: { name: 'raw_iron', count: 20 }, fuel: null, burning: false }, { stillCooking: 20, stuck: 'raw_iron is waiting but the fire is out and the fuel slot is empty: smelt fuel=coal count=20 x= y= z= adds fuel only (no item= needed; charcoal, planks or logs work too)' }],
  ['fuel that does not burn', { input: { name: 'raw_iron', count: 2 }, fuel: { name: 'stick', count: 1 }, burning: false }, { stillCooking: 2, stuck: 'raw_iron is waiting and stick is in the fuel slot, but nothing burns: that input cannot be smelted here, or the output slot holds something else. chest_contents shows the slots' }]
]) test(`furnaceReport: ${name}`, () => assert.deepEqual(furnaceReport(slots), expected))

// Jizo drowned with the surfacing reflex firing three times: it only pressed jump once, and the running task kept steering the body
for (const [name, state, expected] of [
  ['plenty of air', { headInWater: true, inWater: true, oxygen: 15, surfacing: false }, null],
  ['low air under water', { headInWater: true, inWater: true, oxygen: 8, surfacing: false }, 'start'],
  ['low reading on dry land is a bridge misfire', { headInWater: false, inWater: false, oxygen: 3, surfacing: false }, null],
  ['still short of air: keep swimming up', { headInWater: false, inWater: true, oxygen: 10, surfacing: true }, 'hold'],
  ['breathed enough', { headInWater: false, inWater: true, oxygen: 18, surfacing: true }, 'stop'],
  ['out of the water', { headInWater: false, inWater: false, oxygen: 5, surfacing: true }, 'stop']
]) test(`airReflex: ${name}`, () => assert.equal(airReflex(state), expected))

// Chani drowned one block under the surface: the reflex walked towards open water three blocks away and never arrived.
// Straight up is the answer whenever the column over the head is water all the way to the sky.
for (const [name, names, expected] of [
  ['open water over my head', ['water', 'water', 'air'], true],
  ['deep water: no air in the scan, but nothing in the way either', ['water', 'water', 'water'], true],
  ['under my own farmland (Jizo)', ['water', 'farmland', 'air'], false],
  ['under a hull of slabs', ['oak_slab'], false],
  ['kelp and seagrass are still water', ['kelp', 'seagrass', 'air'], true],
  ['a bubble column carries me up', ['bubble_column', 'water', 'air'], true],
  ['nothing overhead at all', [], true]
]) test(`openAbove: ${name}`, () => assert.equal(openAbove(names), expected))

// ...and when the walk was the wrong answer, give it two seconds to prove itself and then just swim
for (const [name, state, expected] of [
  ['just started', { startedAt: 0, now: 500, startY: 54, y: 54, swimming: false }, false],
  ['two seconds and not risen: drop the path and swim', { startedAt: 0, now: 2100, startY: 54, y: 54.2, swimming: false }, true],
  ['rising: the walk is working', { startedAt: 0, now: 3000, startY: 54, y: 55.1, swimming: false }, false],
  ['sinking while it walks is the worst case', { startedAt: 0, now: 2500, startY: 54, y: 52, swimming: false }, true],
  ['already swimming: there is no path to drop', { startedAt: 0, now: 9000, startY: 54, y: 54, swimming: true }, false]
]) test(`surfacingStalled: ${name}`, () => assert.equal(surfacingStalled(state), expected))

// goto walked straight through planted fields and broke the crops: the pathfinder routes round anything a step destroys,
// while farmland itself stays walkable (an empty bed is the only path across some fields)
for (const [name, block, expected] of [
  ['wheat', 'wheat', true],
  ['carrots', 'carrots', true],
  ['potatoes', 'potatoes', true],
  ['beetroots', 'beetroots', true],
  ['a melon stem', 'melon_stem', true],
  ['an attached pumpkin stem', 'attached_pumpkin_stem', true],
  ['sweet berries', 'sweet_berry_bush', true],
  ['farmland is the path between the beds', 'farmland', false],
  ['grass is not a crop', 'short_grass', false],
  ['a torch is not a crop', 'torch', false]
]) test(`breaksUnderfoot: ${name}`, () => assert.equal(breaksUnderfoot(block), expected))

// wedged: the server pins a body whose hitbox lies flush against a block. Which cells can that be? (feet and head level, per flush side)
for (const [name, pos, expected] of [
  ['flush to the west (the leaf at head height by dan-farm)', { x: 4.3, y: 65, z: -99.4 }, [[3, 65, -100], [3, 66, -100]]],
  ['flush to the east, and so far south that the hitbox spans two z cells', { x: 4.7, y: 65, z: 10.9 }, [[5, 65, 10], [5, 66, 10], [5, 65, 11], [5, 66, 11], [4, 65, 11], [4, 66, 11]]],
  ['flush to the north', { x: 0.5, y: 70, z: -5.7 }, [[0, 70, -7], [0, 71, -7]]],
  ['in a corner', { x: 2.3, y: 64, z: 8.7 }, [[1, 64, 8], [1, 65, 8], [2, 64, 9], [2, 65, 9]]],
  ['in the open', { x: 0.5, y: 64, z: 0.5 }, []]
]) test(`flushCells: ${name}`, () => assert.deepEqual(flushCells(pos).map(c => [c.x, c.y, c.z]), expected))

// flock.breed mob=: what each farm animal eats, picked from what the body carries
for (const [name, mob, carried, expected] of [
  ['cows eat wheat', 'cow', ['bread', 'wheat'], 'wheat'],
  ['sheep too', 'sheep', ['wheat'], 'wheat'],
  ['pigs take any root', 'pig', ['wheat', 'potato'], 'potato'],
  ['chickens eat seeds', 'chicken', ['wheat', 'melon_seeds'], 'melon_seeds'],
  ['nothing suitable carried', 'pig', ['wheat'], null],
  ['not a farm animal', 'zombie', ['wheat'], null]
]) test(`breedingFood: ${name}`, () => assert.equal(breedingFood(mob, carried), expected))

// wedged flush against a block that is no leaf (a step up, a wall): first try stepping 2 cm off it, the server takes so small a move
for (const [name, pos, expected] of [
  ['flush to the south (the grass step at 76,66,-128)', { x: 76.4, y: 66, z: -128.3 }, { x: 76.4, y: 66, z: -128.32 }],
  ['flush to the west', { x: 4.3, y: 65, z: -99.4 }, { x: 4.32, y: 65, z: -99.4 }],
  ['in a corner: both axes', { x: 2.3, y: 64, z: 8.7 }, { x: 2.32, y: 64, z: 8.68 }],
  ['in the open: unchanged', { x: 0.5, y: 64, z: 0.5 }, { x: 0.5, y: 64, z: 0.5 }]
]) {
  test(`nudgeAway: ${name}`, () => {
    const moved = nudgeAway(pos)
    assert.deepEqual([moved.x, moved.y, moved.z].map(v => Math.round(v * 1000) / 1000), [expected.x, expected.y, expected.z])
  })
}

// boxes are { min: [x,y,z], max: [x,y,z] }; the block is a full cube at x77 y66 z-128
const ridge = { min: [77, 66, -128], max: [78, 67, -127] }
const playerAt = (x, y, z, w = 0.3001) => ({ min: [x - w, y, z - w], max: [x + w, y + 1.8, z + w] })
const clampedOffsetCases = [
  ['a hitbox edge that rounds 1e-14 past the block face is still stopped (the step-up wedge)', playerAt(77.5, 66.42, -128.3001), 2, 0.0255, 0],
  ['a block well ahead lets the whole step through', playerAt(77.5, 66, -129.5), 2, 0.1, 0.1],
  ['a block just ahead cuts the step short', playerAt(77.5, 66, -128.35, 0.3), 2, 0.1, 0.05],
  ['moving away from a touched block is free', playerAt(77.5, 66.42, -128.3001), 2, -0.1, -0.1],
  ['above the block: nothing in the way', playerAt(77.5, 67, -128.3001), 2, 0.1, 0.1],
  ['sliding along a wall I touch on x: the next wall block does not catch me', playerAt(76.6999, 66, -128.9), 2, 0.8, 0.8],
  ['falling onto the block lands on its top', playerAt(77.5, 67.05, -127.5), 1, -0.2, -0.05],
  ['coming from the far side (negative step) is stopped too', playerAt(77.5, 66, -126.6999), 2, -0.05, 0]
]
for (const [name, player, axis, offset, expected] of clampedOffsetCases) {
  test(`clampedOffset: ${name}`, () => {
    assert.ok(Math.abs(clampedOffset(ridge, player, axis, offset) - expected) < 1e-6)
  })
}

// distances: how far each animal I am leading is from me (gone ones are left out); holding: I stopped to let them catch up
const leadVerdictCases = [
  ['all close: walk on', { distances: [2, 3.5], holding: false }, 'go'],
  ['one falls behind: stop and let it catch up', { distances: [2, 6.5], holding: false }, 'hold'],
  ['while holding, a half-caught-up animal is not enough', { distances: [2, 5], holding: true }, 'hold'],
  ['while holding, all close again: walk on', { distances: [2, 3.9], holding: true }, 'go'],
  ['one lost interest (beyond the 10 blocks they follow from): walk back to it', { distances: [2, 11], holding: false }, 'fetch'],
  ['fetch wins while holding too', { distances: [9.5], holding: true }, 'fetch'],
  ['none left', { distances: [], holding: false }, 'lost'],
  ['held 12 s and it still has not come (a fence between us?): go and get it', { distances: [2, 7], holding: true, heldFor: 12 }, 'fetch'],
  ['held only 5 s: keep waiting', { distances: [2, 7], holding: true, heldFor: 5 }, 'hold'],
  ['fetched 3 times without getting any nearer the goal: give up and say so', { distances: [2, 11], holding: false, fetchesSinceProgress: 3 }, 'giveup'],
  ['2 fetches without progress: try once more', { distances: [2, 11], holding: false, fetchesSinceProgress: 2 }, 'fetch'],
  ['the spot cannot be walked to (a pen with a corner gate, a fence in it): say so at once, do not stand there till the stall alarm', { distances: [2, 3], holding: false, noPath: true }, 'noway'],
  ['an animal to fetch matters more than a path that is not needed yet', { distances: [2, 11], holding: false, noPath: true }, 'fetch']
]
for (const [name, state, expected] of leadVerdictCases) {
  test(`leadVerdict: ${name}`, () => assert.equal(leadVerdict(state), expected))
}

// started: when this body loaded its code; changed: { file: mtime }; now: the time. Null = nothing to tell (yet)
const staleCodeCases = [
  ['nothing changed since I started', 1000, { 'bot.mjs': 900, 'lib.mjs': 1000 }, 900000, null],
  ['one file is newer', 1000, { 'bot.mjs': 900, 'lib.mjs': 1500 }, 1000000, ['lib.mjs']],
  ['both are newer', 1000, { 'bot.mjs': 2000, 'lib.mjs': 1500 }, 1000000, ['bot.mjs', 'lib.mjs']],
  ['a file that is gone (no mtime) is not news', 1000, { 'bot.mjs': undefined }, 900000, null],
  ['Claude is still editing (a change under 15 minutes old): wait. At 3 minutes every push cost every agent a restart: 33 in 88 minutes', 1000, { 'bot.mjs': 2000, 'lib.mjs': 800000 }, 900000, null],
  ['10 minutes of quiet is not enough yet', 1000, { 'bot.mjs': 2000 }, 602000, null],
  ['15 minutes of quiet: tell', 1000, { 'bot.mjs': 2000 }, 902000, ['bot.mjs']],
  ['an afternoon of edits never left 15 quiet minutes, and Kettricken ran 2-hour-old code: a body 45 minutes old is told after 2 quiet minutes', 1000, { 'bot.mjs': 2600000 }, 2721000, ['bot.mjs']],
  ['45 minutes old, but the last change is seconds old (mid-batch): wait', 1000, { 'bot.mjs': 2700000 }, 2721000, null],
  ['a young body still waits for the 15 minutes', 1000, { 'bot.mjs': 1500000 }, 1700000, null]
]
for (const [name, started, changed, now, expected] of staleCodeCases) {
  test(`staleCode: ${name}`, () => assert.deepEqual(staleCode(started, changed, now), expected))
}
test('wakeWorthy: code_updated ends a wait', () => assert.equal(wakeWorthy({ type: 'code_updated' }, 'Claude'), true))

const replantSpotCases = [
  ['wheat goes back on the farmland below', 'wheat', {}, { against: [0, -1, 0], face: [0, 1, 0] }],
  ['cocoa goes back on the side of the log it hung from (north)', 'cocoa', { facing: 'north' }, { against: [0, 0, -1], face: [0, 0, 1] }],
  ['cocoa hanging from a log to the east', 'cocoa', { facing: 'east' }, { against: [1, 0, 0], face: [-1, 0, 0] }]
]
for (const [name, crop, props, expected] of replantSpotCases) {
  test(`replantSpot: ${name}`, () => assert.deepEqual(replantSpot(crop, props), expected))
}
test('ripeCrop: cocoa is ripe at age 2 and replants with beans', () => assert.deepEqual([ripeCrop('cocoa', 2), ripeCrop('cocoa', 1)], ['cocoa_beans', null]))

// bamboo and sugar cane: cut the second segment, the base stays and regrows. Arguments: this block, the one below, the one below that
const isStalkCutCases = [
  ['second segment of bamboo', 'bamboo', 'bamboo', 'grass_block', true],
  ['the base stays', 'bamboo', 'grass_block', 'dirt', false],
  ['third segment falls with the second: no need to dig it', 'sugar_cane', 'sugar_cane', 'sugar_cane', false],
  ['second segment of sugar cane', 'sugar_cane', 'sugar_cane', 'sand', true],
  ['not a stalk', 'wheat', 'farmland', 'dirt', false]
]
for (const [name, block, below, belowThat, expected] of isStalkCutCases) {
  test(`isStalkCut: ${name}`, () => assert.equal(isStalkCut(block, below, belowThat), expected))
}

// the server's "leave bed" sometimes never reaches the body when others skip the night: in bed in broad daylight = get up myself
const oversleepingCases = [
  ['asleep at night', { asleep: true, timeOfDay: 18000, thundering: false }, false],
  ['asleep just after sunrise: the wake-up may still be on its way', { asleep: true, timeOfDay: 100, thundering: false }, false],
  ['asleep in broad daylight', { asleep: true, timeOfDay: 600, thundering: false }, true],
  ['asleep at noon in a thunderstorm (beds work then)', { asleep: true, timeOfDay: 6000, thundering: true }, false],
  ['awake by day', { asleep: false, timeOfDay: 6000, thundering: false }, false],
  ['asleep at dusk, when beds start to work', { asleep: true, timeOfDay: 12600, thundering: false }, false]
]
// asking the server to leave the bed did nothing for Miles and Kalessin (asleep at time 2314 and 5936): it had thrown them out
// long before and only the body's own flag was left. By day nobody is in bed on the server, so after 6 s the body says so itself
for (const [name, state, expected] of [
  ['not oversleeping', { oversleeping: false, forMs: 0 }, null],
  ['just noticed: ask the server', { oversleeping: true, forMs: 0 }, 'ask'],
  ['still asking', { oversleeping: true, forMs: 5000 }, 'ask'],
  ['the server does not answer: it thinks I am up already', { oversleeping: true, forMs: 6000 }, 'declare']
]) {
  test(`wakeStep: ${name}`, () => assert.equal(wakeStep(state), expected))
}
for (const [name, state, expected] of oversleepingCases) {
  test(`oversleeping: ${name}`, () => assert.equal(oversleeping(state), expected))
}

const gatesLeftOpenCases = [
  ['a gate I walked through and left open', ['(1, 2, 3)'], [], ['(1, 2, 3)'], [[1, 2, 3]]],
  ['one the reflex already shut', ['(1, 2, 3)'], [], [], []],
  ['one I hold open on purpose', ['(1, 2, 3)'], ['(1, 2, 3)'], ['(1, 2, 3)'], []],
  ['negative coordinates', ['(20, 67, -119)', '(-4, 70, 9)'], [], ['(20, 67, -119)', '(-4, 70, 9)'], [[20, 67, -119], [-4, 70, 9]]],
  ['none passed', [], [], ['(1, 2, 3)'], []]
]
for (const [name, opened, held, openNow, expected] of gatesLeftOpenCases) {
  test(`gatesLeftOpen: ${name}`, () => assert.deepEqual(gatesLeftOpen(new Set(opened), new Set(held), key => openNow.includes(key)), expected))
}

const transferFixCases = [
  ['exactly what was asked', [{ name: 'wheat', count: 8 }], { wheat: 2 }, { wheat: 10 }, { back: [], more: [] }],
  ['a whole stack came along (asked 8, got 24): put 16 back', [{ name: 'wheat', count: 8 }, { name: 'bread', count: 6 }], { bread: 2 }, { wheat: 24, bread: 8 }, { back: [{ name: 'wheat', count: 16 }], more: [] }],
  ['a transfer got lost: ask for the rest', [{ name: 'coal', count: 4 }], { coal: 1 }, { coal: 3 }, { back: [], more: [{ name: 'coal', count: 2 }] }],
  ['nothing arrived at all', [{ name: 'coal', count: 4 }], {}, {}, { back: [], more: [{ name: 'coal', count: 4 }] }]
]
for (const [name, plan, before, after, expected] of transferFixCases) {
  test(`transferFix: ${name}`, () => assert.deepEqual(transferFix(plan, before, after), expected))
}

// a pen on a map: '.' floor at height 0, '#' fence (1.5), 'B' full block (1), 'G' open gate (floor), 'H' fence raised by one (2.5), ' ' floor outside. Start is 'S' (floor)
const penMap = rows => (x, z) => {
  const c = rows[z]?.[x]
  if (c === undefined) return [0]
  return [{ '#': 1.5, B: 1, H: 2.5, W: 4, T: 2 }[c] ?? 0]
}
const startOf = rows => { const z = rows.findIndex(r => r.includes('S')); return [rows[z].indexOf('S'), 0, z] }
const penCases = [
  ['a shut ring of fence holds', ['      ', ' #### ', ' #S.# ', ' #..# ', ' #### ', '      '], { enclosed: true, cells: 4 }],
  ['an open gate is a way out', ['      ', ' #### ', ' #S.# ', ' #..# ', ' #G## ', '      '], { enclosed: false, via: '2,0,4' }],
  ['a block inside next to the fence is a step over it', ['      ', ' #### ', ' #SB# ', ' #..# ', ' #### ', '      '], { enclosed: false, via: '3,1,2 4,1.5,2 5,0,2' }],
  // Vivenna's pen, 09-19: the fence was raised on both sides of a raised floor tile, but cows stepped off its corner onto the ordinary fence next along and were out by morning
  ['a raised tile in the corner: the fence DIAGONALLY next to it is a step too', ['      ', ' ##HH ', ' #SBH ', ' #..# ', ' #### ', '      '], { enclosed: false, via: '3,1,2 4,1.5,3 5,0,3' }],
  ['no squeezing between two posts that only touch at the corner', ['      ', ' ###  ', ' #S#  ', ' #..# ', ' #### ', '      '], { enclosed: true, cells: 3 }],
  ['a wall of full blocks is only one step high', ['     ', ' BBB ', ' BSB ', ' BBB ', '     '], { enclosed: false, via: '3,1,2 4,0,2 5,0,2' }]
]
for (const [name, rows, expected] of penCases) {
  test(`penLeak: ${name}`, () => assert.deepEqual(penLeak({ start: startOf(rows), topsAt: penMap(rows), radius: 2 }), expected))
}
// Ganesha, 21:08Z: three fences placed, reply `placed=0` and nothing else. The blocks stood there; the count did not say why it was 0
test('placeOutcome: cells that already held the block are told', () => assert.deepEqual(placeOutcome(0, [], 'placed', 3), { placed: 0, alreadyThere: 3 }))
test('placeOutcome: none already there, no word about it', () => assert.deepEqual(placeOutcome(2, [], 'placed', 0), { placed: 2 }))
// AhuraMazda trusted the @x,y,z of a place reply (it is where the BODY stands), dug what he thought was his own bed
// remnant and hit someone else's pressure plate. The reply now carries the placed cells themselves, read back off the world
for (const [name, cells, expected] of [
  ['one block: its own cell', [{ x: 59, y: 67, z: -120, name: 'red_bed' }], { placed: 1, at: '59,67,-120 (red_bed)' }],
  ['a batch: every cell', [{ x: 1, y: 2, z: 3, name: 'oak_fence' }, { x: 2, y: 2, z: 3, name: 'oak_fence' }], { placed: 1, at: '1,2,3 2,2,3 (oak_fence)' }],
  ['a long batch is cut short', Array.from({ length: 9 }, (_, i) => ({ x: i, y: 2, z: 3, name: 'oak_fence' })), { placed: 1, at: '0,2,3 1,2,3 2,2,3 3,2,3 4,2,3 5,2,3 and 3 more (oak_fence)' }],
  ['two kinds in one batch', [{ x: 1, y: 2, z: 3, name: 'oak_fence' }, { x: 2, y: 2, z: 3, name: 'oak_fence_gate' }], { placed: 1, at: '1,2,3 (oak_fence) 2,2,3 (oak_fence_gate)' }],
  ['nothing confirmed: no at= at all', [], { placed: 1 }]
]) {
  test(`placeOutcome: ${name}`, () => assert.deepEqual(placeOutcome(1, [], 'placed', 0, cells), expected))
}
test('placeOutcome: nothing placed, some skipped, some there: still an error, and it says so', () => assert.deepEqual(placeOutcome(0, [{ at: '1,2,3', why: 'cannot get within reach' }], 'placed', 2), { error: 'placed nothing: 1 cannot get within reach (first 1,2,3); 2 were already there' }))

// Ganesha's pen, 21:08Z: three fences missing in the east wall, on level ground. via= named a slope 15-20 blocks away ("the first climb"), and they
// scanned the wrong place. The telling cell is where the way out leaves the fence ring. 'v' is ground one lower
test('penLeak: a wide gap on level ground is named, not the first slope beyond it', () => {
  const rows = ['          ', ' #####    ', ' #S..   vv', ' #...   vv', ' #...   vv', ' #####    ', '          ']
  const topsAt = (x, z) => [{ '#': 1.5, v: -1 }[rows[z]?.[x]] ?? 0]
  assert.deepEqual(penLeak({ start: startOf(rows), topsAt, radius: 7 }), { enclosed: false, via: '5,0,2' })
})
test('penLeak: a wide gap and no slope anywhere', () => {
  const rows = ['          ', ' #####    ', ' #S..     ', ' #...     ', ' #####    ', '          ']
  assert.deepEqual(penLeak({ start: startOf(rows), topsAt: penMap(rows), radius: 7 }), { enclosed: false, via: '5,0,2' })
})
// Dan's starter pen is 17 long: checked from its north end, the far end lay beyond 12 columns and an intact pen read LEAKS (from 2 cells further south it held)
test('penLeak: a long pen checked from one end still holds', () => {
  const rows = ['###', '#S#', ...Array(18).fill('#.#'), '###']
  assert.deepEqual(penLeak({ start: startOf(rows), topsAt: penMap(rows) }), { enclosed: true, cells: 19 })
})
// claude-test-pen at 113,67,-125 stands on a skin of grass over a cave, and pen.check called it a leak by a path three
// blocks UNDER its floor: every standable surface in a neighbouring column counted, however deep. An animal that steps
// off an edge falls to the FIRST surface below it, and never through the block a fence stands on.
test('penLeak: a cave under the pen is not a way out', () => {
  const rows = ['      ', ' #### ', ' #S.# ', ' #..# ', ' #### ', '      ']
  // the floor is a skin of grass over the cavern: every floor column has a second standable surface far below
  const cave = (x, z) => rows[z]?.[x] === '#' ? penMap(rows)(x, z) : [...penMap(rows)(x, z), -3]
  assert.deepEqual(penLeak({ start: startOf(rows), topsAt: cave, radius: 2 }), { enclosed: true, cells: 4 })
})
test('penLeak: a cave under the fence line is not a way out either', () => {
  const rows = ['      ', ' #### ', ' #S.# ', ' #..# ', ' #### ', '      ']
  // the fence columns carry no standable top at all, only the cave below and the ledge their post leaves
  const tops = (x, z) => rows[z]?.[x] === '#' ? [-3] : [0]
  const rims = (x, z) => rows[z]?.[x] === '#' ? [0] : []
  assert.deepEqual(penLeak({ start: startOf(rows), topsAt: tops, rimsAt: rims, radius: 2 }), { enclosed: true, cells: 4 })
})
test('penLeak: a drop to lower ground is still a way out', () => {
  const rows = ['      ', ' #### ', ' #S.  ', ' #..# ', ' #### ', '      ']
  const lower = (x, z) => rows[z]?.[x] === ' ' ? [-3] : penMap(rows)(x, z)
  assert.equal(penLeak({ start: startOf(rows), topsAt: lower, radius: 2 }).enclosed, false)
})

test('penLeak: higher ground outside lets them in, not out', () => {
  const rows = [' BBBB ', ' #### ', ' #S.# ', ' #..# ', ' #### ', '      ']
  assert.deepEqual(penLeak({ start: startOf(rows), topsAt: penMap(rows), radius: 2 }), { enclosed: true, cells: 4 })
})

const ev = n => JSON.stringify({ seq: n, type: 'chat' })
const tailCases = [
  ['whole lines', [ev(1), ev(2), ''].join('\n'), 10, [1, 2]],
  ['only the last few', [ev(1), ev(2), ev(3), ''].join('\n'), 2, [2, 3]],
  ['a first line cut in half by the read, and one half written', ['"type":"chat"}', ev(5), '{"seq":6,"ty'].join('\n'), 10, [5]],
  ['empty file', '', 10, []]
]
for (const [name, text, limit, expected] of tailCases) {
  test(`parseEventTail: ${name}`, () => assert.deepEqual(parseEventTail(text, limit).map(e => e.seq), expected))
}

const fillCases = [
  ['the bucket filled', 'water_bucket', { holding: 'water_bucket' }],
  ['still empty: say so, ok would be a lie', 'bucket', { error: 'the bucket is still empty: stand on the shore 1-2 blocks from the source with a clear view of it, not in the water, and fill again' }],
  ['hand empty', undefined, { error: 'the bucket is still empty: stand on the shore 1-2 blocks from the source with a clear view of it, not in the water, and fill again' }]
]
for (const [name, held, expected] of fillCases) {
  test(`fillOutcome: ${name}`, () => assert.deepEqual(fillOutcome(held), expected))
}

const still = { hasGoal: true, moved: 0, digging: false, seconds: 4, pathfinderMoving: false }
const deadWalkCases = [
  ['no path, no nodes, legs idle for 4 s: this walk is dead, end it so the task can go on', { ...still, path: { status: 'partial', nodes: [] } }, true],
  ['noPath counts too', { ...still, path: { status: 'noPath', nodes: [] } }, true],
  ['a partial path WITH nodes is still being walked', { ...still, path: { status: 'partial', nodes: ['1,2,3'] } }, false],
  ['a found path is the other kind of stall: leave it to the 12 s alarm', { ...still, path: { status: 'success', nodes: ['1,2,3'] } }, false],
  ['only 2 s: the search may still be running', { ...still, seconds: 2, path: { status: 'partial', nodes: [] } }, false],
  ['digging is work', { ...still, digging: true, path: { status: 'partial', nodes: [] } }, false],
  ['it moved', { ...still, moved: 1, path: { status: 'partial', nodes: [] } }, false],
  ['no goal, nothing to end', { ...still, hasGoal: false, path: { status: 'partial', nodes: [] } }, false],
  ['never planned yet', { ...still, path: null }, false]
]
for (const [name, sample, expected] of deadWalkCases) {
  test(`deadWalk: ${name}`, () => assert.equal(deadWalk(sample), expected))
}

const obstacleCases = [
  ['a plain block into air', 'cobblestone', 'air', 'stone', null],
  ['grass gives way to a block', 'dirt', 'short_grass', 'grass_block', null],
  ['a seed on farmland', 'wheat_seeds', 'air', 'farmland', null],
  ['a seed on dirt (the farmland dried back): say so, not "out of reach"', 'wheat_seeds', 'air', 'dirt', 'wheat_seeds needs farmland under it, and there is dirt: till that block first'],
  ['a carrot where carrots already grow', 'carrot', 'carrots', 'farmland', 'carrots is already there (not a block, but in the way): harvest or dig it first'],
  ['a block where a flower stands', 'cobblestone', 'poppy', 'grass_block', 'poppy is already there (not a block, but in the way): harvest or dig it first'],
  ['a sapling on stone', 'oak_sapling', 'air', 'stone', 'oak_sapling needs dirt or grass under it, and there is stone'],
  ['a sapling on grass', 'cherry_sapling', 'air', 'grass_block', null],
  ['sugar cane is its own business', 'sugar_cane', 'air', 'sand', null]
]
for (const [name, item, existing, below, expected] of obstacleCases) {
  test(`placeObstacle: ${name}`, () => assert.equal(placeObstacle(item, existing, below), expected))
}

const swords = ['cherry', 'oak', 'birch'].map(w => ({ [`${w}_planks`]: 2, stick: 1 }))
const furnaces = [{ cobblestone: 8 }, { cobbled_deepslate: 8 }, { blackstone: 8 }]
const shortfallCases = [
  ['5 oak planks, no stick (Ganesha): the stick is what is short, not "cherry_planks"', swords, { oak_planks: 5 }, 'stick:1'],
  ['nothing at all: any planks will do', swords, {}, 'any planks:2 stick:1'],
  ['one plank short of the wood I do carry', swords, { birch_planks: 1, stick: 3 }, 'any planks:1'],
  ['cousins without a common name are listed', furnaces, { cobblestone: 3 }, 'cobblestone:5 (or cobbled_deepslate, blackstone)'],
  ['a single recipe', [{ wheat: 3 }], { wheat: 1 }, 'wheat:2']
]
for (const [name, recipes, have, expected] of shortfallCases) {
  test(`craftShortfall: ${name}`, () => assert.equal(craftShortfall(recipes, have), expected))
}

const spots = ['a', 'b', 'c', 'd']
const pick = (over = {}) => mineTargets({ nearby: spots, wanted: 2, inZone: () => false, wet: () => false, allowWet: false, what: 'sand within 48 blocks', ...over })
const mineTargetCases = [
  ['the nearest ones', {}, { found: ['a', 'b'] }],
  ['protected ones are passed over', { inZone: p => p === 'a' }, { found: ['b', 'c'] }],
  ['wet ones are passed over: bodies drown fetching sand from a lake', { wet: p => p !== 'd' }, { found: ['d'], skippedWet: 3 }],
  ['all of it under water', { wet: () => true }, { error: 'the only sand within 48 blocks lies in or next to water, where mining bodies drown: dig it block by block from the shore (dig x= y= z=), or pass wet=true if you take the risk' }],
  ['wet=true takes them anyway', { wet: () => true, allowWet: true }, { found: ['a', 'b'] }],
  ['all of it protected', { inZone: () => true }, { error: "the only sand within 48 blocks is inside protected zones (someone's build): go further away and retry" }],
  ['none left', { nearby: [] }, { error: 'no more sand within 48 blocks' }]
]
for (const [name, over, expected] of mineTargetCases) {
  test(`mineTargets: ${name}`, () => assert.deepEqual(pick(over), expected))
}

const builtCases = [
  ['cobblestone', true], ['oak_fence', true], ['cherry_fence_gate', true], ['cobblestone_wall', true], ['oak_planks', true], ['glass_pane', true],
  ['white_wool', true], ['oak_door', true], ['red_bed', true], ['chest', true], ['farmland', true], ['wheat', true], ['lantern', true], ['stone_bricks', true],
  ['oak_stairs', true], ['torch', true], ['ladder', true],
  ['stone', false], ['dirt', false], ['grass_block', false], ['oak_log', false], ['oak_leaves', false], ['sand', false], ['gravel', false], ['deepslate', false], ['andesite', false]
]
for (const [name, expected] of builtCases) {
  test(`looksBuilt: ${name}`, () => assert.equal(looksBuilt(name), expected))
}

const verbCases = [
  ['till counts under its own name', 5, [miss(112, "can't turn stone into farmland")], { tilled: 5, skipped: 1, why: "1 can't turn stone into farmland (first 112,65,-135)" }],
  ['nothing tilled', 0, [miss(112, 'still dirt: is there a block on top of it?')], { error: 'tilled nothing: 1 still dirt: is there a block on top of it? (first 112,65,-135)' }],
  ['all tilled', 4, [], { tilled: 4 }]
]
for (const [title, done, skipped, expected] of verbCases) {
  test(`placeOutcome with a verb: ${title}`, () => assert.deepEqual(placeOutcome(done, skipped, 'tilled'), expected))
}

for (const [name, expected] of [['short_grass', true], ['tall_grass', true], ['fern', true], ['snow', true], ['leaf_litter', true], ['dead_bush', true], ['air', false], ['water', false], ['wheat', false], ['poppy', false], ['oak_fence', false]]) {
  test(`isGroundCover: ${name}`, () => assert.equal(isGroundCover(name), expected))
}

// the pathfinder has a path but its physics preview says "can't", so it stands with no key pressed until 'stuck' resets it, for ever
const NUDGE = { hasGoal: true, busy: false, idleTicks: 30, node: { x: 167.5, y: 103, z: -131.5 }, pos: { x: 168.49, y: 104, z: -132.59 }, collided: false }
for (const [name, change, expected] of [
  ['idle with a path: walk at the node', {}, { dx: -0.99, dz: 1.09, jump: false }],
  ['not idle for long yet', { idleTicks: 29 }, null],
  ['no goal', { hasGoal: false }, null],
  ['digging, placing or at a door', { busy: true }, null],
  ['no node left', { node: undefined }, null],
  ['already on the node: the pathfinder will shift it', { pos: { x: 167.6, y: 103, z: -131.4 } }, null],
  ['node above: jump', { node: { x: 167.5, y: 105, z: -131.5 } }, { dx: -0.99, dz: 1.09, jump: true }],
  ['pressed against something: jump', { collided: true }, { dx: -0.99, dz: 1.09, jump: true }]
]) {
  test(`idleNudge: ${name}`, () => {
    const got = idleNudge({ ...NUDGE, ...change })
    assert.deepEqual(got && { dx: Math.round(got.dx * 100) / 100, dz: Math.round(got.dz * 100) / 100, jump: got.jump }, expected)
  })
}

// Aviendha's bedroom: 1x2, all bed, ceiling 2 above the floor. You wake ON the bed with 1.44 of headroom and no walk can ever start
for (const [name, on, above, expected] of [
  ['on a bed under a ceiling', 'white_bed', 'block', /ON a bed under a low ceiling.*dig the bed/],
  ['on a bed with room above', 'red_bed', 'empty', null],
  ['on a bed, above not loaded', 'red_bed', undefined, null],
  ['on a slab under a ceiling', 'oak_slab', 'block', null],
  ['on nothing', undefined, 'block', null]
]) {
  test(`bedTrap: ${name}`, () => {
    const got = bedTrap(on, above)
    assert.equal(expected ? expected.test(got) : got, expected ? true : null)
  })
}

// sleep took the nearest bed, whoever's: Claude slept in Aviendha's at nightfall and her own sleep failed
const BED_ZONES = [{ name: 'aviendha-base', x1: 155, y1: 99, z1: -145, x2: 163, y2: 104, z2: -137 }, { name: 'claude-hut', x1: 113, y1: 66, z1: -146, x2: 121, y2: 76, z2: -136 }, { name: 'starter-stall', x1: 110, y1: 67, z1: -139, x2: 112, y2: 72, z2: -135 }]
const [HERS, MINE, OPEN, STALL] = [{ x: 160, y: 102, z: -140 }, { x: 116, y: 69, z: -141 }, { x: 140, y: 80, z: -120 }, { x: 111, y: 68, z: -137 }]
for (const [name, beds, me, any, expected] of [
  ['no bed', [], 'Claude', false, { error: 'no bed within 32 blocks' }],
  ['my own zone', [MINE], 'Claude', false, { bed: MINE }],
  ['a bed in the open', [OPEN], 'Claude', false, { bed: OPEN }],
  ['the shared stall is for everyone', [STALL], 'Claude', false, { bed: STALL }],
  ['hers is nearer, mine is further: mine', [HERS, MINE], 'Claude', false, { bed: MINE }],
  ['she sleeps in hers', [HERS], 'Aviendha', false, { bed: HERS }],
  ['invited: any=true takes the nearest', [HERS, MINE], 'Claude', true, { bed: HERS }]
]) {
  test(`bedChoice: ${name}`, () => assert.deepEqual(bedChoice(beds, BED_ZONES, me, any), expected))
}
test('bedChoice: an occupied bed is passed over for the next one', () => assert.deepEqual(bedChoice([OPEN, MINE], BED_ZONES, 'Claude', false, new Set(['140,80,-120'])), { bed: MINE }))
test('bedChoice: every bed I may use is taken', () => assert.match(bedChoice([OPEN], BED_ZONES, 'Claude', false, new Set(['140,80,-120'])).error, /occupied.*starter chest/))
// what the bedtime reflex tells the driver when going to bed by itself failed; null = not worth an event
for (const [name, error] of [
  ['the driver gave another order: no failure at all', 'cancelled: superseded by goto'],
  ['a second sleep took over', 'cancelled: superseded by sleep']
]) {
  test(`bedtimeReport: ${name}`, () => assert.equal(bedtimeReport(error), null))
}
for (const [name, error, expected] of [
  ['monsters: say what the server wants', 'there are monsters nearby', /monsters nearby.*within 8 blocks.*attack/],
  ['anything else is passed on as it is', 'no walkable path', /^no walkable path$/]
]) {
  test(`bedtimeReport: ${name}`, () => assert.match(bedtimeReport(error), expected))
}
test('bedChoice: only someone else\'s bed', () => assert.match(bedChoice([HERS], BED_ZONES, 'Claude', false).error, /aviendha-base.*one sleeper.*any=true/))

// three drownings came from walking under water after a block or a drop: both now need wet=true.
// A fluid in the cell itself is worse than wet: dig on water ran 167 seconds doing=dig before it was cancelled (#110)
for (const [name, target, above, allowWet, expected] of [
  ['dry', 'stone', ['air', 'air', 'air'], false, null],
  ['water beside is not asked about: a trench next to a pond is a farm job', 'dirt', ['air'], false, null],
  ['under water', 'stone', ['water', 'air', 'air'], false, /under water.*wet=true/],
  ['under deep water, two up', 'stone', ['sand', 'water', 'water'], false, /under water/],
  ['under water, but told to', 'stone', ['water', 'water', 'water'], true, null],
  ['the cell is water itself', 'water', ['air', 'air', 'air'], false, /water is a fluid, not a block.*never finishes.*fill x= y= z=.*place item=dirt/],
  ['water is a fluid however wet I am willing to get', 'water', ['water', 'water', 'water'], true, /water is a fluid, not a block/],
  ['lava is worse: it burns what it is dug with', 'lava', ['air', 'air', 'air'], false, /lava is a fluid, not a block.*burns/],
  ['a bubble column is water too', 'bubble_column', ['water', 'air', 'air'], false, /bubble_column is a fluid, not a block/],
  ['a waterlogged block is a real block: it digs', 'oak_slab', ['water', 'air', 'air'], true, null],
  ['nothing there at all', undefined, ['air', 'air', 'air'], false, null]
]) {
  test(`digRefusal: ${name}`, () => {
    const got = digRefusal(target, above, allowWet)
    assert.equal(expected ? expected.test(got) : got, expected ? true : null)
  })
}

// Chani watched cobblestone vanish twice over a mine.get (-4, then -5): the pathfinder towers and bridges with whatever
// placeable block it carries, and said nothing about it. Now it is reported like a drop, and taken back where it can be.
const SCAFFOLD = [{ x: 10, y: 70, z: 20, name: 'cobblestone' }, { x: 10, y: 71, z: 20, name: 'cobblestone' }, { x: 13, y: 70, z: 20, name: 'dirt' }]
for (const [name, feet, expected] of [
  ['the column I stand on is never dug out from under me', { x: 10.5, y: 72, z: 20.5 }, [{ x: 13, y: 70, z: 20, name: 'dirt' }]],
  ['standing beside it: both cells of the tower come back', { x: 12.5, y: 70, z: 20.5 }, SCAFFOLD.filter(c => c.x === 10).concat([{ x: 13, y: 70, z: 20, name: 'dirt' }])],
  ['too far below to reach', { x: 12.5, y: 40, z: 20.5 }, []],
  ['too far away to reach', { x: 40.5, y: 70, z: 20.5 }, []]
]) {
  test(`scaffoldTakeBack: ${name}`, () => assert.deepEqual(scaffoldTakeBack(SCAFFOLD, feet), expected))
}

// the pathfinder aims at the same cell several times a tick and its own place call rejects over blocks the server did put
// down, so what it built is read off the world afterwards, not off the clicks
const PLACED = { '10,70,20': 'cobblestone', '10,71,20': 'cobblestone', '11,70,20': 'air' }
for (const [name, tried, expected] of [
  ['nothing aimed at', [], []],
  ['a cell that now holds a block was built', [{ x: 10, y: 70, z: 20 }], [{ x: 10, y: 70, z: 20, name: 'cobblestone' }]],
  ['the same cell tried four times counts once', [{ x: 10, y: 70, z: 20 }, { x: 10, y: 70, z: 20 }, { x: 10, y: 70, z: 20 }, { x: 10, y: 70, z: 20 }], [{ x: 10, y: 70, z: 20, name: 'cobblestone' }]],
  ['a cell still air was never built', [{ x: 11, y: 70, z: 20 }], []],
  ['a cell outside the loaded world is not guessed at', [{ x: 99, y: 70, z: 20 }], []]
]) {
  test(`scaffoldBuilt: ${name}`, () => assert.deepEqual(scaffoldBuilt(tried, c => PLACED[`${c.x},${c.y},${c.z}`] ?? null), expected))
}

for (const [name, spent, taken, left, expected] of [
  ['nothing was built', {}, {}, [], null],
  ['all of it taken back', { cobblestone: 4 }, { cobblestone: 4 }, [], /cobblestone:4 went into the towers and bridges.*dug back cobblestone:4/],
  ['none taken back', { cobblestone: 4 }, {}, [{ x: 10, y: 70, z: 20 }], /cobblestone:4 .*dug none back.*1 still stands at 10,70,20: dig it when you pass/],
  ['two kinds, some left standing', { cobblestone: 4, dirt: 1 }, { cobblestone: 3 }, [{ x: 10, y: 70, z: 20 }, { x: 13, y: 70, z: 20 }], /cobblestone:4 dirt:1 .*dug back cobblestone:3.*2 still stand at 10,70,20 13,70,20/]
]) {
  test(`scaffoldNote: ${name}`, () => {
    const got = scaffoldNote(spent, taken, left)
    assert.equal(expected ? expected.test(got) : got, expected ? true : null)
  })
}

// clear digs a whole box: one water cell in it used to hang the sweep the same way, so fluids are skipped and named
for (const [name, counts, expected] of [
  ['nothing wet', {}, null],
  ['one water cell', { water: 1 }, /water x1 left.*a fluid cannot be dug.*fill x= y= z=.*place item=dirt/],
  ['both, most first', { water: 2, lava: 5 }, /lava x5, water x2 left/]
]) {
  test(`fluidsLeft: ${name}`, () => {
    const got = fluidsLeft(counts)
    assert.equal(expected ? expected.test(got) : got, expected ? true : null)
  })
}
const DROPS = [{ id: 1, dist: 2, deep: true }, { id: 2, dist: 5, deep: false }, { id: 3, dist: 9, deep: false }]
for (const [name, tried, allowWet, expected] of [
  ['nearest dry one', [], false, 2],
  ['told to swim: nearest', [], true, 1],
  ['one try each: a drop I could not reach is not walked at 40 times', [2], false, 3],
  ['nothing left', [2, 3], false, undefined]
]) {
  test(`nextDrop: ${name}`, () => assert.equal(nextDrop(DROPS, new Set(tried), allowWet)?.id, expected))
}
test('leftLying: drops in deep water are named', () => assert.deepEqual(leftLying(3, [], ['sand', 'sand']), { inWater: 'left in deep water: sand. Fetch it from a boat or the shore, or collect wet=true and watch your air' }))

// a NaN goal never ends the pathfinder's search: place with a typo in a coordinate ran until cancelled
for (const [name, a, expected] of [
  ['numbers', { x: 1, y: 64, z: -3.5 }, null],
  ['a word', { x: 'abc', y: 64, z: 2 }, /x, y and z must be numbers \(got x=abc y=64 z=2\)/],
  ['one missing', { x: 1, z: 2 }, /got x=1 y=undefined z=2/],
  ['a list where a number belongs', { x: [1, 2], y: 3, z: 4 }, /must be numbers/],
  ['y outside the world', { x: 1, y: 640, z: 2 }, /y=640 is outside the world \(-64 to 319\)/]
]) {
  test(`coordsError: ${name}`, () => {
    const got = coordsError(a)
    assert.equal(expected ? expected.test(got) : got, expected ? true : null)
  })
}

test('coordsError: an x/z goal has no y to complain about', () => assert.equal(coordsError({ x: -16, z: 'oops' }, false), 'x and z must be numbers (got x=-16 z=oops)'))
test('coordsError: a good x/z goal', () => assert.equal(coordsError({ x: -16, z: 4 }, false), null))

// a craft into a full inventory drops (or loses) what it made, after eating the ingredients
for (const [name, freeSlots, stacks, expected] of [
  ['a free slot', 1, [], null],
  ['full, but a stack of it has room', 0, [60, 64], null],
  ['full, its stacks are full too', 0, [64], /inventory is full.*toss or deposit/],
  ['full, none of it yet', 0, [], /inventory is full/],
  ['full, the stack has room for less than one batch', 0, [62], /inventory is full/]
]) {
  test(`craftRoom: ${name}`, () => {
    const got = craftRoom({ freeSlots, stacks, stackSize: 64, batch: 4, item: 'oak_planks' })
    assert.equal(expected ? expected.test(got) : got, expected ? true : null)
  })
}

// Miles, 96 paper with freeSlots=0: the one paper stack with room filled up at 33, the next craft threw 63 cane on the floor and the error blamed the ingredients
test('craftRoom: filled up part-way says how far it got', () => assert.equal(
  craftRoom({ freeSlots: 0, stacks: [64], stackSize: 64, batch: 3, item: 'paper', made: 33, count: 96 }),
  '33/96 made, then your inventory is full and paper has nowhere to go (the craft would eat the ingredients and drop or lose the result): toss or deposit something first'))
test('craftRoom: part-way with room goes on', () => assert.equal(craftRoom({ freeSlots: 0, stacks: [61], stackSize: 64, batch: 3, item: 'paper', made: 30, count: 96 }), null))

// smelt fuel=coal used to put the whole stack in the furnace: 21 coal for 5 sand
test('pickFuel: a named fuel is measured too', () => assert.deepEqual(pickFuel([{ name: 'coal', count: 21 }], 5), { name: 'coal', count: 1 }))
test('pickFuel: a fuel it has no figure for', () => assert.equal(pickFuel([{ name: 'blaze_rod', count: 3 }], 5), null))

// counting columns in a scan picture goes wrong (negative x runs backwards): where= names the cells instead
const SCAN_WORLD = (x, y, z) => y === 61 && x <= -20 ? 'sand' : y === 62 ? 'water' : 'dirt'
const SCAN_BOX = { x1: -22, y1: 61, z1: -88, x2: -19, y2: 62, z2: -88 }
for (const [name, where, limit, expected] of [
  ['exact name', 'sand', 20, 'sand 3: -22,61,-88 -21,61,-88 -20,61,-88'],
  ['wildcard: each hit says what it is', 's*', 20, 's* 3: sand@-22,61,-88 sand@-21,61,-88 sand@-20,61,-88'],
  ['more than the limit', 'water', 2, 'water 4: -22,62,-88 -21,62,-88 (+2 more)'],
  ['none', 'chest', 20, 'chest 0']
]) {
  test(`scanWhere: ${name}`, () => assert.equal(scanWhere(SCAN_WORLD, SCAN_BOX, where, limit), expected))
}

// Ganesha: "no idea what hit me": 18->8 with nearby=[] is a creeper that is gone because it blew up
for (const [name, state, expected] of [
  ['a big hit, nobody left, a creeper was just here', { lost: 10, nearby: [], sinceCreeperMs: 400 }, 'a creeper blew up (it is gone now)'],
  ['a big hit with nobody there and no creeper seen', { lost: 10, nearby: [], sinceCreeperMs: 60000 }, null],
  ['a zombie did it: nearby says so already', { lost: 3, nearby: ['zombie'], sinceCreeperMs: 400 }, null],
  ['a fall: the drop is the cause', { lost: 4, nearby: [], sinceCreeperMs: 60000, fell: 7 }, 'a fall of 7 blocks'],
  ['starving', { lost: 1, nearby: [], sinceCreeperMs: 60000, food: 0 }, 'starving: eat'],
  ['under water, out of air', { lost: 2, nearby: [], sinceCreeperMs: 60000, oxygen: 0 }, 'drowning: get to air']
]) {
  test(`hurtCause: ${name}`, () => assert.equal(hurtCause({ food: 15, oxygen: 20, fell: 0, ...state }), expected))
}

// stall shape C: a found path walked to its end, the goal not met, and the pathfinder just stands (no new search): 27 s lost per time
const DROPPED = { hasGoal: true, moving: false, digging: false, seconds: 4, pathAgeMs: 15000 }
for (const [name, change, expected] of [
  ['goal set, nothing moving, last search long ago', {}, true],
  ['still walking', { moving: true }, false],
  ['digging its way', { digging: true }, false],
  ['only just stopped', { seconds: 3 }, false],
  ['a search came in a moment ago: deadWalk or idleNudge look after that', { pathAgeMs: 800 }, false],
  ['no goal: nothing to drop', { hasGoal: false }, false],
  ['no search at all yet', { pathAgeMs: null }, false]
]) {
  test(`droppedWalk: ${name}`, () => assert.equal(droppedWalk({ ...DROPPED, ...change }), expected))
}

// "is the cow inside?" judged by eye went wrong again and again: pen.check counts, from the very cells it walked
const FLOOR = ['20,67,-118', '21,67,-118', '20,67,-117', '21,67,-117']
for (const [name, animals, expected] of [
  ['nobody about', [], {}],
  ['all in', [{ name: 'cow', x: 20.4, y: 67, z: -117.2 }, { name: 'cow', x: 21.9, y: 67, z: -117.9 }, { name: 'sheep', x: 20.5, y: 67, z: -116.5 }], { inside: 'cow:2 sheep:1' }],
  ['one stands beside the fence, outside', [{ name: 'cow', x: 20.4, y: 67, z: -117.2 }, { name: 'sheep', x: 20.5, y: 67, z: -120.5 }], { inside: 'cow:1', outside: 'sheep@20,67,-121' }],
  ['on the fence top counts as in the column it stands in', [{ name: 'cow', x: 20.5, y: 68.5, z: -117.5 }], { inside: 'cow:1' }]
]) {
  test(`penCensus: ${name}`, () => assert.deepEqual(penCensus(FLOOR, animals), expected))
}
// lead took the nearest cow, which was the one already in the pen it was leading to (Vivenna's, 09-19)
for (const [name, floor, animals, expected] of [
  ['an animal on the pen floor is left alone', FLOOR, [{ id: 1, x: 20.5, y: 67, z: -117.5 }, { id: 2, x: 40.5, y: 67, z: -117.5 }], [2]],
  ['no pen at the goal (null floor): all may come', null, [{ id: 1, x: 20.5, y: 67, z: -117.5 }], [1]],
  ['nothing outside', FLOOR, [{ id: 1, x: 20.5, y: 67, z: -117.5 }], []]
]) {
  test(`unpenned: ${name}`, () => assert.deepEqual(unpenned(floor, animals, a => a).map(a => a.id), expected))
}
// lead stopped one step inside the gate, the cow 2.5 blocks behind it: in the gateway, and the gate shut in its face
for (const [name, from, expected] of [
  ['the cell furthest from where the animals stand', { x: 20.5, z: -118.5 }, [21, 67, -117]],
  ['from the other side', { x: 21.5, z: -116.5 }, [20, 67, -118]]
]) {
  test(`deepestCell: ${name}`, () => assert.deepEqual(deepestCell(FLOOR, from), expected))
}
test('penLeak: hands out its floor when asked', () => {
  const rows = ['#####', '#..S#', '#####']
  assert.deepEqual(penLeak({ start: startOf(rows), topsAt: penMap(rows), radius: 4, withFloor: true }).floor.length, 3)
})

// Ganesha's body said "undefined" to the whole server (20:31Z, seen by Jizo and Arren): the text came under another name than message=
for (const [name, args, expected] of [
  ['plain', { message: 'hello' }, { text: 'hello' }],
  ['a number is text too', { message: 42 }, { text: '42' }],
  ['cut to what the server takes', { message: 'x'.repeat(300) }, { text: 'x'.repeat(250) }],
  ['under another name', { text: 'hello' }, { error: 'nothing said: the text goes in message= (you gave text=). Quote it: ./mc chat message="hello all"' }],
  ['nothing at all', {}, { error: 'nothing said: the text goes in message=. Quote it: ./mc chat message="hello all"' }],
  ['empty', { message: '  ' }, { error: 'nothing said: the text goes in message= (you gave message=). Quote it: ./mc chat message="hello all"' }]
]) {
  test(`chatText: ${name}`, () => assert.deepEqual(chatText(args, 250), expected))
}

// Ganesha led the same cow 6 times (20:30Z): with=0 each time and the cow never moved from 20,65,101. An animal jumps ONE block: in a pit two deep it cannot follow anyone
for (const [name, rises, expected] of [
  ['open ground', [0, 0, 1, 0], null],
  ['one step out is enough', [2, 3, 1, Infinity], null],
  ['a pit two deep', [2, 2, 3, 2], 'the cow at 20,65,101 stands in a pit (every way out is 2+ blocks up, it jumps 1): give it a step (place a block beside it, or dig the rim down), then lead again'],
  ['walled in', [Infinity, Infinity, Infinity, Infinity], 'the cow at 20,65,101 is walled in on all four sides: open a side (dig), then lead again']
]) {
  test(`pitAdvice: ${name}`, () => assert.equal(pitAdvice('cow', '20,65,101', rises), expected))
}

// pressed against a fence post my centre lies a few cm inside the FENCE's cell (116.07 beside my paddock gate): the pathfinder plans from that cell, on
// the far side of the fence, and the walk hops on the spot until the watchdog ends it (#66, reproduced 21:03Z). The cell I really stand in is the one my
// centre is nearest to; it must be free
for (const [name, position, free, expected] of [
  ['just inside its west edge: I stand west of it', { x: 116.07, y: 69, z: -130.5 }, () => true, { x: 115, y: 69, z: -131 }],
  ['just inside its east edge', { x: 116.93, y: 69, z: -130.5 }, () => true, { x: 117, y: 69, z: -131 }],
  ['north edge', { x: 116.5, y: 69, z: -130.95 }, () => true, { x: 116, y: 69, z: -132 }],
  ['south edge', { x: 116.5, y: 69, z: -130.05 }, () => true, { x: 116, y: 69, z: -130 }],
  ['the nearest side is blocked: the next nearest', { x: 116.07, y: 69, z: -130.2 }, (x, y, z) => x !== 115, { x: 116, y: 69, z: -130 }],
  ['nowhere free', { x: 116.07, y: 69, z: -130.5 }, () => false, null],
  // the inside corner of my paddock, 21:07Z: the cell is the corner post's, west and south of it are fence too, and I stand DIAGONALLY off it
  ['in a corner: the diagonal cell', { x: 116.07, y: 69, z: -131.07 }, (x, y, z) => x === 115 && z === -131, { x: 115, y: 69, z: -131 }],
  ['a free cell half a block away is not where I stand', { x: 116.07, y: 69, z: -131.07 }, (x, y, z) => x === 116 && z === -133, null]
]) {
  test(`realCell: ${name}`, () => assert.deepEqual(realCell(position, free), expected))
}

// the pathfinder hands its cost functions a block WITHOUT a position for an unloaded cell: my thicket cost threw 106 times in Kettricken's body (20:39-20:48Z)
test('besideNames: the four neighbours, by name', () => assert.deepEqual(besideNames({ x: 1, y: 64, z: 2 }, (x, y, z) => `${x},${y},${z}`), ['2,64,2', '0,64,2', '1,64,3', '1,64,1']))
test('besideNames: an unloaded cell has no position and no neighbours', () => assert.deepEqual(besideNames(undefined, () => 'bamboo'), []))
test('thicketCost: no neighbours known', () => assert.equal(thicketCost([]), 0))

// to the pathfinder cut bamboo is a full block to stand on; the real stalk is 3 pixels wide, so my body fell between the stalks into the plot (harvest, 20:47Z)
for (const [name, expected] of [['bamboo', true], ['bamboo_sapling', false], ['bamboo_planks', false], ['sugar_cane', false], ['stone', false]]) {
  test(`noFooting: ${name}`, () => assert.equal(noFooting(name), expected))
}

// my own body, wedged inside the planted bamboo plot at -13..-8,-112..-106, freed itself by digging 8 bamboo BASES: the stalk is gone for good unless it is planted again
for (const [name, dug, expected] of [
  ['a base (dirt below) is planted again', [{ name: 'bamboo', below: 'dirt', at: [1, 64, 2] }], [{ x: 1, y: 64, z: 2, item: 'bamboo' }]],
  ['an upper segment regrows by itself', [{ name: 'bamboo', below: 'bamboo', at: [1, 65, 2] }], []],
  ['leaves are never planted', [{ name: 'oak_leaves', below: 'air', at: [1, 66, 2] }], []],
  ['only the base of a column', [{ name: 'bamboo', below: 'grass_block', at: [1, 64, 2] }, { name: 'bamboo', below: 'bamboo', at: [1, 65, 2] }], [{ x: 1, y: 64, z: 2, item: 'bamboo' }]]
]) {
  test(`wedgeReplant: ${name}`, () => assert.deepEqual(wedgeReplant(dug), expected))
}

// bamboo's hitbox sits elsewhere on the server than in the client: a walk that brushes past a stalk gets position resets, so paths keep a cell away when they can
for (const [name, neighbours, expected] of [
  ['open ground', ['air', 'air', 'air', 'air'], 0],
  ['beside bamboo', ['air', 'bamboo', 'air', 'air'], 25],
  ['beside sugar cane (no hitbox)', ['sugar_cane', 'air', 'air', 'air'], 0],
  ['beside a sapling (no hitbox)', ['bamboo_sapling', 'air', 'air', 'air'], 0],
  ['unloaded', [undefined, undefined, undefined, undefined], 0]
]) {
  test(`thicketCost: ${name}`, () => assert.equal(thicketCost(neighbours), expected))
}

// the count shorthand (dirt cobblestone:3) is for maps of counts; on a result's own fields it printed "ok gatesShut shut fed:2 with"
test('terse: a result whose extra fields are all numbers keeps key=value', () => assert.equal(terse({ ok: true, gatesShut: 1, fed: 2, with: 1 }), 'ok gatesShut=1 fed=2 with=1'))
test('terse: a nested map of counts keeps the shorthand', () => assert.equal(terse({ ok: true, harvested: { wheat: 1, carrots: 3 } }), 'ok harvested(wheat carrots:3)'))

// Jizo wedged 3 times in 66 s in a bamboo grove: bamboo grows back and costs nothing, like the leaves the reflex already clears
for (const [name, expected] of [['oak_leaves', true], ['bamboo', true], ['cherry_leaves', true], ['bamboo_planks', false], ['oak_fence', false], ['stone', false]]) {
  test(`wedgeBreakable: ${name}`, () => assert.equal(wedgeBreakable(name), expected))
}

// Arren tossed bread at Ganesha and nobody got it: give said ok either way. Now it watches its own drop for 5 s
test('giveReport: picked up', () => assert.deepEqual(giveReport('Ganesha', []), { taken: 'yes' }))
// Vivenna tossed 6 bread at me across her pen fence: it fell at her own feet, she picked it up again, and the vanished drop read as taken=yes
test('giveReport: the drop vanished because it came back to me', () => assert.match(giveReport('Claude', [], 6).cameBack, /^6 came back to you.*fence.*same side/))
test('giveReport: nothing came back', () => assert.deepEqual(giveReport('Claude', [], 0), { taken: 'yes' }))
test('giveReport: still lying there', () => assert.match(giveReport('Ganesha', ['3,64,5']).lying, /^3,64,5: Ganesha has not picked it up.*take it back with collect/))

// Ganesha's smelt count=64 waited 700 s into the night and kept all eight others from sleeping: the furnace needs nobody watching it
for (const [name, state, expected] of [
  ['cooking by day', { got: 3, wanted: 8, night: false, timedOut: false }, 'wait'],
  ['all done', { got: 8, wanted: 8, night: false, timedOut: false }, 'done'],
  ['done at night is just done', { got: 8, wanted: 8, night: true, timedOut: false }, 'done'],
  ['took too long', { got: 3, wanted: 8, night: false, timedOut: true }, 'done'],
  ['night fell while it cooks: leave it', { got: 3, wanted: 8, night: true, timedOut: false }, 'night']
]) {
  test(`smeltWait: ${name}`, () => assert.equal(smeltWait(state), expected))
}

// lead to a pen whose gate stood open: "no pen here", so it led the cow that was IN the pen and the rest walked out (Vivenna's, 09-19)
const GATE_AT = { '-41,87,-219': { name: 'cherry_fence_gate', open: true }, '0,64,0': { name: 'oak_fence_gate', open: false } }
for (const [name, via, expected] of [
  ['the leak is an open gate: shut that first', '-41,87,-219', [-41, 87, -219]],
  ['a gap in the fence is not mine to fix', '5,64,5', null],
  ['a shut gate is no leak to fix', '0,64,0', null],
  ['a climb (three spots) is a fence problem', '-41,87,-219 -41,88.5,-220 -41,87,-221', null]
]) {
  test(`gateLeak: ${name}`, () => assert.deepEqual(gateLeak(via, (x, y, z) => GATE_AT[`${x},${y},${z}`]), expected))
}

// nine bodies rewrite clock.json every few seconds: a reader that caught it half-written crashed (`./mc clock`, and `./mc dawn` = an agent's only morning call)
for (const [name, text, expected] of [
  ['a whole clock', '{"day":true,"timeOfDay":500,"by":"Jizo","at":1}', { day: true, timeOfDay: 500, by: 'Jizo', at: 1 }],
  ['caught empty, mid-write', '', null],
  ['caught half-written', '{"day":tr', null]
]) {
  test(`parseClock: ${name}`, () => assert.deepEqual(parseClock(text), expected))
}

// feeding left the wheat in Vivenna's hand; her next walk went out through the pen gate and a cow followed her out, two mornings running
for (const [name, state, expected] of [
  ['wheat in hand at a gate', { held: 'wheat', luring: false, feeding: false, gateNear: true }, true],
  ['carrot too (pigs)', { held: 'carrot', luring: false, feeding: false, gateNear: true }, true],
  ['luring (a lead, from its first step towards the animal): the food is the point', { held: 'wheat', luring: true, feeding: false, gateNear: true }, false],
  ['feeding them right now', { held: 'wheat', luring: false, feeding: true, gateNear: true }, false],
  ['no gate around: carry what you like', { held: 'wheat', luring: false, feeding: false, gateNear: false }, false],
  ['a sword tempts nobody', { held: 'iron_sword', luring: false, feeding: false, gateNear: true }, false],
  ['empty hand', { held: undefined, luring: false, feeding: false, gateNear: true }, false]
]) {
  test(`foodAway: ${name}`, () => assert.equal(foodAway(state), expected))
}

// a helper wired into bot.mjs without its import only blows up when that line runs: 214 "foodAway is not defined" in 20 s on a live body
const unimported = (source, lib) => {
  const imported = new Set(source.match(/import \{([^}]*)\} from '[^']*lib\.mjs'/)[1].split(',').map(n => n.trim()))
  const body = source.replace(/import \{[^}]*\} from '[^']*lib\.mjs'/, '')
  return [...lib.matchAll(/export (?:const|function|async function) (\w+)/g)].map(m => m[1]).filter(n => !imported.has(n) && new RegExp(`\\b${n}\\(`).test(body))
}
const LIB_SOURCE = fs.readFileSync(new URL('../src/lib.mjs', import.meta.url), 'utf8')
test('unimported: spots a lib helper that is called but not imported', () => assert.deepEqual(unimported("import { terse } from './lib.mjs'\nfoodAway({})", LIB_SOURCE), ['foodAway']))
for (const file of ['../src/bot.mjs', '../tools/mc.mjs']) {
  test(`${file} imports every lib helper it calls`, () => assert.deepEqual(unimported(fs.readFileSync(new URL(`./${file}`, import.meta.url), 'utf8'), LIB_SOURCE), []))
}

// the body ate below food 15, but health only comes back at food 18+: Ganesha sat at hp=8 food=15 for 35 minutes, unable to heal
for (const [name, health, expected] of [['unhurt: do not waste food', 20, 15], ['hurt: eat up to where health comes back', 19, 18], ['badly hurt', 8, 18]]) {
  test(`eatBelow: ${name}`, () => assert.equal(eatBelow(health), expected))
}
// the guide promised `item=` beside `blocks=` as the default; the code refused (Kettricken, Miles)
test('withDefaultItem: entries without an item take the default, the others keep theirs', () =>
  assert.deepEqual(withDefaultItem([{ x: 1, y: 2, z: 3 }, { x: 1, y: 3, z: 3, item: 'torch' }], 'dirt'), [{ x: 1, y: 2, z: 3, item: 'dirt' }, { x: 1, y: 3, z: 3, item: 'torch' }]))
test('withDefaultItem: no default given leaves them unnamed', () => assert.deepEqual(withDefaultItem([{ x: 1, y: 2, z: 3 }], undefined), [{ x: 1, y: 2, z: 3 }]))

// "unknown action find" left me guessing: name the actions it could have been
const ACTIONS = ['find_blocks', 'goto', 'pen.check', 'collect', 'furnace_take', 'look_around']
for (const [name, typed, expected] of [
  ['a word of the real name', 'find', 'unknown action find: did you mean find_blocks?'],
  ['several candidates', 'look', 'unknown action look: did you mean look_around?'],
  ['dash for the dot', 'pen-check', 'unknown action pen-check: did you mean pen.check?'],
  ['nothing like it', 'xyzzy', "unknown action xyzzy: ./mc help lists them all"],
  // I typed cancel with a lead walking off with someone's chicken
  ['another word for it', 'cancel', 'unknown action cancel: did you mean stop?'],
  ['nearby', 'nearby', 'unknown action nearby: did you mean look_around?']
]) {
  test(`didYouMean: ${name}`, () => assert.equal(didYouMean(typed, [...ACTIONS, 'stop']), expected))
}
// an action that moved to a new name: the error says where it went, so a journal from last week still leads somewhere
for (const [was, now] of Object.entries(RENAMED)) {
  test(`didYouMean: ${was} is now ${now}`, () =>
    assert.equal(didYouMean(was, [...ACTIONS, now]), `unknown action ${was}: it is now ${now} (./mc help ${now.split('.')[0]})`))
}
test('RENAMED: no old name survives as a real one, and none points at itself', () => {
  const stale = Object.entries(RENAMED).filter(([was, now]) => was === now || PRIMITIVES[was])
  assert.deepEqual(stale, [])
})
test('renamedList: one line, every pair, for the foot of the catalogue', () =>
  assert.match(renamedList(), /^renamed: harvest -> farm\.harvest, .*pen_check -> pen\.check$/))

// a picture of 1500 cells is already a lot to read; where= answers in one line, so it may look much further
for (const [name, where, expected] of [['a map', undefined, 1500], ['where= only lists coordinates', 'potatoes', 60000]]) {
  test(`scanCap: ${name}`, () => assert.equal(scanCap(where), expected))
}

// Miles's cottage: the walk ended one step inside the door, "near" stayed true for ever, the door stood open and a zombie walked in
const DOOR = { near: false, open: true, mine: true, leading: false, moving: false, inDoorway: false }
for (const [name, state, expected] of [
  ['walked on past it', DOOR, true],
  ['stopped right behind it', { ...DOOR, near: true }, true],
  ['still walking through', { ...DOOR, near: true, moving: true }, false],
  ['standing in the doorway', { ...DOOR, near: true, inDoorway: true }, false],
  ['animals are following me', { ...DOOR, leading: true }, false],
  ['not one I opened', { ...DOOR, mine: false }, false],
  ['shut already', { ...DOOR, open: false }, false]
]) {
  test(`shutNow: ${name}`, () => assert.equal(shutNow(state), expected))
}

// Ganesha: "got teleported way out ... hurt 18->8, nearby=[]". The body had fled by itself; nothing told the driver so
test('hurtCause: hurt after my own flee run says so', () => assert.match(hurtCause({ lost: 2, nearby: [], sinceCreeperMs: 60000, fell: 0, food: 15, oxygen: 20, fledFrom: 'zombie', sinceFledMs: 8000 }), /fled from a zombie by itself.*moved/))
test('hurtCause: a flee run long ago explains nothing', () => assert.equal(hurtCause({ lost: 2, nearby: [], sinceCreeperMs: 60000, fell: 0, food: 15, oxygen: 20, fledFrom: 'zombie', sinceFledMs: 90000 }), null))
test('hurtCause: the creeper blast still comes first', () => assert.match(hurtCause({ lost: 10, nearby: [], sinceCreeperMs: 1000, fell: 0, food: 15, oxygen: 20, fledFrom: 'creeper', sinceFledMs: 2000 }), /creeper blew up/))

// Kettricken: "every time I stepped through my gate, an animal slipped out"; she found out at nightfall, from an empty pen. The walk that let it out can say so
const GATE = [20, 67, -119]
for (const [name, animals, expected] of [
  ['all inside', [{ name: 'cow', x: 20.5, y: 67, z: -117.5 }], null],
  ['one right outside the gate', [{ name: 'cow', x: 20.5, y: 67, z: -117.5 }, { name: 'sheep', x: 21.2, y: 67, z: -120.6 }], 'sheep@21,67,-121'],
  ['a wild one far off is none of my business', [{ name: 'cow', x: 40.5, y: 67, z: -140.5 }], null]
]) {
  test(`strays: ${name}`, () => assert.equal(strays(FLOOR, animals, GATE), expected))
}

// a body pressed against a closed gate floors into the gate's own cell: the pathfinder then plans from inside it and never opens it
for (const [name, state, expected] of [
  ['a wooden door ahead on a walk is opened', { near: true, open: false, door: true, moving: true, inDoorway: false }, true],
  ['a closed gate ahead is the pathfinder\'s to open', { near: true, open: false, door: false, moving: true, inDoorway: false }, false],
  ['a closed gate whose cell I stand in is opened', { near: true, open: false, door: false, moving: true, inDoorway: true }, true],
  ['standing still in a closed gate cell opens nothing', { near: true, open: false, door: false, moving: false, inDoorway: true }, false],
  ['an open door is left alone', { near: true, open: true, door: true, moving: true, inDoorway: false }, false],
  ['a far door is left alone', { near: false, open: false, door: true, moving: true, inDoorway: false }, false]
]) {
  test(`openNow: ${name}`, () => assert.equal(openNow(state), expected))
}

// farmland is watered by water within 4 blocks sideways, level with it or one above. Dry and unplanted it turns back within minutes (seen: 3 min)
for (const [name, tilled, waters, expected] of [
  ['water 4 away on the same level waters it', [[0, 64, 0]], [[4, 64, -4]], []],
  ['water one level up waters it', [[0, 64, 0]], [[1, 65, 0]], []],
  ['water 5 away does not', [[0, 64, 0]], [[5, 64, 0]], ['0,64,0']],
  ['water below does not', [[0, 64, 0]], [[1, 63, 0]], ['0,64,0']],
  ['no water at all: every cell is dry', [[0, 64, 0], [1, 64, 0]], [], ['0,64,0', '1,64,0']],
  ['only the far cells of a row are dry', [[0, 64, 0], [6, 64, 0]], [[1, 64, 0]], ['6,64,0']]
]) {
  test(`dryCells: ${name}`, () => assert.deepEqual(dryCells(tilled, waters), expected))
}

// who counts as attacking me: anything within 5 blocks, and archers as far as they shoot. Jizo's 3 pillagers at 10 blocks counted as one, so the body brawled to health 11
for (const [name, hostiles, expected] of [
  ['nobody', [], 0],
  ['a zombie at arm\'s length', [{ name: 'zombie', dist: 3 }], 1],
  ['a zombie across the field does not count', [{ name: 'zombie', dist: 10 }], 0],
  ['three pillagers at crossbow range all count', [{ name: 'pillager', dist: 9 }, { name: 'pillager', dist: 12 }, { name: 'pillager', dist: 20 }], 3],
  ['an archer out of range does not', [{ name: 'skeleton', dist: 30 }], 0],
  ['mixed', [{ name: 'zombie', dist: 2 }, { name: 'skeleton', dist: 15 }, { name: 'spider', dist: 8 }], 2]
]) {
  test(`crowdSize: ${name}`, () => assert.equal(crowdSize(hostiles), expected))
}

// the stall clock restarts only on real progress. A body hopping against a shut gate (the idle nudge jumps) rose 1.25 every sample and was never called stalled: Kalessin hung 300 s
for (const [name, from, here, expected] of [
  ['a step sideways is progress', { x: 0, y: 64, z: 0 }, { x: 0.3, y: 64, z: 0 }, true],
  ['standing still is not', { x: 0, y: 64, z: 0 }, { x: 0.05, y: 64, z: 0.05 }, false],
  ['the top of a hop on the spot is not', { x: 0, y: 64, z: 0 }, { x: 0, y: 65.25, z: 0 }, false],
  ['two blocks up the same column is (a pillar, a ladder)', { x: 0, y: 64, z: 0 }, { x: 0, y: 66, z: 0 }, true],
  ['falling down a shaft is', { x: 0, y: 64, z: 0 }, { x: 0, y: 60, z: 0 }, true]
]) {
  test(`progressed: ${name}`, () => assert.equal(progressed(from, here), expected))
}

// bot/mc without an agent's home fell back to port 3777 = Claude's body: an agent whose shell had drifted to bot/ marched it 300 blocks into her own bedroom
for (const [name, home, action, expected] of [
  ['an agent\'s own ./mc names its home', '/bot/agents/Arren', 'goto', false],
  ['no home: driving a body is refused', undefined, 'goto', true],
  ['no home: state is refused too (it would be somebody else\'s)', undefined, 'state', true],
  ['no home: wait is refused (it reads an agent\'s events)', undefined, 'wait', true],
  ['the clock needs no body', undefined, 'clock', false],
  ['dawn needs no body', undefined, 'dawn', false]
]) {
  test(`noHomeError: ${name}`, () => assert.equal(Boolean(noHomeError(home, action)), expected))
}
test('noHomeError: says where to go', () => assert.match(noHomeError(undefined, 'goto'), /agents\/<YourName>/))

// "all" of an item: tidying rubble into a chest asked for 64 of everything and was called a failure for carrying 20
test('withdrawPlan: "all" takes what there is and is never short', () =>
  assert.deepEqual(withdrawPlan({ andesite: 'all', granite: 'all', dirt: 4 }, { andesite: 20, dirt: 9 }), { take: [{ name: 'andesite', count: 20 }, { name: 'dirt', count: 4 }], short: [] }))

// seen at Dan's pen 09-19: adults {9: 10}, the calf {9: 10, 16: true}. Feeding a baby wastes the food and breeds nothing
for (const [name, metadata, expected] of [
  ['an adult', [0, 300, null, false, false, false, 'standing', 0, 0, 10], false],
  ['a calf', Object.assign([0, 300, null, false, false, false, 'standing', 0, 0, 10], { 16: true }), true],
  ['no metadata yet', undefined, false]
]) {
  test(`isBaby: ${name}`, () => assert.equal(isBaby(metadata), expected))
}

// an uncaught error was logged as its message only: 485 "reading 'y'" lines in Ganesha's log and no way to tell where from
test('stackTop: the first frames, short, without the message line', () =>
  assert.equal(stackTop("TypeError: Cannot read properties of undefined (reading 'y')\n    at feetCell (file:///home/dan/minecraft/claude/bot/lib.mjs:355:37)\n    at Timeout._onTimeout (file:///home/dan/minecraft/claude/bot/bot.mjs:501:9)\n    at listOnTimeout (node:internal/timers:581:17)\n    at process.processTimers (node:internal/timers:519:7)"),
    'feetCell lib.mjs:355:37 < Timeout._onTimeout bot.mjs:501:9 < listOnTimeout node:internal/timers:581:17'))
test('stackTop: no stack', () => assert.equal(stackTop(undefined), ''))

// mineflayer-pathfinder 2.4.5: after opening a gate (useOne) it takes the next thing to place; when there is none, "placing" stays true and the
// next tick reads placingBlock.y of undefined, every tick, for as long as I carry a block to scaffold with (485 errors in Ganesha's log, 423 in mine)
const GATE_BRANCH = "          lockUseBlock.release()\n          placingBlock = nextPoint.toPlace.shift()\n        }, err => {\n"
const GATE_FIXED = "          lockUseBlock.release()\n          placingBlock = nextPoint.toPlace.shift()\n          if (!placingBlock) placing = false // patched by bot/patch-deps.mjs\n        }, err => {\n"
for (const [name, source, expected] of [
  ['the upstream source gets the guard', `a\n${GATE_BRANCH}b`, { status: 'patched', source: `a\n${GATE_FIXED}b` }],
  ['patched already: left alone', `a\n${GATE_FIXED}b`, { status: 'already', source: `a\n${GATE_FIXED}b` }],
  ['a new upstream version without that code: say so, change nothing', 'something else', { status: 'anchor missing', source: 'something else' }]
]) {
  test(`patchPathfinder: ${name}`, () => assert.deepEqual(patchPathfinder(source), expected))
}

// Jizo, 09-19: bread in the inventory, food 7 -> 5 over several minutes, nothing eaten until the body was restarted. mineflayer-auto-eat sets
// _eating before it equips the food and only clears it after the meal: an equip that throws leaves it "eating" for ever. A meal takes 1.6 s
for (const [name, ms, expected] of [
  ['a meal in progress', 1600, false],
  ['a slow one (lag)', 9000, false],
  ['"eating" for 15 s: jammed', 15000, true]
]) {
  test(`eatJammed: ${name}`, () => assert.equal(eatJammed(ms), expected))
}

// SAFETY, 09-23. Every body's log carried the jam line and NOTHING else about eating: mineflayer-auto-eat's own reflex ends
// in `catch {}`, so 140 failures in a row said only that a meal had not finished. eat_failed now says what went wrong, and
// this turns the plugin's own wording (which names its internals, and appends the whole item object after a newline) into
// something a driver can act on without losing the real text.
for (const [name, error, expected] of [
  ['no food: the advice comes first, the real words after',
    new Error("No food specified and couldn't find a choice in inventory!"),
    "nothing I carry is food I am willing to eat: No food specified and couldn't find a choice in inventory!"],
  ['the equip that starves the body (Jizo, Chani)',
    new Error('Failed to equip: bread!\nItem: [object Object]'),
    'the food would not go into my hand: Failed to equip: bread!'],
  ['a meal the server never finished', new Error('Eating timed out with a time of 3000 milliseconds!'),
    'the server never said the meal finished: Eating timed out with a time of 3000 milliseconds!'],
  ['something took the food back out of my hand', new Error('Item switched early to: wheat!\nItem: undefined'),
    'my hand was emptied mid-meal (a reflex that re-equips?): Item switched early to: wheat!'],
  ['a second reflex on top of a running meal', new Error('Already eating!'), 'a meal was already running: Already eating!'],
  ['an error nobody has a word for is passed through whole', new Error('Cannot read properties of undefined (reading x)'),
    'Cannot read properties of undefined (reading x)'],
  ['a throw with no message at all still says something', new Error(''), 'the eat failed and said nothing'],
  ['not an Error at all', 'boom', 'boom']
]) {
  test(`eatFailure: ${name}`, () => assert.equal(eatFailure(error), expected))
}

// backlog #134: "nothing edible carried" told an agent nothing. A body holding rotten flesh and a body holding cobblestone
// got the same line, and neither knew whether to cook, to hunt, or to walk to a chest. So the refusal names what it passed
// over and why, and ./mc eat refuses before it touches the plugin, whose own wording names its internals.
for (const [name, carried, expected] of [
  ['empty pockets', [], { edible: [], banned: [], notFood: [] }],
  ['bread is food and not banned', ['bread'], { edible: ['bread'], banned: [], notFood: [] }],
  ['rotten flesh is food the game knows and this body refuses', ['rotten_flesh'], { edible: [], banned: ['rotten_flesh'], notFood: [] }],
  ['cobblestone is not food at all', ['cobblestone'], { edible: [], banned: [], notFood: ['cobblestone'] }],
  ['a full pocket sorts three ways', ['bread', 'rotten_flesh', 'stick', 'carrot'], { edible: ['bread', 'carrot'], banned: ['rotten_flesh'], notFood: ['stick'] }],
  ['a stack in two slots is named once', ['bread', 'bread'], { edible: ['bread'], banned: [], notFood: [] }]
]) {
  test(`foodSort: ${name}`, () => assert.deepEqual(foodSort(carried, n => ['bread', 'carrot', 'rotten_flesh'].includes(n), ['rotten_flesh']), expected))
}

const sorted = (edible = [], banned = [], notFood = []) => ({ edible, banned, notFood })
for (const [name, args, expected] of [
  ['the plain case: hungry, bread in my pockets', { food: 12, carried: sorted(['bread', 'carrot']) }, null],
  ['empty pockets say so', { food: 12, carried: sorted() }, 'nothing I carry is food: my pockets are empty'],
  ['rotten flesh is carried and never eaten', { food: 12, carried: sorted([], ['rotten_flesh']) },
    'nothing I carry is food (never eaten: rotten_flesh)'],
  ['a pocket of building blocks', { food: 12, carried: sorted([], [], ['cobblestone', 'stick']) },
    'nothing I carry is food (not food: cobblestone, stick)'],
  ['both reasons, both said', { food: 12, carried: sorted([], ['rotten_flesh'], ['cobblestone']) },
    'nothing I carry is food (never eaten: rotten_flesh; not food: cobblestone)'],
  ['a full belly cannot be fed', { food: 20, carried: sorted(['bread']) }, 'food is already 20: the game refuses a meal at a full belly'],
  ['19 is not 20', { food: 19, carried: sorted(['bread']) }, null],
  ['an item I do not carry', { food: 12, item: 'cake', carried: sorted(['bread']) },
    'no cake I would eat: I do not carry it; what I carry is bread'],
  ['the item I named is on the never-eat list', { food: 12, item: 'rotten_flesh', carried: sorted(['bread'], ['rotten_flesh']) },
    'no rotten_flesh I would eat: it is on the never-eat list; what I carry is bread'],
  ['the item I named is not food', { food: 12, item: 'wheat', carried: sorted(['bread'], [], ['wheat']) },
    'no wheat I would eat: it is not food; what I carry is bread'],
  ['an item named with nothing edible at all', { food: 12, item: 'cake', carried: sorted() },
    'no cake I would eat: I do not carry it; I carry nothing edible'],
  ['the named item beats the full belly: say the more useful thing first', { food: 20, item: 'cake', carried: sorted(['bread']) },
    'no cake I would eat: I do not carry it; what I carry is bread'],
  ['the item I asked for, and I have it', { food: 12, item: 'bread', carried: sorted(['bread']) }, null],
  ['the item I asked for, on a full belly', { food: 20, item: 'bread', carried: sorted(['bread']) }, 'food is already 20: the game refuses a meal at a full belly']
]) {
  test(`eatRefusal: ${name}`, () => assert.equal(eatRefusal(args), expected))
}

// The reflex is a physicsTick handler: 20 tries a second. While every meal timed out (see the eat_failed root cause) that
// meant 20 failed eats a second, each one a pair of window clicks at the server and a line in the log. After a failure it
// waits; a meal that works needs no cooldown, because a fed body stops asking.
for (const [name, failedAt, now, expected] of [
  ['nothing has failed yet', null, 1000, true],
  ['just failed: not yet', 1000, 1200, false],
  ['a second later: still not', 1000, 4999, false],
  ['five seconds on: try again', 1000, 6000, true]
]) {
  test(`eatRetryDue: ${name}`, () => assert.equal(eatRetryDue(failedAt, now), expected))
}

// A meal is 1.6 s of holding the food still, and anything that swaps the hand in that window cancels it where the
// server counts: on ClaudeProbe an `equip wheat` 0.9 s into a meal left the bread uneaten and food where it was. The
// pathfinder swaps the hand before every dig and works every gate by hand, so a body stuck against a gate can never
// finish a meal -- Chani's, at health 7 with five carrots in its pockets. These calls wait for the meal instead.
test('afterTheMeal: the call waits for the meal in the air', async () => {
  const order = []
  let finish
  const meal = new Promise(resolve => { finish = resolve })
  const wrapped = afterTheMeal(() => meal, (...args) => { order.push(['equipped', ...args]); return 'held' })
  const call = wrapped('pickaxe')
  await new Promise(resolve => setImmediate(resolve))
  order.push(['still chewing'])
  finish()
  assert.equal(await call, 'held')
  assert.deepEqual(order, [['still chewing'], ['equipped', 'pickaxe']])
})

// a meal that fails is still over, and the walk must not be stuck behind it
test('afterTheMeal: a meal that fails still lets the call through', async () => {
  const wrapped = afterTheMeal(() => Promise.reject(new Error('the meal never showed')), () => 'held')
  assert.equal(await wrapped(), 'held')
})

test('afterTheMeal: nothing in the air, nothing to wait for', async () => {
  const wrapped = afterTheMeal(() => null, () => 'held')
  assert.equal(await wrapped(), 'held')
})

// Item 12. After the 09-22 20:53 server restart, Perrin's and Mariel's bodies wrote `uncaught: Cannot read properties of
// undefined (reading 'isEating')` into their events files every few seconds, for hours: the auto-eat jam timer is started
// at every spawn and never stopped, and it reads the module-level `bot`, which a reconnect has already replaced with one
// whose plugins are not loaded yet. The guard and the stopped timer are the fix; this is the backstop, because ANY error
// that repeats turns the file the next person has to read into a wall. The first one is said, the repeats are counted,
// and the count is said once the message changes or a minute has gone by.
const replayErrors = (events, window) => events.reduce((acc, [message, at]) => {
  const { say, seen } = errorRepeat(acc.seen, message, at, window)
  return { seen, said: [...acc.said, ...(say ? [say] : [])] }
}, { seen: null, said: [] }).said

for (const [name, events, expected] of [
  ['one error is said as it is', [['boom', 0]], ['boom']],
  ['the same error again is counted, not said', [['boom', 0], ['boom', 5000], ['boom', 10000]], ['boom']],
  ['a different error is said at once', [['boom', 0], ['other', 5000]], ['boom', 'other']],
  ['and the first one comes back said, because the state moved on', [['boom', 0], ['other', 1000], ['boom', 2000]], ['boom', 'other', 'boom']],
  ['a stream of the same error says its count after the window', [['boom', 0], ['boom', 10000], ['boom', 20000], ['boom', 60000]], ['boom', 'boom (3 more in the last 60s)']],
  ['and the count starts again after that', [['boom', 0], ['boom', 60000], ['boom', 70000], ['boom', 120000]], ['boom', 'boom (1 more in the last 60s)', 'boom (2 more in the last 60s)']],
  ['a rare error is never folded up', [['boom', 0], ['boom', 90000], ['boom', 180000]], ['boom', 'boom (1 more in the last 90s)', 'boom (1 more in the last 90s)']],
  ["the isEating stream itself: every 5 s for an hour is 13 lines, not 720", Array.from({ length: 720 }, (_, i) => ["uncaught: Cannot read properties of undefined (reading 'isEating')", i * 5000]).slice(0, 25), ["uncaught: Cannot read properties of undefined (reading 'isEating')", "uncaught: Cannot read properties of undefined (reading 'isEating') (12 more in the last 60s)", "uncaught: Cannot read properties of undefined (reading 'isEating') (12 more in the last 60s)"]]
]) {
  test(`errorRepeat: ${name}`, () => assert.deepEqual(replayErrors(events), expected))
}

// Item 13 (#109). Claude's body died unattended on 09-22 and the events file has only a jump to the world spawn: no line
// saying where it fell, so nobody could go and fetch the iron kit, and nothing about what did it. Three parts. First:
// the server says exactly what happened, in a system message addressed to nobody, and reading it beats guessing.
for (const [name, text, expected] of [
  ['slain', 'Claude was slain by Zombie', 'slain by Zombie'],
  ['shot', 'Claude was shot by Skeleton', 'shot by Skeleton'],
  ['blown up', 'Claude was blown up by Creeper', 'blown up by Creeper'],
  ['a fall', 'Claude fell from a high place', 'fell from a high place'],
  ['drowning', 'Claude drowned', 'drowned'],
  ['fire', 'Claude burned to death', 'burned to death'],
  ['lava', 'Claude tried to swim in lava', 'tried to swim in lava'],
  ['starving', 'Claude starved to death', 'starved to death'],
  ['a wall', 'Claude suffocated in a wall', 'suffocated in a wall'],
  ['a named weapon', 'Claude was slain by Perrin using Bee Stinger', 'slain by Perrin using Bee Stinger'],
  ['somebody else dying is not my death', 'Chani was slain by Zombie', null],
  ['a player saying it in chat is not a death', '<Chani> Claude was slain by Zombie', null],
  ['my own name inside a sentence is not a death', 'Perrin whispers: Claude was slain by Zombie', null],
  ['a message that is only my name', 'Claude', null],
  ['a join message', 'Claude joined the game', null],
  ['an ordinary system line', 'Set the time to 1000', null]
]) {
  test(`deathBy: ${name}`, () => assert.equal(deathBy(text, 'Claude'), expected))
}

// Second: what the line says. Where the body STOOD, not where it respawns (the world spawn tells nobody anything), and
// what killed it. The server's own words win; a recent wound is the fallback; a wound from a minute ago is not evidence
// of anything, because a body that stood unhurt for a minute and then died did not drown a minute ago.
const wound = (cause, nearby, at) => ({ cause, nearby, at })
for (const [name, arg, expected] of [
  ['the server said it', { pos: { x: 1, y: 2, z: 3 }, said: 'slain by Zombie', wound: wound('a fall of 9 blocks', [], 0), now: 0 }, { pos: { x: 1, y: 2, z: 3 }, cause: 'slain by Zombie' }],
  ['no word from the server, but a fresh wound with a cause', { pos: { x: 1, y: 2, z: 3 }, wound: wound('a fall of 9 blocks', [], 0), now: 3000 }, { pos: { x: 1, y: 2, z: 3 }, cause: 'a fall of 9 blocks' }],
  ['a fresh wound with mobs round it and no cause', { pos: { x: 1, y: 2, z: 3 }, wound: wound(null, ['zombie'], 0), now: 1000 }, { pos: { x: 1, y: 2, z: 3 }, cause: 'a zombie was on me' }],
  ['two of them', { pos: { x: 1, y: 2, z: 3 }, wound: wound(null, ['zombie', 'skeleton'], 0), now: 1000 }, { pos: { x: 1, y: 2, z: 3 }, cause: 'a zombie and a skeleton were on me' }],
  ['three of them', { pos: { x: 1, y: 2, z: 3 }, wound: wound(null, ['zombie', 'skeleton', 'spider'], 0), now: 1000 }, { pos: { x: 1, y: 2, z: 3 }, cause: 'a zombie, a skeleton and a spider were on me' }],
  ['a wound too old to be the one', { pos: { x: 1, y: 2, z: 3 }, wound: wound('drowning: get to air', [], 0), now: 30000 }, { pos: { x: 1, y: 2, z: 3 } }],
  ['no wound at all', { pos: { x: 1, y: 2, z: 3 }, now: 1000 }, { pos: { x: 1, y: 2, z: 3 } }],
  ['the body was already gone: say so rather than write pos=null', { said: 'drowned', now: 0 }, { cause: 'drowned', where: 'unknown: I was already gone when the death arrived' }]
]) {
  test(`deathReport: ${name}`, () => assert.deepEqual(deathReport(arg), expected))
}

// Third: a death nobody announced. The `death` event is the usual source, but the respawn always arrives, so a respawn
// that no death preceded is a death that went unwritten - which is exactly what the 09-22 file looks like.
for (const [name, arg, expected] of [
  ['a respawn right after a death was written: nothing owed', { diedAt: 1000, now: 1200 }, false],
  ['a respawn with no death ever written', { diedAt: 0, now: 5000 }, true],
  ['a respawn long after the last death: a second death nobody wrote', { diedAt: 1000, now: 60000 }, true],
  ['right on the edge of the window, still the same death', { diedAt: 1000, now: 6000, window: 5000 }, false]
]) {
  test(`deathUnannounced: ${name}`, () => assert.equal(deathUnannounced(arg), expected))
}

// And what fell with me. "The iron kit was lost" is the line in #109 that costs an afternoon: the drops sit where the
// body fell for five minutes, so a died line that names them is the difference between a run back and a re-smelt. Tools
// and armour first because they are what hurts to lose; the rubble is counted, not listed.
for (const [name, items, expected] of [
  ['nothing carried', {}, null],
  ['the kit', { iron_pickaxe: 1, iron_sword: 1, cobblestone: 64, torch: 12 }, 'iron_pickaxe, iron_sword and 76 other blocks'],
  ['tools only', { iron_pickaxe: 1, wooden_hoe: 1 }, 'iron_pickaxe, wooden_hoe'],
  ['armour counts as kit', { iron_helmet: 1, leather_boots: 1, dirt: 32 }, 'iron_helmet, leather_boots and 32 other blocks'],
  ['rubble only', { dirt: 32, cobblestone: 64 }, '96 blocks'],
  ['more kit than fits on a line', { iron_pickaxe: 1, iron_sword: 1, iron_axe: 1, iron_shovel: 1, iron_helmet: 1, iron_chestplate: 1, bow: 1 }, 'iron_pickaxe, iron_sword, iron_axe, iron_shovel, iron_helmet, iron_chestplate and 1 more'],
  ['a stack of tools is still one line', { stone_pickaxe: 3 }, 'stone_pickaxe:3']
]) {
  test(`deathKit: ${name}`, () => assert.equal(deathKit(items), expected))
}

// Item 14 (Perrin, BUGS.md 09-23). `pen.check` on a pen 200 blocks away answered "not a spot to stand on", which blamed
// his coordinates for a chunk his body had never been sent: a block that comes back null is not air and not stone, it
// is nothing at all, and nothing at all may be guessed from. The role's own case, fetching from the shared stock to
// your own pen, starts with the body somewhere else entirely, so this is the ordinary case and not an odd one.
for (const [name, block, at, from, expected] of [
  ['a cell that is there to be read', { name: 'air' }, { x: 1, y: 2, z: 3 }, null, null],
  ['air is an answer, not an absence', { name: 'air', solid: false }, { x: 1, y: 2, z: 3 }, null, null],
  ['a chunk that was never sent', null, { x: 10, y: 64, z: 20 }, null,
    '10,64,20 is too far to see: that chunk is not loaded, so nothing there can be read. goto it first, then ask again'],
  ['and how far away it is, when that is known', null, { x: 110, y: 64, z: 20 }, { x: 10, y: 64, z: 20 },
    '110,64,20 is too far to see: it is 100 blocks off and that chunk is not loaded, so nothing there can be read. goto it first, then ask again']
]) {
  test(`outOfSight: ${name}`, () => assert.equal(outOfSight(block, at, from), expected))
}

// Aviendha, 09-19: goto dig=true tunnelled 50 blocks down into an underground lake and lost 9 hp drowning in its own shaft. To the pathfinder
// a water cell costs 1, the same as a step: a digging walk now pays dearly for each and never drops into water from a height
for (const [name, dig, expected] of [
  ['a digging walk keeps out of water', true, { liquidCost: 40, infiniteLiquidDropdownDistance: false }],
  ['a plain walk swims rivers as before', false, { liquidCost: 1, infiniteLiquidDropdownDistance: true }]
]) {
  test(`waterWary: ${name}`, () => assert.deepEqual(waterWary(dig), expected))
}

// my mine took Dan's pen for a shortcut: in by one gate, out by the other, and a cow went with me. A gate cell now costs a walk 30 steps:
// still the way in and out of a pen (there is no other), never a shortcut across one
for (const [name, block, expected] of [
  ['a fence gate', 'oak_fence_gate', 8],
  ['any wood', 'cherry_fence_gate', 8],
  ['a door is a house, not a pen', 'oak_door', 0],
  ['grass', 'short_grass', 0],
  ['unloaded', undefined, 0]
]) {
  test(`gateStepCost: ${name}`, () => assert.equal(gateStepCost(block), expected))
}

// Aviendha, every morning: the server wakes her ON the bed (0.56 high), and the only free cell beside it lies under a block 2 above the floor:
// 1.44 of headroom, nobody fits, every walk stalls. exits = the cells around the bed: free (feet and head empty), lintel (solid 2 above the floor)
for (const [name, exits, expected] of [
  ['one open cell beside the bed', [{ at: '1,64,0', free: true, lintel: false }, { at: '0,64,1', free: false, lintel: false }], null],
  ['the only free cell is under a lintel', [{ at: '160,102,-139', free: true, lintel: true }, { at: '161,102,-140', free: false, lintel: true }],
    'this bed is a trap: you wake up standing ON it (0.56 high) and the only free cell beside it (160,102,-139) has a block 2 above its floor, which leaves 1.44: nobody fits and no walk will start. Dig the block at 160,104,-139 (or move the bed next to a cell with 3 of headroom). In the morning, if stuck: dig the bed, walk out, place it back'],
  ['walled in all round', [{ at: '1,64,0', free: false, lintel: false }],
    'this bed has no free cell beside it: you will wake up standing ON it with nowhere to step. Clear one cell next to it (feet and head, and the block above those)'],
  ['nothing known', [], null]
]) {
  test(`bedExit: ${name}`, () => assert.equal(bedExit(exits), expected))
}

// Kettricken was told at 19:06Z, played on, and never heard of the five batches after it: "once per set of files" and the set was always bot.mjs lib.mjs
for (const [name, a, b, same] of [
  ['the same edit is told once', [['bot.mjs'], { 'bot.mjs': 5 }], [['bot.mjs'], { 'bot.mjs': 5 }], true],
  ['a later edit of the same file is news again', [['bot.mjs'], { 'bot.mjs': 5 }], [['bot.mjs'], { 'bot.mjs': 9 }], false],
  ['another file joins', [['bot.mjs'], { 'bot.mjs': 5, 'lib.mjs': 1 }], [['bot.mjs', 'lib.mjs'], { 'bot.mjs': 5, 'lib.mjs': 7 }], false]
]) {
  test(`staleKey: ${name}`, () => assert.equal(staleKey(...a) === staleKey(...b), same))
}

// "destination full" are mineflayer's words for both directions: on a withdraw the destination is MY pockets, and I read it as the chest
for (const [name, message, way, expected] of [
  ['withdraw into full pockets', 'destination full', 'withdraw', 'YOUR INVENTORY is full: what fitted was taken (the + above). deposit or toss something, then withdraw the rest'],
  ['the other wording', 'Unable to withdraw, Bot inventory is full.', 'withdraw', 'YOUR INVENTORY is full: what fitted was taken (the + above). deposit or toss something, then withdraw the rest'],
  ['deposit into a full chest', 'destination full', 'deposit', 'the CHEST is full: what fitted went in (the - above). Put the rest in another chest, or take out what does not belong here'],
  ['anything else is left alone', 'no container found nearby', 'deposit', 'no container found nearby']
]) {
  test(`fullSide: ${name}`, () => assert.equal(fullSide(message, way), expected))
}

// my mine, started inside Dan's starter pen, sank a 3-deep shaft in the pen floor and left it open: a calf trap. inside = the census of the pen I stand in
for (const [name, inside, force, expected] of [
  ['animals live here', 'sheep:3 cow:4', undefined, 'you stand in a pen with animals in it (sheep:3 cow:4): mine digs its way down from where you stand and would leave a shaft for them to fall into. Walk out through the gate first, then mine (force=true if you really mean it, and fill the hole after)'],
  ['forced', 'cow:2', true, null],
  ['an empty pen, a yard, a cave pocket', undefined, undefined, null]
]) {
  test(`penShaftRefusal: ${name}`, () => assert.equal(penShaftRefusal(inside, force), expected))
}

// mine climbs back out of its shaft but leaves the top of it open: a pit at my own front door (and my cornflower gone), one in Dan's pen.
// before/after: the ground around the start, { 'x,y,z': block name }; carried: what I have to fill with
for (const [name, before, after, carried, expected] of [
  ['the shaft mouth, filled with what came out of it', { '1,67,1': 'grass_block', '2,67,1': 'dirt' }, { '1,67,1': 'air', '2,67,1': 'dirt' }, ['cobblestone', 'dirt'], [{ x: 1, y: 67, z: 1, item: 'dirt' }]],
  ['cobble will do', { '1,67,1': 'stone' }, { '1,67,1': 'air' }, ['cobblestone'], [{ x: 1, y: 67, z: 1, item: 'cobblestone' }]],
  ['it was air before: not my hole', { '1,67,1': 'air' }, { '1,67,1': 'air' }, ['dirt'], []],
  ['flowers and grass that went are not holes', { '1,68,1': 'short_grass' }, { '1,68,1': 'air' }, ['dirt'], []],
  ['nothing to fill with', { '1,67,1': 'dirt' }, { '1,67,1': 'air' }, ['wheat'], []]
]) {
  test(`holesLeft: ${name}`, () => assert.deepEqual(holesLeft(before, after, carried, n => !['air', 'short_grass'].includes(n)), expected))
}

// Ganesha led a cow 200 blocks home, and then the body walked all the way BACK to shut two gates it had held open since the start
// (a lead kept every gate open until its end) and reported from there: "arrived with=1" 170 blocks from the pen, no cow in sight
for (const [name, me, gate, animals, expected] of [
  ['the cow is still on the far side of the gate', [0, 0, 10], [0, 0, 5], [[0, 0, 2]], false],
  ['in the gateway', [0, 0, 10], [0, 0, 5], [[0, 0, 5.2]], false],
  ['through, at my heels', [0, 0, 10], [0, 0, 5], [[0, 0, 8]], true],
  ['one of two still to come', [0, 0, 10], [0, 0, 5], [[0, 0, 8], [1, 0, 3]], false],
  ['nobody follows any more: nothing to wait for', [0, 0, 10], [0, 0, 5], [], true]
]) {
  test(`herdPassed: ${name}`, () => assert.equal(herdPassed(me, gate, animals), expected))
}
for (const [name, gates, me, expected] of [
  ['the one behind me is mine to shut, the one a long walk back is reported', [[0, 64, 3], [0, 64, -150]], [0, 64, 0], { near: [[0, 64, 3]], far: '0,64,-150' }],
  ['none', [], [0, 64, 0], { near: [], far: '' }]
]) {
  test(`gatesByReach: ${name}`, () => assert.deepEqual(gatesByReach(gates, me), expected))
}

// Item 15, the other half: which of the animals standing round me come along, and what the reply says came. Perrin's
// `count=2` took the two nearest, which were calves, and answered `with=2`: true, useless, and no way to tell from the
// line that the pen now holds nothing that can breed.
for (const [name, herd, expected] of [
  ['grown ones first, nearest of them first', [{ id: 1, grown: false }, { id: 2, grown: true }, { id: 3, grown: true }], [2, 3, 1]],
  ['all grown: distance order is kept', [{ id: 1, grown: true }, { id: 2, grown: true }], [1, 2]],
  ['all calves: distance order is kept', [{ id: 1, grown: false }, { id: 2, grown: false }], [1, 2]],
  ['an animal whose age I could not read counts as grown', [{ id: 1 }, { id: 2, grown: false }], [1, 2]],
  ['nothing at all', [], []]
]) {
  test(`herdOrder: ${name}`, () => assert.deepEqual(herdOrder(herd).map(a => a.id), expected))
}

for (const [name, mob, came, expected] of [
  ['two grown cows', 'cow', [{ grown: true }, { grown: true }], 'cow:2'],
  ['one grown and two calves: the count alone would lie', 'cow', [{ grown: true }, { grown: false }, { grown: false }],
    'cow:3 (1 grown, 2 calves: a calf will not breed)'],
  ['nothing came', 'cow', [], 'cow:0'],
  ['one calf', 'sheep', [{ grown: false }], 'sheep:1 (0 grown, 1 calf: a calf will not breed)']
]) {
  test(`ledReport: ${name}`, () => assert.equal(ledReport(mob, came), expected))
}

// item 17 (Perrin): a lead is walked with the food in my hand, and food in the hand is visible to every animal of its
// kind that can see me - not only to the ones that were picked. A lead for two out of a big herd walks a queue in, and
// `with=2` was true and said nothing about the other four now standing in the pen eating the grass. Shedding them is
// not on offer: the food is what the walk is MADE of, and an animal that follows food cannot be told to stop. So they
// are counted. By id, because one sheep is not told from another by looks or by where it stands; the ones that were
// in the pen before I got there are not followers, and neither are the ones I asked for.
for (const [name, invited, before, now, expected] of [
  ['nobody followed', [1, 2], [], [1, 2], 0],
  ['three walked in behind the two I asked for', [1, 2], [], [1, 2, 7, 8, 9], 3],
  ['the ones already in the pen are not followers', [1, 2], [5, 6], [1, 2, 5, 6], 0],
  ['already-in and followers together', [1], [5], [1, 5, 9], 1],
  ['one I asked for never arrived, and two others did', [1, 2], [], [1, 8, 9], 2],
  ['an empty pen and an empty lead', [], [], [], 0]
]) {
  test(`tagalongs: ${name}`, () => assert.equal(tagalongs(invited, before, now), expected))
}

for (const [name, mob, extra, expected] of [
  ['none: nothing is said', 'sheep', 0, {}],
  ['one', 'sheep', 1, { extra: 1, extraNote: '1 more sheep followed the food in uninvited: it is in there too. Lead it out, or feed the pen for one more' }],
  ['several', 'cow', 3, { extra: 3, extraNote: '3 more cow followed the food in uninvited: they are in there too. Lead them out, or feed the pen for 3 more' }]
]) {
  test(`ledExtra: ${name}`, () => assert.deepEqual(ledExtra(mob, extra), expected))
}

// my own lead (within=64) walked to Aviendha's base for the nearest cow: an animal in SOMEBODY's pen is not free for the taking.
// candidates: nearest first, { id, at, penned }
for (const [name, candidates, allowPenned, expected] of [
  ['the nearest free one', [{ id: 1, at: '5,64,5', penned: true }, { id: 2, at: '9,64,9', penned: false }], false, { id: 2 }],
  ['asked for: the starter pen is there to take from', [{ id: 1, at: '5,64,5', penned: true }], true, { id: 1 }],
  ['only penned ones about', [{ id: 1, at: '5,64,5', penned: true }], false, { error: 'the only ones in range stand in a pen (nearest at 5,64,5): they are somebody\'s. penned=true takes one anyway: only from the starter pen or a pen of your own' }],
  ['none at all', [], false, { error: 'none in range' }],
  // item 15 (Perrin): `flock.lead count=2` out of a 24-cow herd delivered one adult and two calves, silently, and the
  // breed that followed did nothing. A calf is next year's herd, not this year's pair: the grown one is taken even
  // when a calf stands nearer, and a calf is only taken when there was no grown one to take.
  ['a calf standing nearer is passed over', [{ id: 1, at: '5,64,5', penned: false, grown: false }, { id: 2, at: '9,64,9', penned: false, grown: true }], false, { id: 2 }],
  ['nothing but calves: the nearest comes, and it is said out loud', [{ id: 1, at: '5,64,5', penned: false, grown: false }], false,
    { id: 1, note: 'the only cow in range is a calf (at 5,64,5): it will not breed, and it stays with its herd until it grows' }],
  ['a calf in a pen is still somebody\'s', [{ id: 1, at: '5,64,5', penned: true, grown: false }], false,
    { error: 'the only ones in range stand in a pen (nearest at 5,64,5): they are somebody\'s. penned=true takes one anyway: only from the starter pen or a pen of your own' }]
]) {
  test(`leadPick: ${name}`, () => assert.deepEqual(leadPick(candidates, allowPenned, 'cow'), expected))
}

// Dan: "make sure the harvest properly reseeds those canes". The cut is the second segment and the base regrows, but a base that is gone all the
// same after the cut (cut from under by someone else between the look and the dig, popped off by a water change) is planted again from the pockets.
// cut: [{ stalk, base: [x, y, z], baseNow }]
for (const [name, cut, carried, expected] of [
  ['the base stands: nothing to do', [{ stalk: 'sugar_cane', base: [1, 63, 1], baseNow: 'sugar_cane' }], ['sugar_cane'], { plant: [], lost: 0 }],
  ['a base gone: plant it again', [{ stalk: 'sugar_cane', base: [1, 63, 1], baseNow: 'air' }], ['sugar_cane'], { plant: [{ x: 1, y: 63, z: 1, item: 'sugar_cane' }], lost: 1 }],
  ['bamboo too', [{ stalk: 'bamboo', base: [2, 64, 2], baseNow: 'air' }], ['bamboo', 'dirt'], { plant: [{ x: 2, y: 64, z: 2, item: 'bamboo' }], lost: 1 }],
  ['gone and none in my pockets: counted, so the reply can say so', [{ stalk: 'sugar_cane', base: [1, 63, 1], baseNow: 'water' }], [], { plant: [], lost: 1 }]
]) {
  test(`stalkReplant: ${name}`, () => assert.deepEqual(stalkReplant(cut, carried), expected))
}

// my lead at the starter-pen gate (21:39Z) replanned 120 times in 12 s and stalled: the moment the pathfinder pressed a key the nudge ended, and I planned again
// while still in the fence's cell. Once pressed, keep walking to the middle of the real cell whatever the nudge says, and plan again only from there
const CELL = { x: 19, y: 67, z: -120 }
for (const [name, pressed, inFence, nudge, pos, expected] of [
  ['not pressed, no nudge: nothing to do', null, CELL, false, { x: 19.5, z: -119 }, { target: null, replan: false }],
  ['nudged inside a fence cell: start pushing', null, CELL, true, { x: 19.5, z: -119 }, { target: CELL, replan: false }],
  ['nudged in the open: the ordinary nudge', null, null, true, { x: 19.5, z: -119.5 }, { target: null, replan: false }],
  ['pressed, the nudge ended, still at the edge: keep pushing', CELL, CELL, false, { x: 19.5, z: -119.02 }, { target: CELL, replan: false }],
  ['pressed, out of the fence cell but still on its edge: keep pushing', CELL, null, false, { x: 19.5, z: -119.05 }, { target: CELL, replan: false }],
  ['pressed and in the middle of the real cell: plan again', CELL, null, false, { x: 19.6, z: -119.4 }, { target: null, replan: true }]
]) {
  test(`fencePush: ${name}`, () => assert.deepEqual(fencePush(pressed, inFence, nudge, pos), expected))
}

// the starter pen's south gate stood open and the herd was gone (21:33Z): no log could say who opened it. Every body near a gate notes each change in gates.log
const SOUTH_GATE = { x: 21, y: 66, z: -103 }
for (const [name, before, after, players, expected] of [
  ['a gate opens: the nearest player did it', { name: 'oak_fence_gate', open: false }, { name: 'oak_fence_gate', open: true }, [{ name: 'Miles', dist: 9.2 }, { name: 'mruwnik', dist: 1.6 }], { gate: '21,66,-103', now: 'open', nearest: 'mruwnik', dist: 2 }],
  ['a gate shuts', { name: 'oak_fence_gate', open: true }, { name: 'oak_fence_gate', open: false }, [{ name: 'Claude', dist: 1 }], { gate: '21,66,-103', now: 'shut', nearest: 'Claude', dist: 1 }],
  ['nobody in sight', { name: 'oak_fence_gate', open: false }, { name: 'oak_fence_gate', open: true }, [], { gate: '21,66,-103', now: 'open', nearest: null, dist: null }],
  ['the only player I can see is far off (my body saw Kettricken\'s gate move and blamed Miles, 26 away): nobody named', { name: 'oak_fence_gate', open: false }, { name: 'oak_fence_gate', open: true }, [{ name: 'Miles', dist: 26 }], { gate: '21,66,-103', now: 'open', nearest: null, dist: null }],
  ['a gate is placed: no change of state', { name: 'air' }, { name: 'oak_fence_gate', open: false }, [{ name: 'Claude', dist: 1 }], null],
  ['same state (it only turned)', { name: 'oak_fence_gate', open: true }, { name: 'oak_fence_gate', open: true }, [{ name: 'Claude', dist: 1 }], null],
  ['not a gate', { name: 'oak_door', open: false }, { name: 'oak_door', open: true }, [{ name: 'Claude', dist: 1 }], null]
]) {
  test(`gateChange: ${name}`, () => assert.deepEqual(gateChange(SOUTH_GATE, before, after, players), expected))
}

// Kettricken, 22:00Z: collect_items in a sealed cave pocket at 250,-24,-137 said "drops lie outside this pen". Rock all round is no pen: a pen has a fence or wall top beside its floor
for (const [name, rows, expected] of [
  ['a ring of fence', ['      ', ' #### ', ' #S.# ', ' #..# ', ' #### ', '      '], true],
  ['a fence on raised ground', ['     ', ' HHH ', ' HSH ', ' HHH ', '     '], true],
  ['a cave pocket: rock all round', ['     ', ' WWW ', ' WSW ', ' WWW ', '     '], false]
]) {
  test(`fencedIn: ${name}`, () => assert.equal(fencedIn(penLeak({ start: startOf(rows), topsAt: penMap(rows), radius: 2, withFloor: true }).floor, penMap(rows)), expected))
}

// Kettricken, 22:00Z: mine ended with gaveUp="There are no defined chest locations!": that is the collect plugin saying the inventory is full
for (const [message, expected] of [
  ['There are no defined chest locations!', 'your inventory is full: deposit or drop something, then mine again'],
  ['No path to the block', 'No path to the block']
]) {
  test(`mineFailure: ${message}`, () => assert.equal(mineFailure(message), expected))
}

// Vivenna's pen, second leak (09-19 night): her patch put fences ON the grass blocks of the north wall (H). pen.check said holds, and the cows were out within minutes:
// from the raised tile they walk LEVEL onto the rim of such a block (a post leaves room beside it), sidle along the wall, and the ordinary fence next along is half a step up.
// With the tiles dug out the herd stayed in all night: from the floor the rim is a full block up, and no cow jumps onto a ledge that narrow
const rimMap = rows => (x, z) => rows[z]?.[x] === 'H' ? [1] : []
for (const [name, rows, expected] of [
  ['a raised tile, a fence on a block beside it, a plain fence next along: out over the rim', ['       ', ' #HHH  ', ' #.BH  ', ' #S.H  ', ' ####  ', '       '], false],
  ['the same pen with a flat floor holds', ['       ', ' #HHH  ', ' #..H  ', ' #S.H  ', ' ####  ', '       '], true],
  ['fences on blocks all round: the rim leads nowhere', ['       ', ' HHHH  ', ' H.BH  ', ' HS.H  ', ' HHHH  ', '       '], true],
  // the starter pen on its slope read LEAKS (22:10Z): from a rim the search stepped up onto the hillside beyond the fence
  ['a hillside a block higher beyond the fence is not reached through it either', ['TTTTTTT', 'THHHHTT', 'TH.BHTT', 'THS.HTT', 'THHHHTT', 'TTTTTTT'], true],
  ['higher ground beyond the fence is not reached THROUGH it', ['BBBBBBB', 'BHHHHBB', 'BH.BHBB', 'BHS.HBB', 'BHHHHBB', 'BBBBBBB'], true]
]) {
  test(`penLeak rims: ${name}`, () => assert.equal(penLeak({ start: startOf(rows), topsAt: penMap(rows), rimsAt: rimMap(rows), radius: 2 }).enclosed, expected))
}
test('penLeak rims: a rim is no floor cell (cells= and the census count the pen, not its wall)', () => {
  const rows = ['       ', ' HHHH  ', ' H.BH  ', ' HS.H  ', ' HHHH  ', '       ']
  const found = penLeak({ start: startOf(rows), topsAt: penMap(rows), rimsAt: rimMap(rows), radius: 2, withFloor: true })
  assert.deepEqual([found.cells, found.floor.length], [4, 4])
})

// Kettricken's wish (22:12Z): an enchant action for the library table. The table offers three slots (level = xp levels needed; slot n also needs n+1 lapis and costs n+1 levels)
const OFFERS = [{ level: 2 }, { level: 9 }, { level: 30 }]
for (const [name, offers, xp, lapis, wanted, expected] of [
  ['the best slot I can pay for', OFFERS, 12, 3, undefined, { choice: 1 }],
  ['lapis limits it too', OFFERS, 40, 1, undefined, { choice: 0 }],
  ['everything affordable: the top slot', OFFERS, 30, 3, undefined, { choice: 2 }],
  ['a slot asked for by number (1-3)', OFFERS, 40, 3, 1, { choice: 0 }],
  ['asked for, not affordable', OFFERS, 12, 3, 3, { error: 'slot 3 needs xp level 30 and 3 lapis_lazuli: you have level 12 and 3 lapis. Offers: 1=level 2, 2=level 9, 3=level 30' }],
  ['no xp at all', OFFERS, 1, 3, undefined, { error: 'nothing affordable: you have xp level 1 and 3 lapis_lazuli. Offers: 1=level 2, 2=level 9, 3=level 30 (slot n also needs n lapis). Gain xp by mining ore, smelting, breeding or fighting' }],
  ['the table offers nothing for this item', [{ level: -1 }, { level: -1 }, { level: -1 }], 30, 3, undefined, { error: 'the table offers nothing for this item: it cannot be enchanted here (already enchanted, or not enchantable)' }]
]) {
  test(`enchantChoice: ${name}`, () => assert.deepEqual(enchantChoice(offers, xp, lapis, wanted), expected))
}

// my own `withdraw item=lapis_lazuli count=3` (22:17Z) died with "Cannot convert undefined or null": every other action says item=, so the chests take it too
for (const [name, a, expected] of [
  ['items= as ever', { items: { coal: 4 } }, { coal: 4 }],
  ['item= and count=', { item: 'lapis_lazuli', count: 3 }, [{ name: 'lapis_lazuli', count: 3 }]],
  ['item= alone: all of it', { item: 'wheat' }, [{ name: 'wheat', count: undefined }]],
  ['neither', { x: 1 }, undefined]
]) {
  test(`itemsArg: ${name}`, () => assert.deepEqual(itemsArg(a), expected))
}
test('withdrawPlan: nothing asked for is an error to read, not a crash', () => assert.throws(() => withdrawPlan(undefined, {}), /withdraw needs items=/))

// what an enchanted item carries, as the server sent it to my body (22:24Z): {"enchantments":[{"id":33,"level":1}]}; older items: [{name, lvl}]
const ENCHANT_IDS = { 33: 'sharpness' }
for (const [name, raw, expected] of [
  ['by id', { enchantments: [{ id: 33, level: 1 }] }, 'sharpness 1'],
  ['an id my registry does not know', { enchantments: [{ id: 99, level: 2 }] }, 'enchantment#99 2'],
  ['the older list', [{ name: 'efficiency', lvl: 3 }, { name: 'unbreaking', lvl: 1 }], 'efficiency 3, unbreaking 1'],
  ['nothing', undefined, undefined]
]) {
  test(`enchantNames: ${name}`, () => assert.equal(enchantNames(raw, id => ENCHANT_IDS[id]), expected))
}

// Ganesha's pen had its gate in the north-west CORNER for a day: every lead ended outside it, and pen.check said "holds" without a word. A gate is a way in only when
// floor lies on one side of it and open ground straight across on the other
const gateRows = rows => ({
  gates: rows.flatMap((r, z) => [...r].flatMap((c, x) => c === 'G' ? [{ x, z }] : [])),
  inFloor: (x, z) => '.S'.includes(rows[z]?.[x] ?? '#') && rows[z]?.[x] !== ' ',
  barrier: (x, z) => '#G'.includes(rows[z]?.[x] ?? ' ')
})
for (const [name, rows, expected] of [
  ['a gate in the middle of a wall', ['      ', ' #G## ', ' #..# ', ' #### '], []],
  ['a gate in the corner: fence on both far sides', ['      ', ' G### ', ' #..# ', ' #### '], ['1,0,1']],
  ['a mid-wall gate with a block right outside it', ['  #   ', ' #G## ', ' #..# ', ' #### '], ['2,0,1']]
]) {
  test(`blindGates: ${name}`, () => { const g = gateRows(rows); assert.deepEqual(blindGates(g.gates.map(p => ({ ...p, y: 0 })), g.inFloor, g.barrier), expected) })
}

// the starter-pen marker sat ON the fence (21,67,-119): lead place=starter-pen walked the sheep to the fence and "arrived" outside it (21:37Z)
for (const [name, block, expected] of [
  ['a fence cell', { name: 'oak_fence', solid: true }, 'flock.lead: 21,67,-119 is inside a oak_fence, not a spot to stand on: give a free floor cell INSIDE the pen (or mark the place again there)'],
  ['free air', { name: 'air', solid: false }, null],
  ['short grass', { name: 'short_grass', solid: false }, null],
  ['not loaded yet (far away): cannot tell, go', null, null]
]) {
  test(`leadTargetError: ${name}`, () => assert.equal(leadTargetError({ x: 21, y: 67, z: -119 }, block), expected))
}

// after the first enchants (22:45Z) Kettricken's harvest died with "enchantments.concat is not a function" and digs took 9 s: for this server prismarine-item hands out
// the raw component ({enchantments:[{id,level}]}) where everyone expects [{name,lvl}]
const RAW_ENCHANTS = "        return this.componentMap.get('enchantments').data\n"
test('patchItemEnchants: the raw component becomes a list', () => {
  const { status, source } = patchItemEnchants(`a\n${RAW_ENCHANTS}b\n`)
  assert.deepEqual([status, source.includes(RAW_ENCHANTS), source.includes('lvl: e.level ?? e.lvl')], ['patched', false, true])
})
test('patchItemEnchants: a second start finds it done', () => assert.equal(patchItemEnchants(patchItemEnchants(`a\n${RAW_ENCHANTS}b\n`).source).status, 'already'))
test('patchItemEnchants: a new version without the line', () => assert.equal(patchItemEnchants('something else').status, 'anchor missing'))

// ---------------------------------------------------------------- composite actions: plans
// A plan is an ASCII map of a farm or pen anchored at its NORTH-WEST corner: rows run south (z), columns east (x).
// It is the truth of what SHOULD be there; the world is the truth of what is.
for (const [name, map, expected] of [
  ['a row of wheat beside a channel', 'ww~', [3, 1, 3]],
  ['blank leading and trailing lines are dropped', '\nww\ncc\n', [2, 2, 4]],
  ['a space is outside the plan', 'w w', [3, 1, 2]],
  ['a short row is not padded with cells', 'www\nw', [3, 2, 4]]
]) {
  test(`parsePlan: ${name}`, () => {
    const p = parsePlan(map)
    assert.deepEqual([p.width, p.height, p.cells.length], expected)
  })
}
test('parsePlan: an empty map is an error', () => assert.match(parsePlan('  \n \n').error, /no cells/))
test('parsePlan: an absurd map is an error', () => assert.match(parsePlan('w'.repeat(200)).error, /at most 64/))

test('planCells: the anchor is the north-west corner and y is the ground block', () => {
  assert.deepEqual(planCells({ plan: 'w~\n.c', x: 10, y: 63, z: -90 }).map(c => `${c.ch}@${c.x},${c.y},${c.z}`),
    ['w@10,63,-90', '~@11,63,-90', '.@10,63,-89', 'c@11,63,-89'])
})
// Item 4 (fixes round 2, AhuraMazda): the guide said `t` was a crafting table while the legend said sapling, so a `t`
// cell asked for an oak_sapling nobody wanted and the crafting table never appeared. The legend is the spec's: `t` is a
// sapling, `F` a flower, and a crafting table has a letter of its own.
for (const [ch, kind, item] of [['t', 'sapling', 'oak_sapling'], ['F', 'flower', 'dandelion'], ['A', 'table', 'crafting_table']]) {
  test(`PLAN_LEGEND: ${ch} is a ${kind}`, () => assert.deepEqual([PLAN_LEGEND[ch].kind, PLAN_LEGEND[ch].item], [kind, item]))
}
test('planBill: a crafting table cell asks for a crafting table, not a sapling', () =>
  assert.deepEqual(planBill(planRows('A.t')), { crafting_table: 1, oak_sapling: 1 }))
test('farmJobs: a crafting table cell is built like any other block the plan puts on the ground', () =>
  assert.deepEqual(jobsFor('A', { '0,63,0': 'dirt' }, { crafting_table: 1 }).map(jobLine), ['place crafting_table at 0,64,0']))

test('PLAN_LEGEND: every field crop names the seed that replants it', () => {
  assert.deepEqual(['w', 'c', 'p', 'b'].map(ch => [PLAN_LEGEND[ch].crop, PLAN_LEGEND[ch].seed]),
    [['wheat', 'wheat_seeds'], ['carrots', 'carrot'], ['potatoes', 'potato'], ['beetroots', 'beetroot_seeds']])
})

// `plan` refuses to save a field that cannot work: dry farmland turns back to dirt within minutes, and nothing walks through a corner gate
const planRows = (...r) => parsePlan(r.join('\n'))
for (const [name, parsed, expected] of [
  ['a hydrated field is sound', planRows('wwww~wwww'), []],
  ['a character nobody knows', planRows('wwXw'), ['X at 2,0 is not in the legend (w c p b s m k B ~ . # G T C K F t A)']],
  ['wheat five blocks from the water', planRows('~wwwww'), ['1 cell is farmland with no water within 4 blocks (5,0): move the channel or shorten the row']],
  ['a whole dry row is one complaint, not eight', planRows('wwwwwwww'), ['8 cells are farmland with no water within 4 blocks (0,0 1,0 2,0 3,0 and 4 more): move the channel or shorten the row']],
  ['a gate in the corner', planRows('G##', '#..', '###'), ['the gate at 0,0 is in a corner: nothing can walk through it. Put it in the middle of a wall']],
  ['a gate in the middle of a wall', planRows('#G#', '#..', '###'), []]
]) {
  test(`planErrors: ${name}`, () => assert.deepEqual(planErrors(parsed), expected))
}

// Item 21 (Chani, 09-23): `goto` deliberately steps AROUND planted cells rather than trample them, so a field with no
// walkable lane through it is a field the body cannot stand in - her carrot patch sandwiched its water row between two
// carrot rows with nothing but crops between the gate and the far row, and every walk into it answered `no walkable
// path`. A plan can say that before it is built, and a built field can say it when asked what is standing.
const laneOf = (...rows) => planLane(parsePlan(rows.join('\n')).cells)
for (const [name, cells, expected] of [
  ['a lane from the gate down the rows reaches every crop', laneOf('#G##', '#.c#', '#~c#', '#.c#', '####'), {}],
  ['no lane: the gate opens onto the crop that blocks it', laneOf('#G##', '#cc#', '#~~#', '#cc#', '####'),
    { noLane: '3 crop cells have nothing walkable beside them (2,1 1,3 2,3): lay a . path from the gate through the rows, or a walk into the field answers no walkable path' }],
  ['an open field is worked from its edges', laneOf('www', 'www'), {}],
  ['the middle of a wide open field is out of reach too', laneOf('wwww', 'wwww', 'wwww', 'wwww'),
    { noLane: '4 crop cells have nothing walkable beside them (1,1 2,1 1,2 2,2): lay a . path from the gate through the rows, or a walk into the field answers no walkable path' }],
  ['a plan with no crops has no rows to walk', laneOf('###', '#.#', '#G#'), {}],
  ['a fence with no gate leaves its crop unreachable', laneOf('###', '#w#', '###'),
    { noLane: '1 crop cell has nothing walkable beside it (1,1): lay a . path from the gate through the rows, or a walk into the field answers no walkable path' }],
  ['flowers and saplings are walked through, so they serve the row beside them', laneOf('#G##', '#Fc#', '#tc#', '####'), {}],
  ['a chest is not a lane: it is a block in the way', laneOf('#G##', '#Cc#', '#Cc#', '####'),
    { noLane: '2 crop cells have nothing walkable beside them (2,1 2,2): lay a . path from the gate through the rows, or a walk into the field answers no walkable path' }],
  ['more than four unreachable cells are counted, not listed', laneOf('wwwww', 'wwwww', 'wwwww', 'wwwww', 'wwwww'),
    { noLane: '9 crop cells have nothing walkable beside them (1,1 2,1 3,1 1,2 and 5 more): lay a . path from the gate through the rows, or a walk into the field answers no walkable path' }],
  ['an empty plan is not a complaint', planLane([]), {}]
]) {
  test(`planLane: ${name}`, () => assert.deepEqual(cells, expected))
}

// the census reads the same judgement off the plan the field was built from, so `farm.fields` says it about a field
// that already stands and not only about a plan about to be saved
test('fieldCensus: a field with no lane through it says so', () => {
  const cells = planCells({ plan: '#G##\n#cc#\n#~~#\n#cc#\n####', x: 0, y: 63, z: 0 })
  assert.match(fieldCensus(cells, () => null).noLane, /^3 crop cells have nothing walkable beside them/)
})
test('fieldCensus: a field with a lane says nothing about it', () => {
  const cells = planCells({ plan: '#G##\n#.c#\n#~c#\n#.c#\n####', x: 0, y: 63, z: 0 })
  assert.equal('noLane' in fieldCensus(cells, () => null), false)
})

test('planBill counts the seeds, the water and every block to place', () => {
  assert.deepEqual(planBill(planRows('#GT#', '#ww#', '#~c#', '#CK#')),
    { wheat_seeds: 2, carrot: 1, water_bucket: 1, oak_slab: 1, oak_fence: 9, oak_fence_gate: 1, torch: 1, chest: 1, composter: 1 })
})
test('planStructure: what a plan marks stands ON its ground cell, at y+1', () => {
  assert.deepEqual(planStructure(planCells({ plan: 'wC', x: 0, y: 63, z: 0 }), 'C'), { x: 1, y: 64, z: 0 })
  assert.equal(planStructure(planCells({ plan: 'ww', x: 0, y: 63, z: 0 }), 'K'), null)
})
test('planSummary is one line', () => assert.equal(planSummary(planRows('ww~', 'ww~')), '3x2 wheat:4 water:2'))

// 7b: a farm's own channel is a hole in the field - the body walks in, the pathfinder will not cross it, and `dig`
// refuses everything beside it. A plan's water is built covered: a slab laid IN the source keeps the water and the walk
test('fieldCensus: a slab laid in the channel still waters the field', () => {
  const world = fakeWorld({ '0,63,0': 'farmland', '0,63,1': 'oak_slab~' })
  assert.deepEqual(fieldCensus(planCells({ plan: 'w\n~', x: 0, y: 63, z: 0 }), world),
    { crops: {}, cells: 2, ripe: 0, growing: 0, empty: 1, untilled: 0, dry: 0 })
})
test('fieldCensus: an open channel cell is counted as open, a covered one is not', () => {
  const open = fakeWorld({ '0,63,0': 'farmland', '0,63,1': 'water' })
  assert.equal(fieldCensus(planCells({ plan: 'w\n~', x: 0, y: 63, z: 0 }), open).open, 1)
  const covered = fakeWorld({ '0,63,0': 'farmland', '0,63,1': 'oak_slab~' })
  assert.equal(fieldCensus(planCells({ plan: 'w\n~', x: 0, y: 63, z: 0 }), covered).open, undefined)
})

// ---------------------------------------------------------------- composite actions: what a farm needs
// a fake world: '<x>,<y>,<z>' -> block name, with '#<age>' for a crop
const fakeWorld = table => (x, y, z) => {
  const raw = table[`${x},${y},${z}`]
  if (raw === undefined) return null
  // a trailing '~' is a waterlogged block: a slab laid in a channel still holds its water
  const name = raw.replace(/~$/, '')
  return { name: name.split('#')[0], properties: { age: Number(name.split('#')[1] ?? 0), ...(raw.endsWith('~') ? { waterlogged: 'true' } : {}) } }
}

test('fieldCensus counts what is ripe, growing, empty, untilled and dry', () => {
  const world = fakeWorld({
    '0,63,0': 'farmland', '0,64,0': 'wheat#7', '1,63,0': 'farmland', '1,64,0': 'wheat#3', '0,63,1': 'water', '1,63,1': 'dirt'
  })
  assert.deepEqual(fieldCensus(planCells({ plan: 'ww\n~w', x: 0, y: 63, z: 0 }), world),
    { crops: { wheat: 2 }, cells: 4, ripe: 1, growing: 1, empty: 1, untilled: 1, dry: 0, open: 1 })
})
test('fieldCensus: a channel with no water in it is dry', () => {
  assert.equal(fieldCensus(planCells({ plan: '~', x: 0, y: 63, z: 0 }), fakeWorld({ '0,63,0': 'air' })).dry, 1)
})

// Item 1b (Dan, 2026-09-22): a plan's y is the GROUND block, the farmland / pen floor / path the plan describes.
// Two agents anchored their plans there and the code read it as the level they STAND on: Chani's census called her
// 28 wheat empty beds, and pen.build dug the turf out of her sheep pen to lay a floor one block lower.
test('a plan\u0027s y is the ground block: the crop stands on it at y+1', () => {
  assert.deepEqual(fieldCensus(planCells({ plan: 'w', x: 0, y: 63, z: 0 }), fakeWorld({ '0,63,0': 'farmland', '0,64,0': 'wheat#7' })),
    { crops: { wheat: 1 }, cells: 1, ripe: 1, growing: 0, empty: 0, untilled: 0, dry: 0 })
})
test('a plan\u0027s y is the ground block: the water source is AT it', () => {
  assert.equal(fieldCensus(planCells({ plan: '~', x: 0, y: 63, z: 0 }), fakeWorld({ '0,63,0': 'water' })).dry, 0)
})

// A plan anchored a block off reads as a field of empty, untilled beds (Chani's wheat field), or has pen.build dig the
// turf out and lay the floor one lower (her sheep pen). Two checks, both ways round: the world's own copy of the plan
// standing one up or one down, and - on ground nothing is built on yet - cells that are open air over solid ground,
// which is the level you stand on rather than the ground block a plan names.
for (const [name, plan, world, expected] of [
  ['a plan that matches where it says it is stands still', 'ww', { '0,63,0': 'farmland', '0,64,0': 'wheat#3', '1,63,0': 'farmland', '1,64,0': 'wheat#3' }, 0],
  ['crops one block up mean the anchor is one low', 'ww', { '0,64,0': 'farmland', '0,65,0': 'wheat#3', '1,64,0': 'farmland', '1,65,0': 'wheat#3' }, 1],
  ['a pen one block down means the anchor is one high', '##', { '0,63,0': 'oak_fence', '1,63,0': 'oak_fence' }, -1],
  ['one stray block is not enough to move an anchor', 'www', { '0,65,0': 'wheat#3' }, 0],
  ['bare ground has nothing to judge by', 'ww', {}, 0],
  ['water alone cannot move an anchor: a channel is water at y either way', '~~', { '0,63,0': 'water', '1,63,0': 'water' }, 0],
  ['fresh ground: cells of open air over solid ground are the level you stand on', 'ww',
    { '0,63,0': 'air', '0,62,0': 'grass_block', '1,63,0': 'short_grass', '1,62,0': 'grass_block' }, -1],
  ['fresh ground: the plan sitting ON the turf is right', 'ww',
    { '0,63,0': 'grass_block', '0,62,0': 'dirt', '1,63,0': 'grass_block', '1,62,0': 'dirt' }, 0],
  ['fresh ground: one cell of air is not a level', 'ww', { '0,63,0': 'air', '0,62,0': 'grass_block' }, 0],
  ['fresh ground: air over air is a hole, not a level', 'ww',
    { '0,63,0': 'air', '0,62,0': 'air', '1,63,0': 'air', '1,62,0': 'air' }, 0]
]) {
  test(`planAnchor: ${name}`, () => assert.equal(planAnchor(planCells({ plan, x: 0, y: 63, z: 0 }), fakeWorld(world)).off, expected))
}
test('planAnchor: the note says which y to re-save the plan with', () => {
  const found = planAnchor(planCells({ plan: 'ww', x: 0, y: 63, z: 0 }), fakeWorld({ '0,65,0': 'wheat#3', '1,65,0': 'wheat#3' }))
  assert.match(found.note, /says y=63/)
  assert.match(found.note, /re-save it with y=64/)
})
test('planAnchor: fresh ground says you gave the level you stand on', () => {
  const found = planAnchor(planCells({ plan: 'ww', x: 0, y: 63, z: 0 }),
    fakeWorld({ '0,63,0': 'air', '0,62,0': 'grass_block', '1,63,0': 'air', '1,62,0': 'grass_block' }))
  assert.match(found.note, /level you stand on/)
  assert.match(found.note, /re-save it with y=62/)
})

// planAnchor looks one block up and one down and nowhere else, so the ring of a pen standing a few cells TO THE SIDE
// of its plan is invisible to it: Chani's plan was marked 2 east and 3 south of the pen it describes, pen.build read
// bare ground, and laid a second ring through the middle of the first while 4 sheep stood in it (2026-09-23 02:55Z).
// A 6x6 pen ring standing at x=99..104, z=-73..-68 on ground y=70, gate in the middle of its south wall.
const RING = { plan: '######\n#....#\n#....#\n#....#\n#....#\n##G###', x: 99, y: 70, z: -73 }
const ringWorld = (extra = {}) => {
  const world = {}
  for (let x = 99; x <= 104; x++) {
    for (let z = -73; z <= -68; z++) {
      world[`${x},70,${z}`] = 'grass_block'
      if (x === 99 || x === 104 || z === -73 || z === -68) world[`${x},71,${z}`] = x === 101 && z === -68 ? 'oak_fence_gate' : 'oak_fence'
    }
  }
  return { ...world, ...extra }
}
const besideOf = (place, world = ringWorld()) => planBeside(planCells(place), fakeWorld(world), { name: 'chani-sheep-pen' })

for (const [name, place, world, expected] of [
  ['the ring stands where the plan says', RING, ringWorld(), null],
  ['the plan was marked 2 east and 3 south of the ring it describes', { ...RING, x: 101, y: 71, z: -70 }, ringWorld(), { dx: -2, dy: -1, dz: -3, found: 20 }],
  ['a plan two cells north of its ring, level with it', { ...RING, z: -71 }, ringWorld(), { dx: 0, dy: 0, dz: -2, found: 20 }],
  ['bare ground has nothing standing beside it', { ...RING, x: 101, y: 71, z: -70 }, {}, null],
  ['a ring further off than the reach belongs to no plan of mine', { ...RING, x: 105, y: 70, z: -69 }, ringWorld(), null],
  ['a fence line next door, brushing the plan, is not its ring', { ...RING, x: 101, y: 71, z: -70 },
    { '101,72,-71': 'oak_fence', '102,72,-71': 'oak_fence', '103,72,-71': 'oak_fence', '104,72,-71': 'oak_fence', '105,72,-71': 'oak_fence', '106,72,-71': 'oak_fence' }, null]
]) {
  test(`planBeside: ${name}`, () => {
    const found = besideOf(place, world)
    assert.deepEqual(found && { dx: found.dx, dy: found.dy, dz: found.dz, found: found.found }, expected)
  })
}

test('planBeside: the note says where the ring stands and how to re-mark the place there', () => {
  const found = besideOf({ ...RING, x: 101, y: 71, z: -70 })
  assert.match(found.note, /2 west and 3 north/)
  assert.match(found.note, /mark name=chani-sheep-pen x=99 y=70 z=-73/)
  assert.deepEqual(found.at, { x: 99, y: 70, z: -73 })
})

test('pen.build: a plan marked beside the pen it describes is refused before a block is moved', async () => {
  const world = ringWorld()
  const { api, calls } = fakeApi({
    place: { ...fakePlace(RING.plan, 101, 71, -70), name: 'chani-sheep-pen', kind: 'pen' },
    world,
    items: { oak_fence: 21, oak_fence_gate: 1, dirt: 12 }
  })
  await assert.rejects(buildPen.run(api, { place: 'chani-sheep-pen' }),
    /chani-sheep-pen is not where its plan says.*2 west and 3 north.*mark name=chani-sheep-pen x=99 y=70 z=-73/s)
  assert.deepEqual(calls, ['goto x=103 y=72 z=-68 range=2'], 'it walks there and stops')
})

const jobsFor = (plan, world, items) => farmJobs({ cells: planCells({ plan, x: 0, y: 63, z: 0 }), worldAt: fakeWorld(world), items })
const jobLine = j => j.item ? `${j.do} ${j.item} at ${j.x},${j.y},${j.z}` : `${j.do} ${j.x},${j.y},${j.z}`
for (const [name, plan, world, items, expected] of [
  ['bare farmland is planted', 'w', { '0,63,0': 'farmland' }, { wheat_seeds: 64 }, ['plant wheat_seeds at 0,64,0']],
  ['trampled farmland is tilled again, then planted', 'w', { '0,63,0': 'dirt' }, { wheat_seeds: 64 }, ['till 0,63,0', 'plant wheat_seeds at 0,64,0']],
  ['a ripe crop is left for the harvest', 'w', { '0,63,0': 'farmland', '0,64,0': 'wheat#7' }, { wheat_seeds: 64 }, []],
  ['a growing crop is left alone', 'w', { '0,63,0': 'farmland', '0,64,0': 'wheat#3' }, { wheat_seeds: 64 }, []],
  ['a weed on the bed is cleared first', 'w', { '0,63,0': 'farmland', '0,64,0': 'short_grass' }, { wheat_seeds: 64 }, ['clear 0,64,0', 'plant wheat_seeds at 0,64,0']],
  ['a flower that sprang up on the bed is cleared too', 'w', { '0,63,0': 'farmland', '0,64,0': 'dandelion' }, { wheat_seeds: 64 }, ['clear 0,64,0', 'plant wheat_seeds at 0,64,0']],
  ['somebody else\u0027s block on the bed is left alone', 'w', { '0,63,0': 'farmland', '0,64,0': 'cobblestone' }, { wheat_seeds: 64 }, []],
  ['a dry channel is refilled by pouring onto the block UNDER the water', '~', { '0,63,0': 'air' }, { water_bucket: 1 }, ['pour water_bucket at 0,62,0', 'cover oak_slab at 0,63,0']],
  ['a channel somebody filled in is dug out again first', '~', { '0,63,0': 'dirt' }, { water_bucket: 1 }, ['clear water_bucket at 0,63,0', 'pour water_bucket at 0,62,0', 'cover oak_slab at 0,63,0']],
  ['open water gets a slab laid in it: a channel nobody can fall into', '~', { '0,63,0': 'water' }, { oak_slab: 4 }, ['cover oak_slab at 0,63,0']],
  ['a covered channel is finished', '~', { '0,63,0': 'oak_slab~' }, { oak_slab: 4 }, []],
  ['a dry channel is poured first and covered after', '~', { '0,63,0': 'air' }, { water_bucket: 1, oak_slab: 4 }, ['pour water_bucket at 0,62,0', 'cover oak_slab at 0,63,0']],
  ['a dry slab over nothing is not a channel: pour under it', '~', { '0,63,0': 'oak_slab' }, { water_bucket: 1, oak_slab: 4 }, ['clear water_bucket at 0,63,0', 'pour water_bucket at 0,62,0', 'cover oak_slab at 0,63,0']],
  ['a missing fence is put back', '#', { '0,63,0': 'dirt' }, { oak_fence: 8 }, ['place oak_fence at 0,64,0']],
  ['a fence that stands is left alone', '#', { '0,63,0': 'dirt', '0,64,0': 'oak_fence' }, {}, []],
  ['a path cell is left as it is', '.', { '0,63,0': 'grass_block' }, {}, []],
  // Chani's farm.maintain gave up on "twice in a row: dig: that block is under water": the job list told it to dig the
  // block under its own channel, with the channel's water still standing on top of it. dig refuses that, rightly.
  ['a filled-in channel under standing water is skipped, not dug', '~', { '0,63,0': 'dirt', '0,64,0': 'water' }, { water_bucket: 1 }, ['skip 0,63,0']],
  ['a weed on a bed under water is skipped too', 'w', { '0,63,0': 'farmland', '0,64,0': 'short_grass', '0,65,0': 'water' }, { wheat_seeds: 64 }, ['skip 0,64,0']],
  ['water two blocks up still means a dive', '~', { '0,63,0': 'dirt', '0,65,0': 'water' }, { water_bucket: 1 }, ['skip 0,63,0']]
]) {
  test(`farmJobs: ${name}`, () => assert.deepEqual(jobsFor(plan, world, items).map(jobLine), expected))
}
test('farmJobs: a skipped cell says why, and carries no item to fetch', () => {
  const [skip] = jobsFor('~', { '0,63,0': 'dirt', '0,64,0': 'water' }, { water_bucket: 1 })
  assert.match(skip.why, /water stands over it/)
  assert.equal(skip.item, undefined)
})
test('farmJobs flags the seed I do not carry', () => {
  assert.deepEqual(jobsFor('c', { '0,63,0': 'farmland' }, {}).map(j => [j.do, j.item, j.have]), [['plant', 'carrot', false]])
})
test('farmJobs: what a plan puts ON the ground goes at y+1', () => {
  assert.deepEqual(jobsFor('#', { '0,63,0': 'grass_block' }, { oak_fence: 4 }).map(jobLine), ['place oak_fence at 0,64,0'])
})
// A bush grew where claude-test-pen's west wall goes, and the build walked past it: the pen stood with a hole in it
// and pen.check called it a leak. A plant in the way of a fence, chest or torch is weeding, not somebody's block.
for (const [name, plan, world, items, expected] of [
  ['a bush where a fence belongs is cleared first', '#', { '0,63,0': 'grass_block', '0,64,0': 'bush' }, { oak_fence: 4 },
    ['clear 0,64,0', 'place oak_fence at 0,64,0']],
  ['grass where a chest belongs is cleared first', 'C', { '0,63,0': 'grass_block', '0,64,0': 'short_grass' }, { chest: 1 },
    ['clear 0,64,0', 'place chest at 0,64,0']],
  ['somebody\u0027s block where a fence belongs is left alone', '#', { '0,63,0': 'grass_block', '0,64,0': 'cobblestone' }, { oak_fence: 4 }, []],
  ['a bush on a crop bed is cleared too', 'w', { '0,63,0': 'farmland', '0,64,0': 'bush' }, { wheat_seeds: 4 },
    ['clear 0,64,0', 'plant wheat_seeds at 0,64,0']]
]) {
  test(`farmJobs: ${name}`, () => assert.deepEqual(jobsFor(plan, world, items).map(jobLine), expected))
}
// Item 3 (fixes round 2): a first pass over a 28-bed field planted 15 of them. Bare farmland goes back to dirt - dry
// within minutes, and any of it the moment something jumps on it - so a field tilled in one pass and sown in the next
// loses the beds the body walked back over. Each till is followed AT ONCE by the planting of its own cell.
for (const [name, plan, world, items, expected] of [
  ['each bed is sown the moment it is tilled', 'ww',
    { '0,63,0': 'dirt', '1,63,0': 'dirt' }, { wheat_seeds: 64 },
    ['till 0,63,0', 'plant wheat_seeds at 0,64,0', 'till 1,63,0', 'plant wheat_seeds at 1,64,0']],
  ['a bed that only wants seed is sown after the tilled ones', 'ww',
    { '0,63,0': 'farmland', '1,63,0': 'dirt' }, { wheat_seeds: 64 },
    ['till 1,63,0', 'plant wheat_seeds at 1,64,0', 'plant wheat_seeds at 0,64,0']],
  ['the weeding still comes before any of it', 'w',
    { '0,63,0': 'dirt', '0,64,0': 'short_grass' }, { wheat_seeds: 64 },
    ['clear 0,64,0', 'till 0,63,0', 'plant wheat_seeds at 0,64,0']],
  ['a till with no seed to follow it is still done', 'w', { '0,63,0': 'dirt' }, {},
    ['till 0,63,0', 'plant wheat_seeds at 0,64,0']]
]) {
  test(`farmJobs: ${name}`, () => assert.deepEqual(jobsFor(plan, world, items).map(jobLine), expected))
}

// the plain `till` primitive says the same thing, for a hand-tilled bed nobody is about to sow
for (const [name, dry, total, expected] of [
  ['dry beds name the number and the cure', 2, 4, /^2 of 4 have no water within 4 blocks .*plant them AT ONCE/],
  ['wet beds are warned about trampling instead', 0, 4, /turns back to dirt the moment anything jumps on it/]
]) {
  test(`tillWarning: ${name}`, () => assert.match(tillWarning(dry, total), expected))
}

test('farmJobs does the ground work before the planting', () => {
  assert.deepEqual(jobsFor('w~', { '0,63,0': 'dirt', '0,63,1': 'air' }, { wheat_seeds: 1, water_bucket: 1 }).map(j => j.do), ['till', 'plant'])
})

// ---------------------------------------------------------------- composite actions: the runner's rules
for (const [name, spec, given, expected] of [
  ['what it asks for', { place: 'string!', days: 'number' }, { place: 'north-field', days: 3 }, null],
  ['a missing required argument', { place: 'string!', days: 'number' }, {}, 'maintain_farm needs place='],
  ['a misspelt argument', { place: 'string!', days: 'number' }, { place: 'f', palce: 2 }, 'maintain_farm: palce= is not an argument here (place, days?)'],
  ['a wrong type', { days: 'number' }, { days: 'three' }, 'maintain_farm: days= wants a number, got "three"'],
  ['the runner is allowed its own arguments', { place: 'string!' }, { place: 'f', timeout: 600 }, null],
  // apiary.maintain forwarded range: a.range unset to apiary.inspect and crashed every default run (Mariel, 09-23)
  ['an optional argument a composite forwards unset is simply absent', { place: 'string!', range: 'number' }, { place: 'f', range: undefined }, null],
  ['a required argument forwarded unset is still missing', { place: 'string!' }, { place: undefined }, 'maintain_farm needs place=']
]) {
  test(`checkArgs: ${name}`, () => assert.equal(checkArgs('maintain_farm', spec, given), expected))
}

// a composite cannot opt out of these: the runner checks them between steps and hands control back to the driver
const RUNNING_WELL = { health: 20, food: 20, edible: true, night: false, bedNear: true, elapsedDays: 0, now: 0 }
for (const [name, state, expected] of [
  ['nothing wrong', {}, null],
  ['someone spoke to me', { spoken: 'mruwnik: claude come here' }, 'spoken to (mruwnik: claude come here)'],
  ['half dead', { health: 6 }, 'health 6'],
  ['starving with nothing to eat', { food: 5, edible: false }, 'food 5 and nothing edible carried'],
  ['starving with bread in my pocket is no reason to stop', { food: 5, edible: true }, null],
  ['the same step failing twice', { failedTwice: 'till: no hoe' }, 'twice in a row: till: no hoe'],
  ['full up with nowhere to put it', { invFull: true, canDeposit: false }, 'inventory full and no chest to deposit in'],
  ['full up beside a chest is no reason to stop', { invFull: true, canDeposit: true }, null],
  ['night with no bed', { night: true, bedNear: false }, 'night and no bed within 32 blocks'],
  ["night with a bed is the runner's own business", { night: true, bedNear: true }, null],
  ['the day limit', { days: 2, elapsedDays: 2 }, 'days'],
  ['the count limit', { count: 5, done: 5 }, 'count'],
  ['the deadline', { until: 100, now: 100 }, 'until']
]) {
  test(`handBackReason: ${name}`, () => assert.equal(handBackReason({ ...RUNNING_WELL, ...state }), expected))
}

// a library module must declare itself before the body will run it: a bad one is caught at start, not mid-errand
for (const [name, mod, expected] of [
  ['a good module', { doc: 'x', args: { place: 'string!' }, run: () => {} }, null],
  ['no run', { doc: 'x', args: {} }, 'library/foo.mjs: needs `run`'],
  ['no doc', { args: {}, run: () => {} }, 'library/foo.mjs: needs `doc` (its one line in the guide)'],
  ['no args spec', { doc: 'x', run: () => {} }, 'library/foo.mjs: needs `args` ({} when it takes none)'],
  ['a type nobody knows', { doc: 'x', args: { n: 'int' }, run: () => {} }, 'library/foo.mjs: args.n is "int"; use string, number, boolean, any (with ! for required)']
]) {
  test(`compositeError: ${name}`, () => assert.equal(compositeError('foo', mod), expected))
}

// ---------------------------------------------------------------- composite actions: the composites themselves
// A composite only ever touches the world through `api`, so a fake api makes its sequence of primitives a table test.
const fakePlace = (plan, x = 0, y = 63, z = 0) => {
  const parsed = parsePlan(plan)
  return { name: 'test-field', kind: 'farm', x, y, z, plan, parsed, cells: planCells({ plan, x, y, z }), bill: planBill(parsed) }
}

test('maintain_farm: harvests first, then tills and plants what the plan says is missing', async () => {
  const world = { '0,63,0': 'dirt' }
  const { api, calls } = fakeApi({
    place: fakePlace('w'), world, items: { wheat_seeds: 32 },
    answers: {
      'farm.harvest': { harvested: { wheat: 1 }, replanted: 0 },
      till: () => { world['0,63,0'] = 'farmland'; return {} },
      place: () => { world['0,64,0'] = 'wheat'; return {} }
    }
  })
  const summary = await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual(calls, ['goto x=0 y=64 z=0 range=2', 'farm.harvest within=8', 'till 0,63,0', 'place item=wheat_seeds x=0 y=64 z=0'])
  // the field came out as the plan asks: one pass, and nothing left to report
  assert.deepEqual([summary.replanted, summary.unfinished], [1, undefined])
})

test('maintain_farm: a bed that is already planted is left alone', async () => {
  const { api, calls } = fakeApi({ place: fakePlace('w'), world: { '0,63,0': 'farmland', '0,64,0': 'wheat#3' }, items: { wheat_seeds: 32 } })
  await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual(calls, ['goto x=0 y=64 z=0 range=2', 'farm.harvest within=8'])
})

// Chani, 2026-09-22: `farm.maintain days=1` stopped on "twice in a row: dig: that block is under water" — the block it
// dug was under her own channel. A cell the plan itself floods is never dug; it is handed back as skipped=.
test('maintain_farm: never digs under its own channel, and says which cells it left', async () => {
  const { api, calls } = fakeApi({
    place: fakePlace('~w'), world: { '0,63,0': 'dirt', '0,64,0': 'water', '1,63,0': 'farmland', '1,64,0': 'wheat#3' }, items: { water_bucket: 1, wheat_seeds: 32 }
  })
  const summary = await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual(calls, ['goto x=0 y=64 z=0 range=2', 'farm.harvest within=8'])
  assert.match(summary.skipped, /^0,63,0 \(dirt where the channel should be, and water stands over it/)
  assert.equal(summary.unfinished, undefined)
})

test('farm.build: a cell its own water stands over is left for the driver, not dug', async () => {
  const { api, calls } = fakeApi({
    place: fakePlace('~'), world: { '0,63,0': 'dirt', '0,64,0': 'water', '0,62,0': 'stone' }, items: { water_bucket: 1 }
  })
  const summary = await buildFarm.run(api, { place: 'test-field' })
  assert.deepEqual(calls, ['goto x=0 y=64 z=0 range=2'])
  assert.match(summary.skipped, /water stands over it/)
})

// 7b in game: the slab went down and the body walked over it, but the reply read `poured=1 undefined=1` - cover had no
// name in the builder's tally, so the one job the driver most wants to see was the one it could not read.
test('farm.build: covering an open channel is counted under its own name', async () => {
  const { api, calls } = fakeApi({
    place: fakePlace('~'), world: { '0,63,0': 'water', '0,62,0': 'stone' }, items: { oak_slab: 1 }
  })
  const summary = await buildFarm.run(api, { place: 'test-field' })
  assert.equal(summary.covered, 1)
  assert.equal(summary.undefined, undefined)
  assert.match(calls.join('\n'), /place item=oak_slab x=0 y=63 z=0 half=bottom/)
})

test('maintain_farm: an open channel is covered, and counted under its own name', async () => {
  const { api } = fakeApi({
    place: fakePlace('~'), world: { '0,63,0': 'water', '0,62,0': 'stone' }, items: { oak_slab: 1 }
  })
  const summary = await maintainFarm.run(api, { place: 'test-field' })
  assert.equal(summary.covered, 1)
})

test('maintain_farm: says what it had no seed for, and does not try it', async () => {
  const { api, calls } = fakeApi({ place: fakePlace('c'), world: { '0,63,0': 'farmland' }, items: {} })
  const summary = await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual([summary.missing, calls.filter(c => c.startsWith('place'))], ['carrot:1', []])
})

// The sweep runs its job list ONCE. A step that says it worked and left the ground as it was used to be hidden by a second
// pass that quietly did it again; the primitives now read the cell back, so a leftover is named instead (unfinished=).
test('maintain_farm: a job that quietly did nothing is named, not silently repeated', async () => {
  const { api, calls } = fakeApi({
    place: fakePlace('w'), world: { '0,63,0': 'dirt' }, items: { wheat_seeds: 32 },
    answers: { till: () => ({}) }
  })
  const summary = await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual([calls.filter(c => c.startsWith('till')).length, summary.unfinished],
    [1, 'till 0,63,0 (dirt where farmland should be); plant 0,64,0 (an empty bed)'])
})

test('maintain_farm: one job that fails does not throw away the rest of the sweep', async () => {
  const { api, calls } = fakeApi({
    place: fakePlace('ww'), world: { '0,63,0': 'dirt', '1,63,0': 'dirt' }, items: { wheat_seeds: 32 },
    answers: { till: new Error('maintain_farm/till: no hoe') }
  })
  const summary = await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual([calls.filter(c => c.startsWith('till')).length, summary.stuck], [2, 'maintain_farm/till: no hoe'])
})


test('maintain_farm: the surplus goes into the chest the plan marks', async () => {
  const place = fakePlace('wC')
  const { api, calls } = fakeApi({
    place, world: { '0,63,0': 'farmland', '0,64,0': 'wheat#3', '1,63,0': 'dirt', '1,64,0': 'chest' }, items: { wheat: 40, wheat_seeds: 9 },
    answers: { 'farm.harvest': { harvested: { wheat: 40 } } }
  })
  await maintainFarm.run(api, { place: 'test-field' })
  assert.deepEqual(calls.filter(c => c.startsWith('deposit')), ['deposit items(wheat:40 wheat_seeds:7) x=1 y=64 z=0'])
})

// what a farm makes, above the seed the plan needs to sow itself again, belongs in the chest
for (const [name, items, reserve, expected] of [
  ['the harvest, less the seed to sow the field again', { wheat: 40, wheat_seeds: 20 }, { wheat_seeds: 8 }, { wheat: 40, wheat_seeds: 12 }],
  ['nothing to spare', { wheat_seeds: 4 }, { wheat_seeds: 8 }, {}],
  ['tools and rubble are not produce', { wheat: 3, iron_pickaxe: 1, cobblestone: 64 }, {}, { wheat: 3 }],
  ['bread is kept to eat, not stored', { bread: 6, wheat: 3 }, {}, { wheat: 3 }]
]) {
  test(`farmSurplus: ${name}`, () => assert.deepEqual(farmSurplus(items, reserve), expected))
}

// ---------------------------------------------------------------- composite actions: composting
for (const [name, items, opts, expected] of [
  // with no items= the whole inventory is offered, so listing every tool and stone as skipped is noise: only what a
  // composter WOULD take and was held back is worth a line
  ['everything compostable, the seed held back', { wheat: 10, wheat_seeds: 64, cobblestone: 5 }, {}, [['wheat:10'], ['wheat_seeds']]],
  ['the food I live on is never composted by default', { bread: 7, carrot: 4, wheat: 10 }, {}, [['wheat:10'], ['bread', 'carrot']]],
  ['bread asked for by name goes in all the same', { bread: 7 }, { want: { bread: 2 } }, [['bread:2'], []]],
  ['what I asked for by name and count', { wheat: 10, rotten_flesh: 3 }, { want: { wheat: 4 } }, [['wheat:4'], []]],
  ['a bare item name means all of it', { wheat: 10 }, { want: 'wheat' }, [['wheat:10'], []]],
  ['the chest form of items=', { wheat: 10 }, { want: [{ name: 'wheat', count: 3 }] }, [['wheat:3'], []]],
  ['a reserve is left alone', { wheat: 10 }, { keep: { wheat: 8 } }, [['wheat:2'], []]],
  ['nothing a composter eats', { cobblestone: 5 }, {}, [[], []]],
  ['asking for something it will not eat says so', { cobblestone: 5 }, { want: { cobblestone: 1 } }, [[], ['cobblestone']]],
  ['asking for seed I hold back by default takes it anyway', { wheat_seeds: 30 }, { want: { wheat_seeds: 30 } }, [['wheat_seeds:30'], []]]
]) {
  test(`compostPlan: ${name}`, () => {
    const p = compostPlan(items, opts)
    assert.deepEqual([p.feed.map(f => `${f.name}:${f.count}`), p.skipped.map(s => s.name)], expected)
  })
}
test('COMPOST_CHANCE: bread is worth more to a composter than a seed', () => assert.ok(COMPOST_CHANCE.bread > COMPOST_CHANCE.wheat_seeds))

test('compost: feeds the composter and takes the bone meal out when it fills', async () => {
  let level = 6
  const { api, calls } = fakeApi({
    items: { wheat: 2 },
    answers: { find_blocks: { positions: [{ x: 5, y: 64, z: 5 }] }, use: () => { level = level === 8 ? 0 : level + 1; return {} } }
  })
  api.block = () => ({ name: 'composter', properties: { level } })
  const summary = await compost.run(api, {})
  assert.deepEqual([calls[0], calls.filter(c => c === 'use 5,64,5').length, summary.fed, summary.boneMeal],
    ['find_blocks block=composter maxDistance=32', 3, 'wheat:2', 1])
})

test('compost: with no composter within reach it says how to get one', async () => {
  const { api } = fakeApi({ items: { wheat: 2 }, answers: { find_blocks: { positions: [] } } })
  await assert.rejects(compost.run(api, {}), /composter/)
})

test('compost: nothing in my pockets a composter would take', async () => {
  const { api, calls } = fakeApi({ items: { cobblestone: 5 }, answers: { find_blocks: { positions: [{ x: 5, y: 64, z: 5 }] } } })
  api.block = () => ({ name: 'composter', properties: { level: 0 } })
  const summary = await compost.run(api, {})
  assert.deepEqual([summary.fed, calls.filter(c => c.startsWith('use')).length], [undefined, 0])
})

test('compost: the composter a plan marks is used when a place is named', async () => {
  const { api, calls } = fakeApi({ items: { wheat: 1 }, place: fakePlace('wK'), answers: { use: () => ({}) } })
  api.block = () => ({ name: 'composter', properties: { level: 0 } })
  await compost.run(api, { place: 'test-field' })
  assert.deepEqual(calls.filter(c => c.startsWith('use')), ['use 1,64,0'])
})

// ---------------------------------------------------------------- composite actions: routines
const ROLE_FILES = { 'farmer/homestead': '[{"action":"maintain_farm","place":"north-field"},{"action":"compost"}]' }
const readRole = name => ROLE_FILES[name] ?? null
for (const [name, args, expected] of [
  ['a step list', { steps: [{ action: 'maintain_farm', place: 'f' }] }, [{ action: 'maintain_farm', place: 'f' }]],
  ['a routine a role ships', { name: 'farmer/homestead' }, [{ action: 'maintain_farm', place: 'north-field' }, { action: 'compost' }]],
  ['neither', {}, 'routine needs steps= or name= (a routine shipped in roles/<role>/<name>.json)'],
  ['a name nobody ships', { name: 'farmer/nope' }, 'no routine called farmer/nope (roles/farmer/nope.json)'],
  ['a step with no action', { steps: [{ place: 'f' }] }, 'step 1 has no action='],
  ['a step list that is not a list', { steps: { action: 'x' } }, 'steps= must be a list of {"action":...} objects'],
  ['a routine file that is not JSON', { name: 'farmer/broken' }, 'roles/farmer/broken.json is not a list of steps: Unexpected token']
]) {
  test(`routineSteps: ${name}`, () => {
    const r = routineSteps(args, n => (n === 'farmer/broken' ? 'not json at all' : readRole(n)))
    assert.deepEqual(r.error?.slice(0, String(expected).length) ?? r.steps, expected)
  })
}

test('routine: runs every step in order and notes what each one did', async () => {
  const { api, calls } = fakeApi({ answers: { maintain_farm: { replanted: 3 }, compost: { boneMeal: 1 } } })
  const summary = await routine.run(api, { steps: [{ action: 'maintain_farm', place: 'north-field' }, { action: 'compost' }] })
  assert.deepEqual([calls.filter(c => !c.startsWith('note')), summary.days, summary.ran],
    [['maintain_farm place=north-field', 'compost'], 1, 2])
})

test('routine: a step that fails is noted and the next one still runs', async () => {
  const { api, calls } = fakeApi({ answers: { maintain_farm: new Error('routine/maintain_farm: no plan called north-field'), compost: {} } })
  const summary = await routine.run(api, { steps: [{ action: 'maintain_farm', place: 'north-field' }, { action: 'compost' }] })
  assert.deepEqual([calls.filter(c => !c.startsWith('note')).length, summary.failed], [2, 'maintain_farm: routine/maintain_farm: no plan called north-field'])
})

// ---------------------------------------------------------------- composite actions: getting seed
for (const [name, crop, expected] of [
  ['wheat comes from breaking grass', 'wheat', ['grass', 'short_grass', 'wheat_seeds']],
  ['sugar cane is top-cut from a wild stand, never dug up', 'sugar_cane', ['stalk', 'sugar_cane', 'sugar_cane']],
  ['bamboo the same way', 'bamboo', ['stalk', 'bamboo', 'bamboo']],
  ['carrots come out of a farm chest', 'carrot', ['chest', 'carrots', 'carrot']],
  ['potatoes, plural or not', 'potatoes', ['chest', 'potatoes', 'potato']],
  ['beetroot seeds likewise', 'beetroot', ['chest', 'beetroots', 'beetroot_seeds']],
  ['melons are cut from a wild patch, leaving the stem', 'melon', ['wild', 'melon', 'melon_slice']],
  ['pumpkins likewise', 'pumpkin', ['wild', 'pumpkin', 'pumpkin']]
]) {
  test(`seedSource: ${name}`, () => {
    const s = seedSource(crop)
    assert.deepEqual([s.from, s.block, s.item], expected)
  })
}
test('seedSource: a crop nobody grows', () => assert.equal(seedSource('coffee'), null))

test('get_seeds: wheat seed comes from breaking grass, clump by clump', async () => {
  let seeds = 0
  const { api, calls } = fakeApi({ answers: { find_blocks: { positions: [{ x: 1, y: 64, z: 1 }, { x: 2, y: 64, z: 1 }] }, dig: () => { seeds += 4; return {} } } })
  api.inv = () => ({ wheat_seeds: seeds })
  const summary = await getSeeds.run(api, { crop: 'wheat', count: 16 })
  assert.deepEqual([calls.filter(c => c.startsWith('dig')).length, calls.includes('collect'), summary.got], [4, true, 16])
})

test('get_seeds: two rounds that find no grass at all and it gives up', async () => {
  const { api, calls } = fakeApi({ answers: { find_blocks: { positions: [] } } })
  api.inv = () => ({})
  const summary = await getSeeds.run(api, { crop: 'wheat', count: 16 })
  assert.deepEqual([calls.filter(c => c.startsWith('find_blocks')).length, summary.got, summary.gaveUp],
    [2, 0, 'nothing more to take within 64 blocks: walk somewhere else and run it again'])
})

test('get_seeds: cane is cut where it stands, and the base is left to regrow', async () => {
  let cane = 0
  const { api, calls } = fakeApi({ answers: { find_blocks: { positions: [{ x: 9, y: 64, z: 9 }] }, 'farm.harvest': () => { cane += 4; return {} } } })
  api.inv = () => ({ sugar_cane: cane })
  await getSeeds.run(api, { crop: 'sugar_cane', count: 4 })
  assert.deepEqual(calls, ['find_blocks block=sugar_cane maxDistance=64 count=8', 'goto x=9 y=64 z=9 range=3', 'farm.harvest within=6'])
})

test('get_seeds: carrots come out of the chest the plan marks', async () => {
  let carrots = 0
  const { api, calls } = fakeApi({ place: fakePlace('cC'), answers: { withdraw: () => { carrots = 8; return {} } } })
  api.inv = () => ({ carrot: carrots })
  await getSeeds.run(api, { crop: 'carrot', count: 8, place: 'test-field' })
  assert.deepEqual(calls, ['withdraw items(carrot:8) x=1 y=64 z=0'])
})

test('get_seeds: a crop that comes from a chest needs a farm to take it from', async () => {
  const { api } = fakeApi({})
  api.inv = () => ({})
  await assert.rejects(getSeeds.run(api, { crop: 'carrot', count: 8 }), /place=/)
})

test('get_seeds: a crop nobody grows this way', async () => {
  const { api } = fakeApi({})
  await assert.rejects(getSeeds.run(api, { crop: 'coffee', count: 4 }), /coffee/)
})

test('routineSteps: a shipped routine takes the farm it works on from place=', () => {
  const r = routineSteps({ name: 'farmer/homestead', place: 'claude-test-field' }, () => '[{"action":"maintain_farm","place":"$place"},{"action":"compost","place":"$place"}]')
  assert.deepEqual(r.steps, [{ action: 'maintain_farm', place: 'claude-test-field' }, { action: 'compost', place: 'claude-test-field' }])
})

test('routineSteps: a shipped routine that works on a farm and is given none', () => {
  const r = routineSteps({ name: 'farmer/homestead' }, () => '[{"action":"maintain_farm","place":"$place"}]')
  assert.equal(r.error, 'routine name=farmer/homestead needs place=<the name of a marked farm> to work on')
})

// grass drops a seed about one clump in eight, so a round that breaks grass and brings nothing back is normal;
// two of them in a row means the grass within reach is gone, not that the world is out of seed
test('get_seeds: rounds that come back empty-handed count as barren too', async () => {
  const { api, calls } = fakeApi({ answers: { find_blocks: { positions: [{ x: 1, y: 64, z: 1 }] } } })
  api.inv = () => ({ wheat_seeds: 5 })
  const summary = await getSeeds.run(api, { crop: 'wheat', count: 16 })
  assert.deepEqual([calls.filter(c => c.startsWith('dig')).length, summary.got, summary.gaveUp],
    [2, 0, 'nothing more to take within 64 blocks: walk somewhere else and run it again'])
})

// ---------------------------------------------------------------- the help catalogue
const HELP_ENTRIES = [
  { name: 'dig', section: 'block', args: 'x= y= z= [wet=]', doc: 'break one block and pick up what it drops' },
  { name: 'state', section: 'sense', args: '', doc: 'health, food, the time and who is about' },
  { name: 'pen.check', section: 'pen', args: 'x= y= z= [radius=]', doc: 'walk a fence and find where it leaks' },
  { name: 'farm.maintain', args: 'place= [days=]', doc: 'harvest, replant and re-till one saved plan', stops: 'days= done, or two failures in a row', composite: true },
  { name: 'farm.fields', args: '[place=]', doc: 'a census of every plan near me', stops: 'nothing: it only looks', composite: true }
]
for (const [name, topic, expected] of [
  ['the whole catalogue: every action with its arguments, sections in reading order', undefined, [
    'sense: what I can see from here',
    '  state',
    'block: blocks and the ground',
    '  dig x= y= z= [wet=]',
    'farm: crops, fields and what comes off them',
    '  farm.maintain place= [days=] | stops: days= done, or two failures in a row',
    '  farm.fields [place=] | stops: nothing: it only looks',
    'pen: fences and gates',
    '  pen.check x= y= z= [radius=]',
    './mc help <section> or ./mc help <action> for what one does',
    renamedList()
  ]],
  ['one section', 'farm', [
    'farm: crops, fields and what comes off them',
    '  farm.maintain place= [days=] - harvest, replant and re-till one saved plan',
    '    stops: days= done, or two failures in a row',
    '  farm.fields [place=] - a census of every plan near me',
    '    stops: nothing: it only looks'
  ]],
  ['one composite', 'farm.maintain', [
    'farm.maintain place= [days=]',
    'harvest, replant and re-till one saved plan',
    'stops: days= done, or two failures in a row'
  ]],
  ['one primitive', 'dig', ['dig x= y= z= [wet=]', 'break one block and pick up what it drops']],
  ['a section that is also spelled like nothing else', 'pen', ['pen: fences and gates', '  pen.check x= y= z= [radius=] - walk a fence and find where it leaks']],
  ['something nobody has', 'digg', ['unknown action digg: did you mean dig?']]
]) {
  test(`helpText: ${name}`, () => assert.deepEqual(helpText(topic, HELP_ENTRIES).split('\n'), expected))
}

for (const [name, args, expected] of [
  ['required first, optional in brackets', { place: 'string!', days: 'number' }, 'place= [days=]'],
  ['an action that takes nothing', {}, ''],
  ['every argument optional', { items: 'any', place: 'string' }, '[items=] [place=]']
]) {
  test(`argsUsage: ${name}`, () => assert.equal(argsUsage(args), expected))
}

test('didYouMean: a dotted name is matched on either half', () =>
  assert.match(didYouMean('maintain', ['farm.maintain', 'dig']), /farm\.maintain/))

// the catalogue is the only place a primitive is described, so an entry that is half-written is a hole in ./mc help
test('PRIMITIVES: every primitive belongs to a section the help knows', () =>
  assert.deepEqual(Object.entries(PRIMITIVES).filter(([, p]) => !SECTIONS[p.section]).map(([name]) => name), []))

test('PRIMITIVES: every primitive says what it does and what it takes', () =>
  assert.deepEqual(Object.entries(PRIMITIVES).filter(([, p]) => !p.doc || typeof p.args !== 'string').map(([name]) => name), []))

test('PRIMITIVES: the catalogue covers every section', () =>
  assert.deepEqual(Object.keys(SECTIONS).filter(s => !['farm', 'pen', 'flock', 'mine', 'work'].includes(s))
    .filter(s => !Object.values(PRIMITIVES).some(p => p.section === s)), []))

test('helpText: the real catalogue is one line per action, under what an agent will read', () => {
  const text = helpText(undefined, Object.entries(PRIMITIVES).map(([name, p]) => ({ name, ...p })))
  assert.deepEqual([text.split('\n').length > Object.keys(PRIMITIVES).length, text.length < 6000], [true, true])
})

for (const [name, doc, expected] of [
  ['the usage line is stripped off, the sentence kept', 'compost [items=]: feed a composter what a farm cannot use', 'feed a composter what a farm cannot use'],
  ['a doc that is only a sentence', 'feed a composter', 'feed a composter'],
  ['only the first colon counts', 'a: b: c', 'b: c']
]) {
  test(`docText: ${name}`, () => assert.equal(docText(doc), expected))
}

// ./mc help farm.maintain: a bare word used to be chopped into a nonsense key (farm.maintai=farm.maintain)
for (const [name, argv, expected] of [
  ['key=value pairs', ['x=1', 'y=2'], { x: 1, y: 2 }],
  ['a bare word is the topic', ['farm.maintain'], { topic: 'farm.maintain' }],
  ['-v asks for the raw answer, it is not an argument', ['x=1', '-v'], { x: 1 }],
  ['JSON values are parsed', ['items={"wheat":2}'], { items: { wheat: 2 } }],
  ['an = inside the value is kept', ['message=a=b'], { message: 'a=b' }],
  ['nothing at all', [], {}]
]) {
  test(`parseCliArgs: ${name}`, () => assert.deepEqual(parseCliArgs(argv), expected))
}

// ---------------------------------------------------------------- farm.plan and farm.fields
test('farm.plan: a plan that waters every cell is checked, then saved on the shared map', async () => {
  const { api, calls } = fakeApi({ places: [] })
  const out = await farmPlan.run(api, { name: 'north-field', map: 'w~w', x: 10, y: 64, z: -20 })
  assert.deepEqual([calls, out.saved, out.needs], [['mark name=north-field kind=farm note=3x1 wheat:2 water map=w~w x=10 y=64 z=-20'], 'north-field', { wheat_seeds: 2, oak_slab: 1, water_bucket: 1 }])
})

test('farm.plan: a plan with a cell nothing waters is refused before anything is dug', async () => {
  const { api, calls } = fakeApi({ places: [] })
  await assert.rejects(farmPlan.run(api, { name: 'north-field', map: 'w~wwwwww', x: 0, y: 64, z: 0 }), /no water within 4 blocks/)
  assert.deepEqual(calls, [])
})

test('farm.plan: name= alone prints what is saved, with its bill of materials', async () => {
  const { api } = fakeApi({ places: [{ name: 'north-field', kind: 'farm', x: 1, y: 64, z: 2, plan: 'w~w' }] })
  const out = await farmPlan.run(api, { name: 'north-field' })
  assert.match(out.text, /north-field farm @1,64,2\nw~w\n3x1 wheat:2 water needs wheat_seeds:2 oak_slab water_bucket/)
})

test('farm.fields: every plan in range, counted from the map without walking', async () => {
  const { api, calls } = fakeApi({
    places: [{ name: 'north-field', kind: 'farm', x: 0, y: 63, z: 0, plan: 'ww' }],
    world: { '0,64,0': 'wheat#7', '1,64,0': 'wheat#3', '0,63,0': 'farmland', '1,63,0': 'farmland' }
  })
  const out = await farmFields.run(api, {})
  assert.deepEqual([out.text, calls], ['north-field 0m crops(wheat:2) cells=2 ripe=1 growing=1 empty=0 untilled=0 dry=0', []])
})

test('farm.fields: a plan anchored one block low is counted where its crops really stand, and says so', async () => {
  const { api } = fakeApi({
    places: [{ name: 'north-field', kind: 'farm', x: 0, y: 63, z: 0, plan: 'ww' }],
    world: { '0,65,0': 'wheat#7', '1,65,0': 'wheat#3', '0,64,0': 'farmland', '1,64,0': 'farmland' }
  })
  const out = await farmFields.run(api, {})
  assert.match(out.text, /crops\(wheat:2\) cells=2 ripe=1 growing=1 empty=0 untilled=0 dry=0/)
  assert.match(out.text, /anchor: the plan says y=63.*re-save it with y=64/)
})

test('farm.plan: saving a plan over blocks that stand one level up warns which y to use', async () => {
  const { api } = fakeApi({ places: [], world: { '0,65,0': 'wheat#3', '1,65,0': 'wheat#3' } })
  const out = await farmPlan.run(api, { name: 'north-field', map: 'ww~', x: 0, y: 63, z: 0 })
  assert.match(out.warn, /re-save it with y=64/)
})

test('farm.maintain: a plan anchored at the wrong level is refused, not tilled a block under the farm', async () => {
  const place = fakePlace('ww')
  const { api, calls } = fakeApi({ place, world: { '0,65,0': 'wheat#3', '1,65,0': 'wheat#3', '0,64,0': 'farmland', '1,64,0': 'farmland' }, items: { wheat_seeds: 32 } })
  await assert.rejects(maintainFarm.run(api, { place: 'test-field' }), /re-save it with y=64/)
  assert.deepEqual(calls, ['goto x=0 y=64 z=0 range=2'])
})

test('farm.build: a plan anchored at the wrong level is refused before a block is moved', async () => {
  const place = fakePlace('ww')
  const { api, calls } = fakeApi({ place, world: { '0,65,0': 'wheat#3', '1,65,0': 'wheat#3', '0,64,0': 'farmland', '1,64,0': 'farmland' }, items: { wheat_seeds: 32 } })
  await assert.rejects(buildFarm.run(api, { place: 'test-field' }), /re-save it with y=64/)
  assert.deepEqual(calls, ['goto x=0 y=64 z=0 range=2'])
})

// The other way round, and the one Dan watched happen: a plan saved at the level you STAND on, over ground nothing is
// built on yet. Chani's pen.build dug the turf out of chani-sheep-pen and laid its floor a block lower.
test('farm.build: a plan saved at the level you stand on is refused, with the y to re-save it with', async () => {
  const place = fakePlace('ww', 0, 64, 0)
  const { api, calls } = fakeApi({
    place, items: { wheat_seeds: 32, dirt: 8 },
    world: { '0,64,0': 'air', '1,64,0': 'short_grass', '0,63,0': 'grass_block', '1,63,0': 'grass_block' }
  })
  await assert.rejects(buildFarm.run(api, { place: 'test-field' }), /level you stand on.*re-save it with y=63/)
  assert.deepEqual(calls, ['goto x=0 y=65 z=0 range=2'])
})

// Item 21 (Chani, 09-23): a field with nothing walkable between its gate and its far row cannot be worked at all, and
// nothing said so until every walk into it had already answered "no walkable path". A lane-less plan is legal - it is a
// shape, not a contradiction - so it is saved with a warning rather than refused, and the field says it too.
test('farm.plan: a plan with no lane through its rows is saved, with the cells nothing can stand beside', async () => {
  const { api, calls } = fakeApi({ places: [] })
  const out = await farmPlan.run(api, { name: 'north-field', map: '#G##\n#cc#\n#~~#\n#cc#\n####', x: 0, y: 63, z: 0 })
  assert.match(out.warn, /^3 crop cells have nothing walkable beside them \(2,1 1,3 2,3\): lay a \. path/)
  assert.equal(calls.length, 1)
})

test('farm.plan: a plan with a path from the gate down the rows warns about nothing', async () => {
  const { api } = fakeApi({ places: [] })
  const out = await farmPlan.run(api, { name: 'north-field', map: '#G##\n#.c#\n#~c#\n#.c#\n####', x: 0, y: 63, z: 0 })
  assert.equal(out.warn, undefined)
})

test('farm.fields: a field with no lane through it says so on a line of its own', async () => {
  const { api } = fakeApi({ places: [{ name: 'north-field', kind: 'farm', x: 0, y: 63, z: 0, plan: '#G##\n#cc#\n#~~#\n#cc#\n####' }] })
  const out = await farmFields.run(api, {})
  assert.match(out.text, /^north-field 0m crops\(carrots:0\)|^north-field 0m cells=/)
  assert.match(out.text, /\n {2}lane: 3 crop cells have nothing walkable beside them \(2,1 1,3 2,3\): lay a \. path/)
})

test('farm.fields: a place that is on the map but has no plan is not a field', async () => {
  const { api } = fakeApi({ places: [{ name: 'my-hut', kind: 'base', x: 0, y: 64, z: 0 }] })
  await assert.rejects(farmFields.run(api, {}), /no farm plan within 48 blocks/)
})

test('argsUsage: the three coordinates read as one thing, not three', () =>
  assert.equal(argsUsage({ place: 'string', x: 'number', y: 'number', z: 'number', range: 'number' }), '[place=] [x= y= z=] [range=]'))

test('helpText: a composite with no folder is a job in its own right, not a section of one', () =>
  assert.deepEqual(helpText(undefined, [{ name: 'routine', args: '[days=]', doc: 'run a list of steps', stops: 'days= done', composite: true }]).split('\n'), [
    'work: whole jobs that run themselves',
    '  routine [days=] | stops: days= done',
    './mc help <section> or ./mc help <action> for what one does',
    renamedList()
  ]))

// ---------------------------------------------------------------- collect
const LYING = [
  { id: 1, item: 'wheat', x: 3, y: 64, z: 0, dist: 3, deep: false, outsidePen: false },
  { id: 2, item: 'bone', x: 1, y: 64, z: 0, dist: 1, deep: false, outsidePen: false },
  { id: 3, item: 'wheat_seeds', x: 9, y: 64, z: 0, dist: 9, deep: false, outsidePen: true }
]
test('collect: walks to each drop in turn, nearest first, and leaves what is outside the pen', async () => {
  const { api, calls } = fakeApi({ drops: LYING })
  const out = await collect.run(api, { range: 16 })
  assert.deepEqual([calls, out.picked, out.outsidePen], [
    ['goto x=1 y=64 z=0 range=0', 'goto x=3 y=64 z=0 range=0'],
    2,
    '1 drops lie outside this pen and were left: walk out yourself (goto), then collect again'])
})

test('collect: a drop deep in water is left where it is, and named', async () => {
  const { api, calls } = fakeApi({ drops: [{ id: 1, item: 'sand', x: 2, y: 62, z: 0, dist: 2, deep: true, outsidePen: false }] })
  const out = await collect.run(api, {})
  assert.deepEqual([calls, out.inWater.startsWith('left in deep water: sand')], [[], true])
})

test('collect: a full inventory stops it where it stands', async () => {
  const { api, calls } = fakeApi({ drops: LYING.slice(0, 2), freeSlots: 0 })
  const out = await collect.run(api, {})
  assert.deepEqual([calls.length, out.picked, out.inventoryFull.startsWith('left lying:')], [1, 1, true])
})

// ---------------------------------------------------------------- farm.harvest
test('farm.harvest: digs what is ripe, puts the seed straight back, and leaves the rest growing', async () => {
  const world = { '0,64,0': 'wheat#7', '1,64,0': 'wheat#3' }
  const { api, calls } = fakeApi({
    world,
    answers: {
      find_blocks: args => ({ positions: String(args.block).includes('wheat') ? [{ x: 0, y: 64, z: 0 }, { x: 1, y: 64, z: 0 }] : [] }),
      dig: args => { delete world[`${args.x},${args.y},${args.z}`]; return {} }
    }
  })
  const out = await farmHarvest.run(api, { within: 8 })
  assert.deepEqual([out.harvested, out.replanted, out.stillGrowing, calls.filter(c => c.startsWith('dig') || c.startsWith('place'))],
    [{ wheat: 1 }, 1, 1, ['dig 0,64,0', 'place item=wheat_seeds x=0 y=64 z=0']])
})

test('farm.harvest: a stalk is cut at the second segment, never at its base', async () => {
  const world = { '5,66,5': 'sugar_cane', '5,65,5': 'sugar_cane', '5,64,5': 'sand' }
  const { api, calls } = fakeApi({
    world,
    answers: {
      find_blocks: args => ({ positions: String(args.block).includes('bamboo') ? [{ x: 5, y: 66, z: 5 }, { x: 5, y: 65, z: 5 }] : [] }),
      dig: args => { delete world[`${args.x},${args.y},${args.z}`]; return {} }
    }
  })
  const out = await farmHarvest.run(api, {})
  assert.deepEqual([out.harvested, calls.filter(c => c.startsWith('dig')), out.stalkBases],
    [{ sugar_cane: 1 }, ['dig 5,66,5'], 'all 1 still stand and will regrow'])
})

test('farm.harvest: a ripe crop it cannot walk to is named, and the rest is still harvested', async () => {
  const world = { '0,64,0': 'wheat#7', '9,64,9': 'wheat#7' }
  const { api } = fakeApi({
    world,
    answers: {
      find_blocks: args => ({ positions: String(args.block).includes('wheat') ? [{ x: 0, y: 64, z: 0 }, { x: 9, y: 64, z: 9 }] : [] }),
      dig: args => {
        if (args.x === 9) throw new Error('dig: no walkable path')
        delete world[`${args.x},${args.y},${args.z}`]
        return {}
      }
    }
  })
  const out = await farmHarvest.run(api, {})
  assert.deepEqual([out.harvested, out.unreachable.startsWith('1 ripe crops I could not get to (first 9,64,9)')], [{ wheat: 1 }, true])
})

// ---------------------------------------------------------------- mine.get
const stoneWorld = () => ({ '1,64,0': 'stone', '2,64,0': 'stone', '3,64,0': 'stone', '0,63,0': 'dirt' })
const stoneApi = (world, extra = {}) => fakeApi({
  world,
  items: { stone_pickaxe: 1, dirt: 4 },
  answers: {
    find_blocks: { positions: [{ x: 1, y: 64, z: 0 }, { x: 2, y: 64, z: 0 }, { x: 3, y: 64, z: 0 }] },
    zones: { zones: [] },
    path_to: { status: 'success' },
    dig: args => { delete world[`${args.x},${args.y},${args.z}`]; return {} },
    ...extra
  }
})

test('mine.get: digs them one at a time and stops at count=', async () => {
  const world = stoneWorld()
  const { api, calls } = stoneApi(world)
  const out = await mineGet.run(api, { block: 'stone', count: 2 })
  assert.deepEqual([calls.filter(c => c.startsWith('dig')), out.got], [['dig x=1 y=64 z=0 dig', 'dig x=2 y=64 z=0 dig'], 2])
})

test(`mine.get: someone else's build is left standing, even when it is the only stone about`, async () => {
  const world = stoneWorld()
  const { api, calls } = stoneApi(world, { zones: { zones: [{ name: 'someones-hut', x1: 0, y1: 60, z1: -2, x2: 5, y2: 70, z2: 2 }] } })
  const out = await mineGet.run(api, { block: 'stone', count: 2 })
  assert.deepEqual([calls.filter(c => c.startsWith('dig')), out.got, out.gaveUp],
    [[], 0, `the only stone within 48 blocks is inside protected zones (someone's build): go further away and retry`])
})

test('mine.get: a pit it cannot climb out of is named, not hidden', async () => {
  const world = stoneWorld()
  const { api } = stoneApi(world, { path_to: { status: 'noPath' }, goto: new Error('goto: no walkable path') })
  const out = await mineGet.run(api, { block: 'stone', count: 1 })
  assert.deepEqual([out.got, out.pit], [1, 'you are in the pit you dug and could not get back out: goto with dig=true'])
})

test('mine.get: it will not sink a shaft in a pen with animals in it', async () => {
  const world = stoneWorld()
  const { api } = stoneApi(world)
  api.pen = () => ({ enclosed: true, census: { inside: 'sheep:4' } })
  await assert.rejects(mineGet.run(api, { block: 'stone', count: 1 }), /you stand in a pen with animals in it \(sheep:4\)/)
})

test('mine.get: a full inventory is refused before the first swing, not after it', async () => {
  const world = stoneWorld()
  const { api, calls } = fakeApi({ world, items: { stone_pickaxe: 1 }, freeSlots: 0 })
  await assert.rejects(mineGet.run(api, { block: 'stone', count: 1 }), /inventory full: mine.get needs one free slot/)
  assert.deepEqual(calls, [])
})

test('mine.get: the hand-back rules never cut it short of putting the ground back', async () => {
  const world = stoneWorld()
  const { api, calls, checkpoints } = stoneApi(world)
  await mineGet.run(api, { block: 'stone', count: 2 })
  // count= is this loop's own limit: telling the runner about it would end the task down in the pit it just dug
  assert.deepEqual([checkpoints.every(c => c.done === undefined), calls.at(-1).startsWith('path_to')], [true, true])
})

// ---------------------------------------------------------------- flock.breed
const sheepApi = (extra = {}, opts = {}) => fakeApi({
  items: { wheat: 4 },
  answers: {
    animals: { found: [{ mob: 'sheep', id: 11, at: '1,64,1', dist: 2, grown: true, inMyPen: true }, { mob: 'sheep', id: 12, at: '3,64,1', dist: 4, grown: true, inMyPen: true }, { mob: 'sheep', id: 13, at: '2,64,2', dist: 3, grown: false, inMyPen: true }] },
    feed: { fed: 1, with: 'wheat' },
    ...extra
  },
  ...opts
})

test('flock.breed: feeds the two nearest grown ones and counts who really ate', async () => {
  const { api, calls } = sheepApi()
  const out = await flockBreed.run(api, { mob: 'sheep' })
  assert.deepEqual([calls.filter(c => c.startsWith('feed')), out.fed, out.with, out.herd],
    [['feed mob=sheep id=11', 'feed mob=sheep id=12'], 2, 'wheat', 2])
})

test('flock.breed: a lamb is no partner, and two are needed', async () => {
  const { api } = sheepApi({ animals: { found: [{ mob: 'sheep', id: 11, at: '1,64,1', dist: 2, grown: true, inMyPen: true }, { mob: 'sheep', id: 13, at: '2,64,2', dist: 3, grown: false, inMyPen: true }] } })
  await assert.rejects(flockBreed.run(api, { mob: 'sheep' }), /breeding takes two: 1 grown sheep within 24 blocks/)
})

test('flock.breed: standing in a pen, the ones outside it are none of my business', async () => {
  const { api } = sheepApi({ animals: { found: [{ mob: 'sheep', id: 11, at: '1,64,1', dist: 2, grown: true, inMyPen: true }, { mob: 'sheep', id: 12, at: '9,64,9', dist: 9, grown: true, inMyPen: false }] } })
  api.pen = () => ({ enclosed: true, floor: ['1,64,1'] })
  await assert.rejects(flockBreed.run(api, { mob: 'sheep' }), /breeding takes two: 1 grown sheep in this pen with you/)
})

test('flock.breed: what it cannot breed, and what it has nothing to feed', async () => {
  const { api } = sheepApi()
  await assert.rejects(flockBreed.run(api, { mob: 'creeper' }), /cannot breed creeper: one of/)
  const { api: empty } = fakeApi({ items: { bread: 2 }, answers: { animals: { found: [] } } })
  await assert.rejects(flockBreed.run(empty, { mob: 'sheep' }), /a sheep eats wheat: you carry none/)
})

// ---------------------------------------------------------------- flock.lead
const PEN = { name: 'paddock', kind: 'pen', x: 10, y: 64, z: 10 }
const leadApi = (extra = {}) => fakeApi({
  items: { wheat: 4 },
  places: [PEN],
  answers: {
    'pen.check': { pen: 'holds', cells: 20, inside: 'sheep:2', outside: 'sheep@1,64,1' },
    escort: { arrived: true, with: 2, animals: 'sheep@10,64,11 sheep@10,64,12', inside: 'sheep:2', outside: 'sheep@1,64,1' },
    ...extra
  }
})

// the count comes from the escort, taken from the cells it walked BEFORE the gate behind us is shut: a pen with its gate
// still open reads as open country, and a check here would report no pen at all
test('flock.lead: place= names the cell, and who came in is counted there', async () => {
  const { api, calls } = leadApi()
  const out = await flockLead.run(api, { mob: 'sheep', place: 'paddock' })
  assert.deepEqual([calls, out.arrived, out.with, out.inside, out.outside],
    [['pen.check 10,64,10', 'escort mob=sheep x=10 y=64 z=10'], true, 2, 'sheep:2', 'sheep@1,64,1'])
})

test('flock.lead: a gate left ajar at the goal is shut before a single animal is fetched', async () => {
  let shut = false
  const { api, calls } = leadApi({
    'pen.check': () => (shut ? { pen: 'holds', cells: 20, inside: 'sheep:2' } : { pen: 'LEAKS', via: '12,64,10' }),
    toggle: () => { shut = true; return { block: 'oak_fence_gate', now: 'closed' } }
  })
  api.block = (x, y, z) => (x === 12 ? { name: 'oak_fence_gate', properties: { open: true }, solid: false } : null)
  const out = await flockLead.run(api, { mob: 'sheep', x: 10, y: 64, z: 10 })
  assert.deepEqual([calls.slice(0, 3), out.shutFirst],
    [['pen.check 10,64,10', 'toggle x=12 y=64 z=10', 'pen.check 10,64,10'], 'the gate at 12,64,10 stood open: shut it before fetching them'])
})

test('flock.lead: a goal inside a block, an unknown place and an animal it cannot lead are all refused', async () => {
  const { api } = leadApi()
  api.block = () => ({ name: 'oak_fence', properties: {}, solid: true })
  await assert.rejects(flockLead.run(api, { mob: 'sheep', x: 10, y: 64, z: 10 }), /10,64,10 is inside a oak_fence, not a spot to stand on/)
  const { api: plain } = leadApi()
  await assert.rejects(flockLead.run(plain, { mob: 'sheep', place: 'nowhere' }), /no place called nowhere/)
  await assert.rejects(flockLead.run(plain, { mob: 'creeper', x: 1, y: 2, z: 3 }), /cannot lead creeper: one of/)
})

test('flock.lead: with nothing it follows, it never sets off', async () => {
  const { api, calls } = fakeApi({ items: { bread: 1 }, places: [PEN] })
  await assert.rejects(flockLead.run(api, { mob: 'sheep', place: 'paddock' }), /a sheep follows wheat: you carry none/)
  assert.deepEqual(calls, [])
})

test('flock.lead: when the escort gives up, its reason is what the driver gets', async () => {
  const { api } = leadApi({ escort: { arrived: false, with: 0, why: 'the sheep will not follow (fetched it 3 times, got no nearer)' } })
  const out = await flockLead.run(api, { mob: 'sheep', place: 'paddock' })
  assert.deepEqual([out.arrived, out.why, out.inside], [false, 'the sheep will not follow (fetched it 3 times, got no nearer)', undefined])
})

test('flock.lead: a goal in open country is no pen: nothing to shut, nothing to count', async () => {
  const { api, calls } = leadApi({
    'pen.check': new Error('10,64,10 is not a spot to stand on'),
    escort: { arrived: true, with: 2, animals: 'sheep@10,64,11 sheep@10,64,12' }
  })
  const out = await flockLead.run(api, { mob: 'sheep', x: 10, y: 64, z: 10 })
  assert.deepEqual([calls, out.arrived, out.inside, out.notes], [['pen.check 10,64,10', 'escort mob=sheep x=10 y=64 z=10'], true, undefined, undefined])
})

// `mine block=short_grass` never worked: the collect plugin wants a path target and refuses a block that breaks in one
// hit and may drop nothing. mine.get walks and digs itself, so a block gone from its cell counts even when nothing was gained
test('mine.get: grass breaks in one hit and often drops nothing: it still counts', async () => {
  const world = { '1,64,0': 'short_grass', '2,64,0': 'short_grass', '3,64,0': 'short_grass', '0,63,0': 'dirt' }
  const { api, calls } = fakeApi({
    world,
    items: {},
    answers: {
      find_blocks: { positions: [{ x: 1, y: 64, z: 0 }, { x: 2, y: 64, z: 0 }, { x: 3, y: 64, z: 0 }] },
      zones: { zones: [] },
      path_to: { status: 'success' },
      dig: args => { delete world[`${args.x},${args.y},${args.z}`]; return {} }
    }
  })
  const out = await mineGet.run(api, { block: 'short_grass', count: 3 })
  assert.deepEqual([out.got, out.gaveUp, calls.filter(c => c.startsWith('dig')).length], [3, undefined, 3])
})

// A plant that reported ok and left the bed empty, once a sweep: bot.placeBlock resolves whether or not the server kept
// the block, so `place` counted it as done without ever looking. Farm sweeps ran their whole job list twice to paper over it.
for (const [name, before, after, expected] of [
  ['a seed became a crop', 'air', 'wheat', null],
  ['a block appeared', 'air', 'oak_fence', null],
  ['the water it replaced is gone', 'water', 'dirt', null],
  ['nothing happened at all', 'air', 'air', /the server dropped it without a word/],
  ['the water is still there', 'water', 'water', /the server dropped it without a word/]
]) {
  test(`placeMissed: ${name}`, () => assert.match(String(placeMissed(before, after)), expected ?? /^null$/))
}

// ---------------------------------------------------------------- farm.build: ground first, then the plan
for (const [name, bill, items, expected] of [
  ['nothing missing', { wheat_seeds: 4 }, { wheat_seeds: 10 }, {}],
  ['exactly enough', { dirt: 3 }, { dirt: 3 }, {}],
  ['some of it missing', { wheat_seeds: 4, oak_fence: 8 }, { wheat_seeds: 1 }, { wheat_seeds: 3, oak_fence: 8 }],
  ['nothing in my pockets', { chest: 1 }, {}, { chest: 1 }]
]) {
  test(`billShortfall: ${name}`, () => assert.deepEqual(billShortfall(bill, items), expected))
}

for (const [name, job, expected] of [
  ['a block in the way is dug', { do: 'clear', x: 1, y: 2, z: 3 }, ['dig', { x: 1, y: 2, z: 3 }]],
  ['a seed is placed', { do: 'plant', x: 1, y: 2, z: 3, item: 'carrot' }, ['place', { item: 'carrot', x: 1, y: 2, z: 3 }]],
  ['a floor is placed', { do: 'fill', x: 1, y: 2, z: 3, item: 'dirt' }, ['place', { item: 'dirt', x: 1, y: 2, z: 3 }]],
  ['the ground is tilled', { do: 'till', x: 1, y: 2, z: 3 }, ['till', { x: 1, y: 2, z: 3 }]],
  ['water is poured', { do: 'pour', x: 1, y: 2, z: 3 }, ['pour', { x: 1, y: 2, z: 3 }]]
]) {
  test(`jobCall: ${name}`, () => assert.deepEqual(jobCall(job), expected))
}

// the ground a plan needs before anything can be tilled or planted: a floor under every cell and open air in it
const groundOf = (plan, world) => groundJobs({
  cells: planCells({ plan, x: 0, y: 63, z: 0 }),
  worldAt: (x, y, z) => world[`${x},${y},${z}`] === undefined ? null : { name: world[`${x},${y},${z}`] },
  solid: name => name !== 'air' && name !== 'water' && name !== 'short_grass'
})
for (const [name, plan, world, expected] of [
  ['flat ground with room above needs nothing', 'w', { '0,63,0': 'dirt', '0,64,0': 'air', '0,65,0': 'air' }, []],
  ['a hole under the bed gets a floor', 'w', { '0,63,0': 'air', '0,64,0': 'air', '0,65,0': 'air' },
    [{ do: 'fill', x: 0, y: 63, z: 0, why: 'air where the floor should be', item: 'dirt' }]],
  ['a pond under the bed gets a floor', 'w', { '0,63,0': 'water', '0,64,0': 'air', '0,65,0': 'air' },
    [{ do: 'fill', x: 0, y: 63, z: 0, why: 'water where the floor should be', item: 'dirt' }]],
  ['sugar cane stands on sand, so the floor is sand', 's', { '0,63,0': 'air', '0,64,0': 'air', '0,65,0': 'air' },
    [{ do: 'fill', x: 0, y: 63, z: 0, why: 'air where the floor should be', item: 'sand' }]],
  ['a boulder in the bed and over it comes out', 'w', { '0,63,0': 'dirt', '0,64,0': 'stone', '0,65,0': 'stone' },
    [{ do: 'clear', x: 0, y: 64, z: 0, why: 'stone stands in the cell' },
      { do: 'clear', x: 0, y: 65, z: 0, why: 'stone stands where the plan wants open air' }]],
  ['weeds are left to the job list, they are not levelling', 'w', { '0,63,0': 'dirt', '0,64,0': 'short_grass', '0,65,0': 'air' }, []],
  ['what the plan already has is never dug out', 'C', { '0,63,0': 'dirt', '0,64,0': 'chest', '0,65,0': 'air' }, []],
  ['a crop already growing is never dug out', 'w', { '0,63,0': 'farmland', '0,64,0': 'wheat', '0,65,0': 'air' }, []],
  ['a channel is floored one below, because its source sits at the plan\u0027s y', '~', { '0,62,0': 'air', '0,63,0': 'air', '0,64,0': 'air', '0,65,0': 'air' },
    [{ do: 'fill', x: 0, y: 62, z: 0, why: 'air where the floor should be', item: 'dirt' }]],
  // Chani's sheep pen, 99,71,-73: pen.build dug the grass layer out and laid its floor one block lower, because the
  // plan's y was read as the level she stood on. The ground a plan names is the floor: it is never dug.
  ['the ground the plan names is the floor, never something to dig out', '.', { '0,63,0': 'grass_block', '0,64,0': 'air', '0,65,0': 'air' }, []],
  ['a pen floor of turf needs no levelling either', '#', { '0,63,0': 'grass_block', '0,64,0': 'air', '0,65,0': 'air' }, []],
  ['cells out of sight are left alone', 'w', {}, []]
]) {
  test(`groundJobs: ${name}`, () => assert.deepEqual(groundOf(plan, world), expected))
}

// clearing comes before filling: dig the boulder out, then floor the hole it stood over
test('groundJobs: the head is cleared before the floor is laid', () => {
  const jobs = groundOf('w', { '0,63,0': 'air', '0,64,0': 'stone', '0,65,0': 'air' })
  assert.deepEqual(jobs.map(j => j.do), ['clear', 'fill'])
})

test('farm.build: the ground is levelled, then the plan is tilled and planted', async () => {
  const world = { '0,63,0': 'air', '0,64,0': 'air', '0,65,0': 'air' }
  const { api, calls } = fakeApi({
    place: fakePlace('w'), world, items: { wheat_seeds: 32, dirt: 8 },
    answers: {
      place: ({ item, x, y, z }) => { world[`${x},${y},${z}`] = item === 'wheat_seeds' ? 'wheat' : item; return {} },
      till: ({ x, y, z }) => { world[`${x},${y},${z}`] = 'farmland'; return {} }
    }
  })
  const summary = await buildFarm.run(api, { place: 'test-field' })
  assert.deepEqual(calls, ['goto x=0 y=64 z=0 range=2', 'place item=dirt x=0 y=63 z=0', 'till 0,63,0', 'place item=wheat_seeds x=0 y=64 z=0'])
  // dirt for the floor is part of what a build needs, though no plan legend mentions it
  assert.deepEqual([summary.levelled, summary.planted, summary.unfinished], [1, 1, undefined])
})

// the bill is checked before a single block is moved: half a farm is worse than none
test('farm.build: without the materials it refuses and does nothing but walk there', async () => {
  const { api, calls } = fakeApi({ place: fakePlace('ww'), world: { '0,63,0': 'dirt', '1,63,0': 'dirt' }, items: { wheat_seeds: 1 } })
  await assert.rejects(buildFarm.run(api, { place: 'test-field' }), /still needs wheat_seeds:1/)
  assert.deepEqual(calls, ['goto x=0 y=64 z=0 range=2'])
})

// what is asked for is what is still MISSING from the ground, not the whole plan: a farm that already stands used to be
// refused for the chest and the bucket that were sitting in it (09-22)
test('farm.build: a plan that already stands is left alone', async () => {
  const { api, calls } = fakeApi({
    place: fakePlace('w'), world: { '0,63,0': 'farmland', '0,64,0': 'wheat', '0,65,0': 'air' }, items: {}
  })
  const summary = await buildFarm.run(api, { place: 'test-field' })
  assert.deepEqual([calls, summary.already], [['goto x=0 y=64 z=0 range=2'], 'everything the plan asks for is already there'])
})

for (const [name, jobs, expected] of [
  ['one of each', [{ do: 'plant', item: 'carrot' }, { do: 'place', item: 'chest' }], { carrot: 1, chest: 1 }],
  ['a job that needs nothing carried', [{ do: 'till' }, { do: 'clear' }], {}],
  ['the same seed twice', [{ do: 'plant', item: 'carrot' }, { do: 'plant', item: 'carrot' }], { carrot: 2 }],
  ['one bucket does a whole field', [{ do: 'pour', item: 'water_bucket' }, { do: 'pour', item: 'water_bucket' }], { water_bucket: 1 }]
]) {
  test(`jobsBill: ${name}`, () => assert.deepEqual(jobsBill(jobs), expected))
}

// Chani, BUGS.md 2026-09-23 00:28Z: farm.build ran out of water buckets and left a `~` cell as a HOLE. Standing near
// it made every goto fail and she could not path out of her own finished field. The dig that opens a channel and the
// pour that fills it are one job in two halves, and only the second half was ever conditional on carrying the water:
// the first half ran, the second was skipped for want of a bucket, and the pit stayed. The dig now carries
// water_bucket too, so a body with no water in hand never opens the hole it cannot fill - at the bill, and again at
// the moment the job runs, which is the case that bit her (one bucket bills for a whole field but empties on first use).
test('farm.build: a channel is never dug out by a body that carries no water', async () => {
  const world = { '0,63,0': 'grass_block', '1,63,0': 'grass_block' }
  const { api, calls } = fakeApi({ place: fakePlace('~~'), world, items: {} })
  const summary = await buildFarm.run(api, { place: 'test-field', partial: true })
  assert.deepEqual(calls.filter(c => c.startsWith('dig')), [], 'no cell is dug')
  assert.equal(summary.missing, 'water_bucket:1')
})

// the other half of Chani's hole: one that is ALREADY dug, because the bucket emptied between the dig and the pour.
// Running the build again finds an open cell it cannot fill with water, and fills it back in with ground instead of
// walking past a pit. What is missing is still the water, not the dirt, so that is what missing= names.
test('farm.build: a channel cell already dug out and still unwaterable is filled back in', async () => {
  const world = { '0,63,0': 'air', '0,62,0': 'dirt' }
  const { api, calls } = fakeApi({ place: fakePlace('~'), world, items: { dirt: 8 } })
  const summary = await buildFarm.run(api, { place: 'test-field', partial: true })
  assert.deepEqual(calls.filter(c => c.startsWith('place')), ['place item=dirt x=0 y=63 z=0'])
  assert.equal(summary.missing, 'water_bucket:1')
})

test('farm.build: a dry channel on solid ground is left alone, not filled', async () => {
  const world = { '0,63,0': 'grass_block', '0,62,0': 'dirt' }
  const { api, calls } = fakeApi({ place: fakePlace('~'), world, items: { dirt: 8 } })
  await buildFarm.run(api, { place: 'test-field', partial: true })
  assert.deepEqual(calls.filter(c => c.startsWith('place') || c.startsWith('dig')), [])
})

test('farm.build: a plan with a dry channel and no bucket is refused before anything is touched', async () => {
  const world = { '0,63,0': 'grass_block', '1,63,0': 'grass_block' }
  const { api, calls } = fakeApi({ place: fakePlace('~~'), world, items: { oak_slab: 4 } })
  await assert.rejects(buildFarm.run(api, { place: 'test-field' }), /still needs water_bucket:1/)
  assert.deepEqual(calls.filter(c => c.startsWith('dig') || c.startsWith('place')), [])
})

test('farm.build: a channel IS dug out by a body that carries the water for it', async () => {
  const world = { '0,63,0': 'grass_block' }
  const { api, calls } = fakeApi({
    place: fakePlace('~'), world, items: { water_bucket: 1, oak_slab: 1 },
    answers: { dig: ({ x, y, z }) => { world[`${x},${y},${z}`] = 'air'; return {} } }
  })
  await buildFarm.run(api, { place: 'test-field', partial: true })
  assert.deepEqual(calls.filter(c => c.startsWith('dig')), ['dig 0,63,0'])
})

test('farm.build: partial=true builds what it can and names the rest', async () => {
  const world = { '0,63,0': 'dirt', '1,63,0': 'dirt' }
  const { api, calls } = fakeApi({
    place: fakePlace('ww'), world, items: {},
    answers: { till: ({ x, y, z }) => { world[`${x},${y},${z}`] = 'farmland'; return {} } }
  })
  const summary = await buildFarm.run(api, { place: 'test-field', partial: true })
  assert.deepEqual([summary.missing, calls.filter(c => c.startsWith('place'))], ['wheat_seeds:2', []])
})

// A bucket emptied at the wrong block floods everything downhill, and `pour` used to return `above=air` and call it done:
// that is how claude-test-field drowned (09-22). The water that appeared where nobody asked for it is found by comparing
// the sources around me before and after the click.
for (const [name, before, after, expected] of [
  ['it went where it was asked to', ['1,64,1'], [{ x: 1, y: 64, z: 1, name: 'water', level: 0 }], null],
  ['a new source at my head', [], [{ x: 1, y: 66, z: 1, name: 'water', level: 0 }], { x: 1, y: 66, z: 1, name: 'water', level: 0 }],
  ['flowing water is not the source', [], [{ x: 1, y: 66, z: 1, name: 'water', level: 1 }], null],
  ['another fluid is not mine', [], [{ x: 1, y: 66, z: 1, name: 'lava', level: 0 }], null],
  ['a source that was already there', ['1,66,1'], [{ x: 1, y: 66, z: 1, name: 'water', level: 0 }], null]
]) {
  test(`strayFluid: ${name}`, () => assert.deepEqual(strayFluid(new Set(before), after, 'water'), expected))
}

// ---------------------------------------------------------------- pen.build: where to stand when asking whether it holds
// the plan's y is the pen FLOOR, so the spot to stand on (where pen.check wants feet) is one block above it
const cellsOf = plan => planCells({ plan, x: 10, y: 63, z: 20 })
for (const [name, plan, expected] of [
  ['the open floor inside the walls', '###\n#.#\n#G#', { x: 11, y: 64, z: 21 }],
  ['the middle of a bigger floor', '#####\n#...#\n#...#\n#...#\n##G##', { x: 12, y: 64, z: 22 }],
  ['a pen with no floor marked has nowhere to stand', '###\n###\n###', null],
  ['nothing at all', '', null]
]) {
  test(`penInside: ${name}`, () => assert.deepEqual(penInside(cellsOf(plan)), expected))
}

// item 16 (Perrin): `pen.build place= partial=true` on a pen that is finished and holding failed with "no walkable
// path" instead of the guide's promised already=. The build walks to the MIDDLE of its plan first, to get the ground in
// sight before judging it - and the middle of a finished pen is inside the fence, with a shut gate between it and the
// body. Standing next to the pen is enough to read it, so the walk settles for that rather than failing the build.
const standingPen = () => ({
  '10,63,20': 'dirt', '11,63,20': 'dirt', '12,63,20': 'dirt', '10,63,21': 'dirt', '11,63,21': 'dirt', '12,63,21': 'dirt',
  '10,63,22': 'dirt', '11,63,22': 'dirt', '12,63,22': 'dirt',
  '10,64,20': 'oak_fence', '11,64,20': 'oak_fence', '12,64,20': 'oak_fence', '10,64,21': 'oak_fence',
  '11,64,21': 'air', '12,64,21': 'oak_fence', '10,64,22': 'oak_fence', '11,64,22': 'oak_fence_gate', '12,64,22': 'oak_fence'
})

test('pen.build: a finished pen with no way in says already=, it does not fail on the walk', async () => {
  const { api, calls } = fakeApi({
    place: { ...fakePlace('###\n#.#\n#G#', 10, 63, 20), kind: 'pen' }, world: standingPen(), items: { oak_fence: 20 },
    answers: {
      goto: ({ range }) => range > 2 ? {} : new Error('no walkable path (walks don\'t dig or bridge): look for a way round'),
      'pen.check': { pen: 'holds', cells: 1, inside: 'sheep:2' }
    }
  })
  const summary = await buildPen.run(api, { place: 'test-pen', partial: true })
  assert.deepEqual([summary.already, summary.pen, summary.inside],
    ['everything the plan asks for is already there', 'holds', 'sheep:2'])
  assert.deepEqual(calls.slice(0, 2), ['goto x=11 y=64 z=21 range=2', 'goto x=11 y=64 z=21 range=8'])
})

test('pen.build: a plan it cannot get near at all is still refused', async () => {
  const { api } = fakeApi({
    place: { ...fakePlace('###\n#.#\n#G#', 10, 63, 20), kind: 'pen' }, world: {}, items: { oak_fence: 20 },
    answers: { goto: () => new Error('no walkable path (walks don\'t dig or bridge): look for a way round') }
  })
  await assert.rejects(buildPen.run(api, { place: 'test-pen', partial: true }), /too far to see/)
})

test('pen.build: builds the plan, then says it holds', async () => {
  const world = { '10,63,20': 'dirt', '11,63,20': 'dirt', '12,63,20': 'dirt', '10,63,21': 'dirt', '11,63,21': 'dirt', '12,63,21': 'dirt', '10,63,22': 'dirt', '11,63,22': 'dirt', '12,63,22': 'dirt' }
  const { api, calls } = fakeApi({
    place: { ...fakePlace('###\n#.#\n#G#', 10, 63, 20), kind: 'pen' }, world, items: { oak_fence: 20, oak_fence_gate: 2 },
    answers: {
      place: ({ item, x, y, z }) => { world[`${x},${y},${z}`] = item; return {} },
      'pen.check': { pen: 'holds', cells: 1, sheep: 0 }
    }
  })
  const summary = await buildPen.run(api, { place: 'test-pen' })
  assert.deepEqual([summary.built, summary.pen, calls.at(-1)], [8, 'holds', 'pen.check 11,64,21'])
})

// a pen that leaks is not built: an animal walks straight out of it, so the composite fails instead of reporting done
test('pen.build: it refuses to call a leaking pen finished', async () => {
  const world = { '10,63,20': 'dirt', '11,63,20': 'dirt', '12,63,20': 'dirt', '10,63,21': 'dirt', '11,63,21': 'dirt', '12,63,21': 'dirt', '10,63,22': 'dirt', '11,63,22': 'dirt', '12,63,22': 'dirt' }
  const { api } = fakeApi({
    place: { ...fakePlace('###\n#.#\n#G#', 10, 63, 20), kind: 'pen' }, world, items: { oak_fence: 20, oak_fence_gate: 2 },
    answers: {
      place: ({ item, x, y, z }) => { world[`${x},${y},${z}`] = item; return {} },
      'pen.check': { pen: 'LEAKS', via: '11,64,22', advice: 'shut the gate' }
    }
  })
  await assert.rejects(buildPen.run(api, { place: 'test-pen' }), /leaks via 11,64,22/)
})

// A build fills and digs before it places anything, and every one of those jobs takes a floor or a wall apart while the
// list runs. Chani ran pen.build over a pen with 4 sheep in it (BUGS.md 2026-09-23 02:55Z) and all four walked out
// through the gap. Placing only adds, so a plan with nothing to fill or clear may still be built over a full pen.
for (const [name, jobs, census, expected] of [
  ['a floor to lay inside a pen that holds sheep', [{ do: 'fill' }], { inside: 'sheep:4' }, /holds sheep:4/],
  ['a block to dig out of one', [{ do: 'clear' }], { inside: 'cow:2' }, /holds cow:2/],
  ['what the build would do is named', [{ do: 'fill' }, { do: 'fill' }, { do: 'clear' }], { inside: 'sheep:4' }, /2 cells to fill and 1 to clear/],
  ['the way out is named', [{ do: 'fill' }], { inside: 'sheep:4' }, /flock\.lead/],
  ['nothing to fill or clear: the build only adds', [{ do: 'place' }, { do: 'plant' }], { inside: 'sheep:4' }, null],
  ['an empty pen', [{ do: 'fill' }], { cells: 16 }, null],
  ['no pen there at all', [{ do: 'fill' }], null, null]
]) {
  test(`penOpenRefusal: ${name}`, () => {
    const refusal = penOpenRefusal('chani-sheep-pen', jobs, census)
    assert.equal(expected === null, refusal === null)
    if (expected) assert.match(refusal, expected)
  })
}

// where pen.check is asked whether a pen stands around the plan: over the plan's floor cells, and one level lower too,
// because a pen whose floor is sunk one below its plan is exactly the case this guards
for (const [name, plan, expected] of [
  ['over the middle floor cell, and one down', '###\n#.#\n#G#', [{ x: 11, y: 64, z: 21 }, { x: 11, y: 63, z: 21 }]],
  ['a pen with no floor marked has nowhere to probe', '###\n###\n###', []],
  ['nothing at all', '', []]
]) {
  test(`penProbes: ${name}`, () => assert.deepEqual(penProbes(cellsOf(plan), 1), expected))
}

test('pen.build: a pen with animals in it is not opened up', async () => {
  const world = {
    '10,63,20': 'dirt', '11,63,20': 'dirt', '12,63,20': 'dirt', '10,63,21': 'dirt', '11,63,21': 'air', '12,63,21': 'dirt',
    '10,63,22': 'dirt', '11,63,22': 'dirt', '12,63,22': 'dirt'
  }
  const { api, calls } = fakeApi({
    place: { ...fakePlace('###\n#.#\n#G#', 10, 63, 20), name: 'test-pen', kind: 'pen' },
    world,
    items: { oak_fence: 20, oak_fence_gate: 2, dirt: 8 },
    answers: { 'pen.check': { pen: 'holds', cells: 4, inside: 'sheep:4' } }
  })
  await assert.rejects(buildPen.run(api, { place: 'test-pen' }), /test-pen holds sheep:4 and the build would open it/)
  assert.deepEqual(calls.filter(c => c.startsWith('place') || c.startsWith('dig')), [], 'not a block is moved')
})

// the probe is a question, not a step: a cell that is no spot to stand on simply is not a pen, and the build goes on.
// Each probe names its own cell, so the runner's "failed twice in a row" rule never sees a repeat and hands back
test('pen.build: a pen.check that finds nowhere to stand does not stop the build', async () => {
  const world = {
    '10,63,20': 'dirt', '11,63,20': 'dirt', '12,63,20': 'dirt', '10,63,21': 'dirt', '11,63,21': 'air', '12,63,21': 'dirt',
    '10,63,22': 'dirt', '11,63,22': 'dirt', '12,63,22': 'dirt'
  }
  const { api, calls } = fakeApi({
    place: { ...fakePlace('###\n#.#\n#G#', 10, 63, 20), name: 'test-pen', kind: 'pen' },
    world,
    items: { oak_fence: 20, oak_fence_gate: 2, dirt: 8 },
    answers: {
      place: ({ item, x, y, z }) => { world[`${x},${y},${z}`] = item; return {} },
      'pen.check': at => at.y === 64 && at.x === 11 && at.z === 21 && world['11,63,21'] === 'dirt'
        ? { pen: 'holds', cells: 1 }
        : new Error(`${at.x},${at.y},${at.z} is not a spot to stand on`)
    }
  })
  const summary = await buildPen.run(api, { place: 'test-pen' })
  assert.deepEqual([summary.levelled, summary.built, summary.pen], [1, 8, 'holds'])
  assert.deepEqual(calls.filter(c => c.startsWith('pen.check')).slice(0, 2), ['pen.check 11,64,21', 'pen.check 11,63,21'])
})

test('pen.build: a pen that holds animals and needs nothing levelled is still built', async () => {
  const world = { '10,63,20': 'dirt', '11,63,20': 'dirt', '12,63,20': 'dirt', '10,63,21': 'dirt', '11,63,21': 'dirt', '12,63,21': 'dirt', '10,63,22': 'dirt', '11,63,22': 'dirt', '12,63,22': 'dirt' }
  const { api, calls } = fakeApi({
    place: { ...fakePlace('###\n#.#\n#G#', 10, 63, 20), name: 'test-pen', kind: 'pen' },
    world,
    items: { oak_fence: 20, oak_fence_gate: 2 },
    answers: {
      place: ({ item, x, y, z }) => { world[`${x},${y},${z}`] = item; return {} },
      'pen.check': { pen: 'holds', cells: 4, inside: 'sheep:4' }
    }
  })
  const summary = await buildPen.run(api, { place: 'test-pen' })
  assert.deepEqual([summary.built, summary.pen], [8, 'holds'])
  assert.equal(calls.filter(c => c.startsWith('place')).length, 8)
})

// ---------------------------------------------------------------- flock.bring_pair
for (const [name, inside, mob, expected] of [
  ['one kind in the pen', 'cow:2', 'cow', 2],
  ['several kinds', 'cow:2 sheep:1', 'sheep', 1],
  ['a kind that is not in there', 'cow:2 sheep:1', 'pig', 0],
  ['an empty pen', undefined, 'cow', 0],
  ['a name that starts the same', 'mooshroom:3', 'moo', 0]
]) {
  test(`insideCount: ${name}`, () => assert.equal(insideCount(inside, mob), expected))
}

for (const [name, args, expected] of [
  ['a pair from nothing', { mob: 'cow', inside: 0, grown: 3 }, { fetch: 2 }],
  ['one more to make the pair', { mob: 'cow', inside: 1, grown: 2 }, { fetch: 1 }],
  ['exactly as many as I can see', { mob: 'cow', inside: 0, grown: 2 }, { fetch: 2 }],
  ['a bigger flock asked for', { mob: 'sheep', inside: 1, grown: 5, want: 4 }, { fetch: 3 }],
  ['the pair is already in there', { mob: 'cow', inside: 2, grown: 4 }, { fetch: 0, note: 'the pen already holds 2 grown cow' }],
  ['more than asked for is still enough', { mob: 'cow', inside: 3, grown: 4 }, { fetch: 0, note: 'the pen already holds 3 grown cow' }],
  ['only one to be had', { mob: 'cow', inside: 0, grown: 1 },
    { fetch: 0, refuse: 'breeding takes two: the pen holds no grown cow and there is 1 within reach to fetch. Look further afield (within=), or bring one in by hand' }],
  ['none at all', { mob: 'cow', inside: 1, grown: 0 },
    { fetch: 0, refuse: 'breeding takes two: the pen holds 1 grown cow and there is nothing within reach to fetch. Look further afield (within=), or bring one in by hand' }]
]) {
  test(`pairPlan: ${name}`, () => assert.deepEqual(pairPlan(args), expected))
}

for (const [name, places, args, expected] of [
  ['a place by name', [{ name: 'paddock', x: 1.7, y: 64, z: -3.2 }], { place: 'paddock' }, { at: { x: 1, y: 64, z: -4 } }],
  ['plain coordinates', [], { x: 5, y: 64, z: 6 }, { at: { x: 5, y: 64, z: 6 } }],
  ['a place nobody marked', [], { place: 'nowhere' }, { error: 'no place called nowhere: places lists them' }],
  ['nothing to aim at', [], {}, { error: 'flock.lead needs place=<name> or x= y= z=' }],
  ['half a coordinate', [], { x: 5, z: 6 }, { error: 'flock.lead needs place=<name> or x= y= z=' }]
]) {
  test(`placeTarget: ${name}`, () => assert.deepEqual(placeTarget(places, args, 'flock.lead'), expected))
}

const penAt = { x: 10, y: 64, z: 20 }
const bringApi = (checks, found, led = {}, world = { '10,64,20': 'air' }) => fakeApi({
  items: { wheat: 8 }, places: [{ name: 'paddock', ...penAt }], world: { '12,64,22': 'oak_fence_gate#open', ...world },
  answers: {
    'pen.check': () => checks.shift(),
    animals: { found },
    'flock.lead': { arrived: true, with: 2, inside: 'cow:2', ...led }
  }
})

test('flock.bring_pair: a pen it cannot see is walked to before anything is counted', async () => {
  const { api, calls } = bringApi(
    [{ pen: 'holds', cells: 20 }, { pen: 'holds', cells: 20, inside: 'cow:2' }],
    [{ mob: 'cow', id: 1, grown: true, inMyPen: false }, { mob: 'cow', id: 2, grown: true, inMyPen: false }],
    {}, {})
  const summary = await bringPair.run(api, { mob: 'cow', place: 'paddock' })
  assert.equal(calls[0], 'goto x=10 y=64 z=20 range=3', 'the walk comes first')
  assert.equal(calls[1], 'pen.check 10,64,20', 'and only then is the pen read')
  assert.equal(summary.walked, '10,64,20: the pen was not in sight from where I stood')
})

test('flock.bring_pair: a pen already in sight is not walked to', async () => {
  const { api, calls } = bringApi(
    [{ pen: 'holds', cells: 20 }, { pen: 'holds', cells: 20, inside: 'cow:2' }],
    [{ mob: 'cow', id: 1, grown: true, inMyPen: false }, { mob: 'cow', id: 2, grown: true, inMyPen: false }])
  const summary = await bringPair.run(api, { mob: 'cow', place: 'paddock' })
  assert.equal(calls[0], 'pen.check 10,64,20')
  assert.equal(summary.walked, undefined)
})

test('flock.bring_pair: fetches the pair and says the pen holds them', async () => {
  const { api, calls } = bringApi(
    [{ pen: 'holds', cells: 20 }, { pen: 'holds', cells: 20, inside: 'cow:2' }],
    [{ mob: 'cow', id: 1, grown: true, inMyPen: false }, { mob: 'cow', id: 2, grown: true, inMyPen: false }])
  const summary = await bringPair.run(api, { mob: 'cow', place: 'paddock' })
  assert.deepEqual([summary.inside, summary.fetched, calls[1], calls[2]],
    ['cow:2', 2, 'animals mob=cow within=32 x=10 y=64 z=20', 'flock.lead mob=cow count=2 x=10 y=64 z=20'])
})

test('flock.bring_pair: fetches only the one the pen still needs', async () => {
  const { api, calls } = bringApi(
    [{ pen: 'holds', cells: 20, inside: 'cow:1' }, { pen: 'holds', cells: 20, inside: 'cow:2' }],
    [{ mob: 'cow', id: 1, grown: true, inMyPen: false }])
  await bringPair.run(api, { mob: 'cow', place: 'paddock' })
  assert.equal(calls[2], 'flock.lead mob=cow count=1 x=10 y=64 z=20')
})

test('flock.bring_pair: a pen that already holds a pair is left in peace', async () => {
  const { api, calls } = bringApi([{ pen: 'holds', cells: 20, inside: 'cow:2' }], [])
  const summary = await bringPair.run(api, { mob: 'cow', place: 'paddock' })
  assert.deepEqual([summary.already, calls.filter(c => c.startsWith('flock.lead'))], ['the pen already holds 2 grown cow', []])
})

test('flock.bring_pair: one animal is not a pair, and it says so before walking anywhere', async () => {
  const { api, calls } = bringApi([{ pen: 'holds', cells: 20 }], [{ mob: 'cow', id: 1, grown: true, inMyPen: false }])
  await assert.rejects(bringPair.run(api, { mob: 'cow', place: 'paddock' }), /breeding takes two/)
  assert.deepEqual(calls.filter(c => c.startsWith('flock.lead')), [])
})

// calves do not breed, and one already in the pen is not one to fetch
test('flock.bring_pair: calves and the ones already inside are not counted as fetchable', async () => {
  const { api } = bringApi([{ pen: 'holds', cells: 20 }],
    [{ mob: 'cow', id: 1, grown: false, inMyPen: false }, { mob: 'cow', id: 2, grown: true, inMyPen: true }, { mob: 'cow', id: 3, grown: true, inMyPen: false }])
  await assert.rejects(bringPair.run(api, { mob: 'cow', place: 'paddock' }), /there is 1 within reach/)
})

test('flock.bring_pair: an empty hand is refused before the walk', async () => {
  const { api, calls } = fakeApi({ places: [{ name: 'paddock', ...penAt }], items: { dirt: 3 } })
  await assert.rejects(bringPair.run(api, { mob: 'cow', place: 'paddock' }), /a cow follows wheat: you carry none/)
  assert.deepEqual(calls, [])
})

// the gate we walked in through stands open until somebody shuts it, and an open gate makes the whole pen read as open country
test('flock.bring_pair: it shuts the gate that leaks before counting who is inside', async () => {
  const { api, calls } = bringApi(
    [{ pen: 'holds', cells: 20 }, { pen: 'LEAKS', via: '12,64,22' }, { pen: 'holds', cells: 20, inside: 'cow:2' }],
    [{ mob: 'cow', id: 1, grown: true, inMyPen: false }, { mob: 'cow', id: 2, grown: true, inMyPen: false }])
  const summary = await bringPair.run(api, { mob: 'cow', place: 'paddock' })
  assert.deepEqual([calls.filter(c => c.startsWith('toggle')), summary.inside], [['toggle x=12 y=64 z=22'], 'cow:2'])
})

// ---------------------------------------------------------------- flock.maintain
for (const [name, args, expected] of [
  ['a pair and room to grow', { mob: 'cow', grown: 2, young: 0, size: 4 }, { do: 'breed', why: '2 cow of 4' }],
  ['calves count towards the size', { mob: 'cow', grown: 2, young: 1, size: 4 }, { do: 'breed', why: '3 cow of 4' }],
  ['one animal cannot breed', { mob: 'cow', grown: 1, young: 0, size: 4 }, { do: 'wait', why: '1 cow of 4, and breeding takes two grown ones' }],
  ['a full pen is left alone', { mob: 'cow', grown: 4, young: 0, size: 4 }, { do: 'nothing', why: '4 cow of 4' }],
  ['over size, culled back to it', { mob: 'cow', grown: 6, young: 0, size: 4 }, { do: 'cull', count: 2, why: '6 cow of 4' }],
  ['never culled below a breeding pair', { mob: 'cow', grown: 3, young: 3, size: 1 }, { do: 'cull', count: 1, why: '6 cow of 1' }],
  ['calves are not culled, so a pen of calves waits', { mob: 'cow', grown: 2, young: 4, size: 4 }, { do: 'nothing', why: '6 cow of 4' }],
  ['cull=false only grows', { mob: 'cow', grown: 6, young: 0, size: 4, cull: false }, { do: 'nothing', why: '6 cow of 4' }]
]) {
  test(`flockPlan: ${name}`, () => assert.deepEqual(flockPlan(args), expected))
}

for (const [name, items, expected] of [
  ['wool is all surplus', { white_wool: 12, black_wool: 1 }, { white_wool: 12, black_wool: 1 }],
  ['meat above what I eat', { mutton: 12 }, { mutton: 4 }],
  ['meat I would want to eat is kept', { cooked_mutton: 5 }, {}],
  ['what the flock never produced is left alone', { wheat: 40, oak_log: 3 }, {}],
  ['feathers, leather and eggs go in', { feather: 6, leather: 2, egg: 3 }, { feather: 6, leather: 2, egg: 3 }]
]) {
  test(`flockSurplus: ${name}`, () => assert.deepEqual(flockSurplus(items), expected))
}

const flockApi = (found, items = {}, answers = {}) => fakeApi({
  items, places: [{ name: 'paddock', ...penAt }],
  answers: { animals: { found }, shear: { tried: 1 }, collect: {}, ...answers }
})

test('flock.maintain: breeds while the flock is under size', async () => {
  const { api, calls } = flockApi([
    { mob: 'cow', id: 1, grown: true, inMyPen: true }, { mob: 'cow', id: 2, grown: true, inMyPen: true },
    { mob: 'cow', id: 3, grown: true, inMyPen: false }
  ], { wheat: 8 })
  const summary = await flockMaintain.run(api, { mob: 'cow', place: 'paddock', size: 4 })
  assert.deepEqual([summary.bred, summary.flock, calls.includes('flock.breed mob=cow within=24')], [1, '2 cow of 4', true])
})

test('flock.maintain: one grown animal is not a pair, and it says so', async () => {
  const { api, calls } = flockApi([{ mob: 'cow', id: 1, grown: true, inMyPen: true }], { wheat: 8 })
  const summary = await flockMaintain.run(api, { mob: 'cow', place: 'paddock', size: 4 })
  assert.deepEqual([summary.short, calls.filter(c => c.startsWith('flock.breed'))], ['1 cow of 4, and breeding takes two grown ones', []])
})

test('flock.maintain: culls the ones over the size it was given, by id', async () => {
  const { api, calls } = flockApi([
    { mob: 'cow', id: 1, grown: true, inMyPen: true }, { mob: 'cow', id: 2, grown: true, inMyPen: true },
    { mob: 'cow', id: 3, grown: true, inMyPen: true }, { mob: 'cow', id: 4, grown: true, inMyPen: true }
  ], { wheat: 8 })
  const summary = await flockMaintain.run(api, { mob: 'cow', place: 'paddock', size: 2 })
  assert.deepEqual([summary.culled, calls.filter(c => c.startsWith('attack'))], [2, ['attack mob=cow id=1', 'attack mob=cow id=2']])
})

test('flock.maintain: shears the sheep and puts the wool in the pen chest', async () => {
  const { api, calls } = flockApi(
    [{ mob: 'sheep', id: 1, grown: true, inMyPen: true }, { mob: 'sheep', id: 2, grown: true, inMyPen: true }],
    { wheat: 8, white_wool: 6 })
  const summary = await flockMaintain.run(api, { mob: 'sheep', place: 'paddock', size: 2 })
  assert.deepEqual([summary.sheared, calls.filter(c => c.startsWith('shear'))], [1, ['shear within=8']])
})

// a cow is not a sheep: nothing to shear, and the shears stay in the bag
test('flock.maintain: only sheep are sheared', async () => {
  const { api, calls } = flockApi([{ mob: 'cow', id: 1, grown: true, inMyPen: true }, { mob: 'cow', id: 2, grown: true, inMyPen: true }], { wheat: 8 })
  await flockMaintain.run(api, { mob: 'cow', place: 'paddock', size: 2 })
  assert.deepEqual(calls.filter(c => c.startsWith('shear')), [])
})

// a sheep was sheared inside a pen whose census said 0: the gate we walked in through was still open, and an open gate
// makes the whole pen read as open country (09-22)
test('flock.maintain: shuts the gate it walked through before counting the flock', async () => {
  const { api, calls } = fakeApi({
    items: { wheat: 8 }, places: [{ name: 'paddock', ...penAt }], world: { '12,64,22': 'oak_fence_gate#open' },
    answers: {
      'pen.check': () => ({ pen: 'LEAKS', via: '12,64,22' }),
      animals: { found: [{ mob: 'cow', id: 1, grown: true, inMyPen: true }, { mob: 'cow', id: 2, grown: true, inMyPen: true }] },
      collect: {}
    }
  })
  await flockMaintain.run(api, { mob: 'cow', place: 'paddock', size: 4 })
  assert.deepEqual(calls.slice(0, 4), ['goto x=10 y=64 z=20 range=1', 'pen.check 10,64,20', 'toggle x=12 y=64 z=22', 'pen.check 10,64,20'])
})

// a flock that was sheared yesterday has no wool today: normal, and not something to report as stuck
test('flock.maintain: a sheep with no wool yet is not a failure', async () => {
  const { api } = flockApi([{ mob: 'sheep', id: 1, grown: true, inMyPen: true }, { mob: 'sheep', id: 2, grown: true, inMyPen: true }],
    { wheat: 8 }, { shear: new Error('flock.maintain/shear: no sheep with wool within 8 blocks') })
  const summary = await flockMaintain.run(api, { mob: 'sheep', place: 'paddock', size: 2 })
  assert.deepEqual([summary.sheared, summary.stuck], [0, undefined])
})

// anything else that goes wrong while shearing is worth saying out loud
test('flock.maintain: shears that fail for another reason are reported', async () => {
  const { api } = flockApi([{ mob: 'sheep', id: 1, grown: true, inMyPen: true }, { mob: 'sheep', id: 2, grown: true, inMyPen: true }],
    { wheat: 8 }, { shear: new Error('flock.maintain/shear: you carry no shears') })
  const summary = await flockMaintain.run(api, { mob: 'sheep', place: 'paddock', size: 2 })
  assert.equal(summary.stuck, 'flock.maintain/shear: you carry no shears')
})

// ---------------------------------------------------------------- the client jar and its block textures
// textures/ is not checked in: tools/textures.mjs extracts it from a client jar when a body starts without it
for (const [name, dirs, expected] of [
  ['newest first, and numerically: 1.10 is newer than 1.9', ['1.9', '1.10', '1.21.8'], ['1.21.8', '1.10', '1.9']],
  ['a bare version is older than the same version with a patch', ['1.19', '1.19.2'], ['1.19.2', '1.19']],
  ['OptiFine builds are somebody else\'s jar', ['1.16.5-OptiFine_HD_U_G8', '1.16.5'], ['1.16.5']],
  ['pre-releases and release candidates are not releases', ['1.21.4-pre1', '1.21.4-rc3', '1.21.4'], ['1.21.4']],
  ['loader folders hold no client jar of their own', ['fabric-loader-0.16.7-1.21.1', 'iris-fabric-loader-0.16.7-1.21.1', '1.21.1'], ['1.21.1']],
  ['snapshots are skipped', ['23w31a', '1.21'], ['1.21']],
  ['nothing usable', ['fabric-loader-0.16.7-1.21.1'], []],
  ['no versions installed at all', [], []]
]) {
  test(`clientVersions: ${name}`, () => assert.deepEqual(clientVersions(dirs), expected))
}

for (const [name, entries, expected] of [
  ['a block texture is taken', ['assets/minecraft/textures/block/dirt.png'], ['assets/minecraft/textures/block/dirt.png']],
  ['items, entities and the gui are not blocks', ['assets/minecraft/textures/item/apple.png', 'assets/minecraft/textures/entity/creeper.png', 'assets/minecraft/textures/gui/bars.png'], []],
  ['animation metadata is not a texture', ['assets/minecraft/textures/block/water_still.png.mcmeta'], []],
  ['the folder entry itself is not a texture', ['assets/minecraft/textures/block/'], []],
  ['another namespace is not ours', ['assets/create/textures/block/andesite.png'], []],
  ['the order of the jar is kept', ['assets/minecraft/textures/block/stone.png', 'pack.mcmeta', 'assets/minecraft/textures/block/dirt.png'],
    ['assets/minecraft/textures/block/stone.png', 'assets/minecraft/textures/block/dirt.png']]
]) {
  test(`blockTextures: ${name}`, () => assert.deepEqual(blockTextures(entries), expected))
}
