import test from 'node:test'
import assert from 'node:assert/strict'
import { executeFlow, executeLegacySteps, evaluateFlowCondition, parseFlowEDN, resolveFlowAction, validateFlow } from '../src/flow.mjs'
import { parseCliArgs } from '../src/cli.mjs'

const actionNames = ['goto', 'toggle', 'say']
const observationNames = ['block_at', 'entity', 'state']

test('flow accepts only bounded tagged sequences, safe observations, and existing leaf actions', () => {
  const program = ['seq',
    ['action', 'goto', { x: 1, y: 64, z: 2 }],
    ['when', ['eq', ['read', 'block_at', { x: 2, y: 64, z: 2 }, 'properties.open'], true], 10,
      ['action', 'toggle', { x: 2, y: 64, z: 2, open: false }]]]
  const valid = validateFlow(program, { actions: actionNames, observations: observationNames })
  assert.equal(valid.nodes, 7)
  for (const bad of [
    ['seq'],
    ['when', ['eq', 1, 1], 0, ['action', 'say', {}]],
    ['when', ['read', 'quit', {}, 'ok'], 1, ['action', 'say', {}]],
    ['when', ['eq', 1, 1], 1, ['action', 'run', { steps: [] }]],
    ['any', ['when', ['eq', 1, 1], 1, ['action', 'say', {}]]],
    ['seq', ['when', ['eq', 1, 1], 4000, ['action', 'say', {}]]]
  ]) assert.throws(() => validateFlow(bad, { actions: actionNames, observations: observationNames }), /flow:/)
  assert.throws(() => validateFlow(['action', 'say', { message: 'x'.repeat(200) }], {
    actions: actionNames, observations: observationNames, limits: { bytes: 64 }
  }), /program exceeds 64 bytes/)
})

test('flow parses one complete EDN form with keyword names, maps, vectors, and comparisons', async () => {
  const source = '(seq (action :goto {:x -77 :y 69 :z -41}) (when (= (read :block_at {:x -73 :y 69 :z -38} [:properties :open]) true) 60 (action :toggle {:x -73 :y 69 :z -38 :open false})))'
  const program = parseFlowEDN(source)
  assert.deepEqual(program, ['seq', ['action', 'goto', { x: -77, y: 69, z: -41 }], ['when', ['eq', ['read', 'block_at', { x: -73, y: 69, z: -38 }, 'properties.open'], true], 60, ['action', 'toggle', { x: -73, y: 69, z: -38, open: false }]]])
  const calls = []
  await executeFlow(program, {
    actions: actionNames, observations: observationNames,
    act: async (name, args) => { calls.push([name, args]); return {} },
    observe: async () => ({ properties: { open: true } }),
    waitTicks: async () => {}
  })
  assert.deepEqual(calls.map(([name]) => name), ['goto', 'toggle'])
  for (const bad of [
    `${source} (action :say {})`,
    `${source} trailing`,
    '(action :say {:bad (not-data)})',
    '(action :say {:duplicate 1 :duplicate 2})',
    '(action :say {:bad #{:unsafe}})',
    '(action :say {:bad #js [1]})',
    '(action :say {:bad unsupported-symbol})',
    '(action :say {:bad "unterminated})',
    '(action :say {:bad "unknown\\q"})'
  ]) assert.throws(() => parseFlowEDN(bad), /flow:/)
  assert.equal(parseCliArgs([`steps=${source}`]).steps, source, 'CLI keeps proper EDN text intact for the EDN parser')
  const vectorArgs = parseFlowEDN('(action :say {:items [:cow :pig]})')
  assert.deepEqual(vectorArgs, ['action', 'say', { items: ['cow', 'pig'] }])
  assert.doesNotThrow(() => validateFlow(vectorArgs, { actions: actionNames, observations: observationNames }))
  assert.deepEqual(parseFlowEDN('(action :say {:map {:nested true}})'), ['action', 'say', { map: { nested: true } }])
  const protoArgs = parseFlowEDN('(action :say {:__proto__ {:polluted true}})')[2]
  assert.equal(Object.hasOwn(protoArgs, '__proto__'), true)
  assert.equal({}.polluted, undefined)
  assert.deepEqual(parseFlowEDN('(when (contains [:eq :other] :eq) 1 (action :say {}))'), ['when', ['contains', ['literal', ['eq', 'other']], 'eq'], 1, ['action', 'say', {}]])
  assert.throws(() => parseFlowEDN(`(action :say {:deep ${'['.repeat(70)}0${']'.repeat(70)}})`), /EDN nesting exceeds/)
  assert.deepEqual(parseFlowEDN(String.raw`(action :say {:message "snowman \u2603"})`), ['action', 'say', { message: 'snowman ☃' }])
  assert.deepEqual(parseFlowEDN(String.raw`(action :say {:message "a\bb\fc\t"})`), ['action', 'say', { message: 'a\bb\fc\t' }])
  assert.deepEqual(parseFlowEDN('; leading comment\n(action :say {}) ; trailing comment'), ['action', 'say', {}])
})

test('flow dispatch invokes existing long composites and quick leaves without recursing into run', async () => {
  const calls = []
  const composite = args => { calls.push(['farm.harvest', args]); return 'harvest' }
  const quick = args => { calls.push(['block_at', args]); return 'block' }
  assert.equal(resolveFlowAction('farm.harvest', { 'farm.harvest': composite }, { 'farm.harvest': quick }), composite)
  assert.equal(resolveFlowAction('block_at', {}, { block_at: quick }), quick)
  assert.equal(resolveFlowAction('run', { run: composite }, {}), null)
  assert.equal(resolveFlowAction('missing', {}, {}), null)
  const long = { 'farm.harvest': composite }
  const quickActions = { block_at: quick }
  await executeFlow(parseFlowEDN('(seq (action :farm.harvest {:crop :wheat}) (action :block_at {:x 2 :y 64 :z 3}))'), {
    actions: ['farm.harvest', 'block_at'], observations: [],
    act: async (name, args) => resolveFlowAction(name, long, quickActions)(args),
    observe: async () => undefined, waitTicks: async () => {}
  })
  assert.deepEqual(calls, [['farm.harvest', { crop: 'wheat' }], ['block_at', { x: 2, y: 64, z: 3 }]])
})

test('legacy object lists share action execution but preserve their response shape, empty list, and unbounded length', async () => {
  const calls = []
  const host = {
    actions: actionNames, observations: observationNames,
    act: async (name, args) => { calls.push([name, args]); return name === 'say' ? { text: 'said' } : { name: 'grass_block', properties: {} } },
    observe: async () => undefined, waitTicks: async () => {}
  }
  const result = await executeLegacySteps([
    { action: 'say', message: 'hello' },
    { action: 'block_at', x: 2, y: 64, z: 3 }
  ], host)
  assert.deepEqual(calls, [['say', { message: 'hello' }], ['block_at', { x: 2, y: 64, z: 3 }]])
  assert.deepEqual(result, { results: [
    { action: 'say', text: 'said' },
    { action: 'block_at', name: 'grass_block', properties: {} }
  ] })
  assert.deepEqual(await executeLegacySteps([], host), { results: [] })
  calls.length = 0
  const longList = Array.from({ length: 70 }, (_, i) => ({ action: 'say', message: String(i) }))
  assert.equal((await executeLegacySteps(longList, host)).results.length, 70, 'legacy lists do not inherit EDN action limits')
  assert.equal(calls.length, 70)
})

test('legacy errors retain step number/action text and stop later actions', async () => {
  const calls = []
  await assert.rejects(executeLegacySteps([
    { action: 'say', message: 'before' },
    { action: 'missing' },
    { action: 'say', message: 'must not run' }
  ], {
    act: async (name, args, step) => {
      calls.push([name, args, step])
      if (name === 'missing') throw new Error(`step ${step.index}/${step.total} (${name}): unknown action`)
      return { text: 'ok' }
    }, observe: async () => undefined, waitTicks: async () => {}
  }), /step 2\/3 \(missing\): unknown action/)
  assert.deepEqual(calls.map(([name]) => name), ['say', 'missing'])
})

test('condition operators preserve unknown observations through negation', async () => {
  const never = async () => undefined
  assert.equal(await evaluateFlowCondition(['not', ['eq', ['read', 'state', {}, 'missing'], 'x']], { observe: never, observations: observationNames }), null)
  assert.equal(await evaluateFlowCondition(['and', ['eq', 2, 2], ['gte', 4, 3]], { observe: never }), true)
  assert.equal(await evaluateFlowCondition(['or', ['contains', ['read', 'entity', { name: 'cow' }, 'found'], 'cow'], ['truthy', false]], {
    observe: async () => ({ found: ['cow'] }), observations: observationNames
  }), true)
  let calls = 0
  assert.equal(await evaluateFlowCondition(['and', false, ['eq', ['read', 'state', {}, 'hp'], 0]], {
    observe: async () => { calls++; return { hp: 0 } }, observations: observationNames
  }), false)
  assert.equal(calls, 0, 'boolean operators short circuit unnecessary observations')
  assert.equal(await evaluateFlowCondition(['contains', ['literal', ['cow', 'pig']], 'pig'], { observe: never }), true)
  assert.equal(await evaluateFlowCondition(['eq', ['read', 'state', {}, 'pos'], ['literal', { x: 1, y: 2 }]], {
    observe: async () => ({ pos: { y: 2, x: 1 } }), observations: observationNames
  }), true)
})

test('seq executes normal actions in order and when rechecks before dispatch', async () => {
  const calls = [], observations = []
  let reads = 0, ticks = 0, clock = 0
  const program = ['seq',
    ['action', 'goto', { x: 2, y: 64, z: 4 }],
    ['when', ['eq', ['read', 'block_at', { x: 1, y: 64, z: 1 }, 'properties.open'], true], 4,
      ['action', 'toggle', { x: 1, y: 64, z: 1, open: false }]]]
  const result = await executeFlow(program, {
    actions: actionNames, observations: observationNames,
    act: async (name, args) => { calls.push([name, args]); return { ok: true } },
    observe: async () => { observations.push(++reads); return { properties: { open: reads >= 3 } } },
    waitTicks: async n => { ticks += n; clock += n * 50 }, now: () => clock,
    pollSeconds: 0.25
  })
  assert.deepEqual(calls.map(x => x[0]), ['goto', 'toggle'])
  assert.equal(observations.length, 4, 'two false samples, readiness sample and independent pre-action recheck')
  assert.equal(ticks, 10)
  assert.equal(result.actions, 2)
  assert.equal(result.waited, 0.5)
})

test('any samples competing conditions in order and runs exactly one ready branch', async () => {
  const calls = [], reads = []
  let ticks = 0
  const condition = (x, yes) => ['eq', ['read', 'block_at', { x, y: 0, z: 0 }, 'name'], yes ? 'open' : 'closed']
  const program = ['any',
    ['when', condition(1, true), 5, ['action', 'toggle', { x: 1, open: false }]],
    ['when', condition(2, true), 5, ['action', 'say', { message: 'second' }]]]
  const result = await executeFlow(program, {
    actions: actionNames, observations: observationNames,
    act: async (name, args) => { calls.push([name, args]); return {} },
    observe: async (_name, args) => { reads.push(args.x); return { name: 'open' } },
    waitTicks: async n => { ticks += n },
    pollSeconds: 0.25
  })
  assert.deepEqual(calls.map(c => c[0]), ['toggle'])
  assert.deepEqual(reads, [1, 1], 'both branches share a sample, then the selected condition is refreshed before acting')
  assert.equal(ticks, 0)
  assert.equal(result.actions, 1)
})

test('any does not run a stale winner if it turns false before action and can choose the next branch', async () => {
  const calls = [], values = new Map([[1, 'open'], [2, 'open']])
  const readOpen = x => ['eq', ['read', 'block_at', { x }, 'name'], 'open']
  const program = ['any',
    ['when', readOpen(1), 2, ['action', 'toggle', { x: 1 }]],
    ['when', readOpen(2), 2, ['action', 'say', { message: 'second' }]]]
  let x1Reads = 0
  await executeFlow(program, {
    actions: actionNames, observations: observationNames,
    act: async name => { calls.push(name); return {} },
    observe: async (_name, args) => {
      if (args.x === 1 && ++x1Reads === 2) values.set(1, 'closed')
      return { name: values.get(args.x) }
    },
    waitTicks: async () => {}, pollSeconds: 0.25
  })
  assert.deepEqual(calls, ['say'])
})

test('when bodies compose seq and any, preserving first-ready branch order', async () => {
  const program = parseFlowEDN('(when (= (read :state {} [:ready]) true) 5 (seq (action :goto {:x 1}) (any (when (= (read :block_at {:x 2} [:name]) :open) 2 (seq (action :say {:message "winner-1"}) (action :toggle {:x 2 :open false}))) (when (= (read :block_at {:x 3} [:name]) :open) 2 (action :say {:message "winner-2"})))))')
  const calls = []
  await executeFlow(program, {
    actions: actionNames, observations: observationNames,
    act: async (name, args) => { calls.push([name, args]); return {} },
    observe: async name => name === 'state' ? { ready: true } : { name: 'open' },
    waitTicks: async () => {}
  })
  assert.deepEqual(calls, [
    ['goto', { x: 1 }],
    ['say', { message: 'winner-1' }],
    ['toggle', { x: 2, open: false }]
  ])
})

test('timeout and cancellation before readiness dispatch no actions', async () => {
  let actionCount = 0, ticks = 0, clock = 0
  const node = ['when', ['eq', ['read', 'block_at', { x: 0 }, 'name'], 'open'], 0.5, ['action', 'toggle', { x: 0 }]]
  await assert.rejects(executeFlow(node, {
    actions: actionNames, observations: observationNames,
    act: async () => { actionCount++ }, observe: async () => ({ name: 'closed' }),
    waitTicks: async n => { ticks += n; clock += n * 50 }, now: () => clock, pollSeconds: 0.25
  }), /condition timed out.*no action ran/)
  assert.equal(actionCount, 0)
  let cancelled = false
  await assert.rejects(executeFlow(node, {
    actions: actionNames, observations: observationNames,
    act: async () => { actionCount++ }, observe: async () => ({ name: 'closed' }),
    waitTicks: async n => { cancelled = true; clock += n * 50 }, now: () => clock,
    alive: () => { if (cancelled) throw new Error('cancelled') }, pollSeconds: 0.25
  }), /cancelled/)
  assert.equal(actionCount, 0)
})

test('timeouts count slow observations and never run a condition that became true after its deadline', async () => {
  let clock = 0, actions = 0, reads = 0
  const program = ['when', ['eq', ['read', 'state', {}, 'ready'], true], 0.1, ['action', 'say', {}]]
  await assert.rejects(executeFlow(program, {
    actions: actionNames, observations: observationNames,
    act: async () => { actions++ },
    observe: async () => {
      reads++
      if (reads === 1) return { ready: false }
      clock += 60 // the condition lookup itself overshoots the remaining deadline
      return { ready: true }
    },
    waitTicks: async n => { clock += n * 50 }, now: () => clock, pollSeconds: 0.25
  }), /condition timed out.*no action ran/)
  assert.equal(reads, 2)
  assert.equal(actions, 0)
})

test('each wait starts at sequence entry while the total flow budget includes prior actions', async () => {
  let clock = 0
  const calls = []
  const program = ['seq', ['action', 'goto', {}], ['when', ['eq', ['read', 'state', {}, 'ready'], true], 5, ['action', 'say', {}]]]
  await executeFlow(program, {
    actions: actionNames, observations: observationNames,
    act: async name => { calls.push(name); if (name === 'goto') clock += 10_000 },
    observe: async () => ({ ready: true }), waitTicks: async n => { clock += n * 50 }, now: () => clock
  })
  assert.deepEqual(calls, ['goto', 'say'], 'the nested condition gets a fresh five-second deadline')

  clock = 0
  calls.length = 0
  await assert.rejects(executeFlow(program, {
    actions: actionNames, observations: observationNames, limits: { waitSeconds: 5 },
    act: async name => { calls.push(name); if (name === 'goto') clock += 6_000 },
    observe: async () => ({ ready: true }), waitTicks: async n => { clock += n * 50 }, now: () => clock
  }), /total flow time limit 5s/)
  assert.deepEqual(calls, ['goto'], 'the global budget prevents every subsequent action after an overlong leaf')
})

test('unknown block observations never satisfy not, action failures stop the sequence', async () => {
  let actions = 0, clock = 0
  const unknownNot = ['when', ['not', ['truthy', ['read', 'block_at', { x: 1 }, 'name']]], 0.5, ['action', 'say', {}]]
  await assert.rejects(executeFlow(unknownNot, {
    actions: actionNames, observations: observationNames,
    act: async () => { actions++ }, observe: async () => ({ name: null }),
    waitTicks: async n => { clock += n * 50 }, now: () => clock, pollSeconds: 0.25
  }), /condition timed out/)
  await assert.rejects(executeFlow(['seq', ['action', 'goto', {}], ['action', 'say', {}]], {
    actions: actionNames, observations: observationNames,
    act: async () => { actions++; throw new Error('action failed') }, observe: async () => ({}), waitTicks: async () => {}
  }), /action failed/)
  assert.equal(actions, 1)
})
