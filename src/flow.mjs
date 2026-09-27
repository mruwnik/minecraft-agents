// Small, bounded EDN command flow. EDN is parsed as data, never evaluated;
// world reads and actions are delegated to explicit host callbacks.
import { performance } from 'node:perf_hooks'
import ednDataParser from 'edn-data'

const { EDNListParser } = ednDataParser
const UNKNOWN = Symbol('unknown observation')
const DEFAULT_LIMITS = Object.freeze({ bytes: 262144, nodes: 128, depth: 16, actions: 64, waitSeconds: 3600, pollSeconds: 0.25 })
const isObject = value => value !== null && typeof value === 'object' && !Array.isArray(value)
const fail = message => { throw new Error(`flow: ${message}`) }
const jsonValue = value => value === null || typeof value === 'string' || typeof value === 'boolean' || (typeof value === 'number' && Number.isFinite(value)) ||
  (Array.isArray(value) && value.every(jsonValue)) || (isObject(value) && Object.values(value).every(jsonValue))
const exactKeys = (value, keys, where) => {
  if (!isObject(value) || Object.keys(value).some(k => !keys.includes(k))) fail(`${where} has unsupported fields`)
}

const CONDITION_OPS = new Set(['read', 'literal', 'and', 'or', 'not', 'truthy', 'eq', 'ne', 'lt', 'lte', 'gt', 'gte', 'contains'])
const SYMBOL_OPS = Object.freeze({ '=': 'eq', 'not=': 'ne', '<': 'lt', '<=': 'lte', '>': 'gt', '>=': 'gte' })

function ednData (value) {
  if (value === null || typeof value === 'string' || typeof value === 'boolean' || (typeof value === 'number' && Number.isFinite(value))) return value
  if (Array.isArray(value)) return value.map(ednData)
  if (value instanceof Map) {
    const out = {}
    for (const [key, item] of value) {
      if (!key || typeof key !== 'object' || typeof key.key !== 'string') fail('EDN maps in flow data need keyword keys')
      Object.defineProperty(out, key.key, { value: ednData(item), enumerable: true, configurable: true, writable: true })
    }
    return out
  }
  if (value && typeof value === 'object' && typeof value.key === 'string') return value.key
  fail('flow EDN data supports only nil, booleans, finite numbers, strings, keywords, vectors, and keyword-keyed maps')
}

function ednExpression (form) {
  if (!form || typeof form !== 'object' || !Array.isArray(form.list) || !form.list.length) fail('flow forms must be non-empty EDN lists')
  const [head, ...raw] = form.list
  if (!head || typeof head.sym !== 'string') fail('flow list heads must be operators')
  const sourceOp = head.sym
  const op = SYMBOL_OPS[sourceOp] ?? sourceOp
  const expr = child => child && typeof child === 'object' && Array.isArray(child.list)
    ? ednExpression(child)
    : Array.isArray(child) ? ['literal', ednData(child)] : ednData(child)
  if (op === 'seq' || op === 'any') return [op, ...raw.map(ednExpression)]
  if (op === 'action') {
    if (raw.length !== 2 || !raw[0] || typeof raw[0].key !== 'string') fail('action syntax is (action :command {:arg value})')
    return ['action', raw[0].key, ednData(raw[1])]
  }
  if (op === 'when') {
    if (raw.length !== 3) fail('when syntax is (when condition seconds flow-node)')
    return ['when', expr(raw[0]), ednData(raw[1]), ednExpression(raw[2])]
  }
  if (op === 'read') {
    if (raw.length !== 3 || !raw[0] || typeof raw[0].key !== 'string' || !Array.isArray(raw[2])) fail('read syntax is (read :observation {:args ...} [:path :to-field])')
    const path = raw[2].map(part => {
      if (part && typeof part.key === 'string') return part.key
      if (typeof part === 'number' && Number.isInteger(part) && part >= 0) return String(part)
      fail('read path vectors contain only keywords and non-negative indexes')
    }).join('.')
    return ['read', raw[0].key, ednData(raw[1]), path]
  }
  if (CONDITION_OPS.has(op)) return [op, ...raw.map(expr)]
  fail(`unsupported flow operator ${JSON.stringify(sourceOp)}`)
}

// Parse full EDN input with the maintained EDN parser. The synthetic outer
// list lets us reject trailing forms instead of silently accepting only the
// first one (the package's convenience parser does that by design).
export function parseFlowEDN (source) {
  if (typeof source !== 'string' || Buffer.byteLength(source) > DEFAULT_LIMITS.bytes) fail(`EDN program exceeds ${DEFAULT_LIMITS.bytes} bytes or is not text`)
  // edn-data correctly supports EDN's string escapes; fail closed on anything
  // outside that set instead of letting an unknown escape become "undefined".
  let inString = false
  for (let i = 0; i < source.length; i++) {
    if (!inString && source[i] === ';') { while (i < source.length && source[i] !== '\n') i++; continue }
    if (inString && source[i] === '\\') {
      const next = source[++i]
      if (!['t', 'r', 'n', 'b', 'f', 'u', '\\', '"'].includes(next)) fail('invalid EDN string escape')
      continue
    }
    if (source[i] === '"') inString = !inString
  }
  let forms
  try {
    const parser = new EDNListParser({ mapAs: 'map', setAs: 'object', keywordAs: 'object', charAs: 'object', listAs: 'object' })
    forms = parser.next(`(${source}\n)`)
    if (!parser.isDone()) fail('incomplete EDN input')
  } catch (error) {
    if (String(error?.message).startsWith('flow:')) throw error
    fail(`invalid EDN: ${error?.message ?? error}`)
  }
  if (!Array.isArray(forms) || forms.length !== 1) fail('expected exactly one EDN flow form')
  const pending = [[forms[0], 0]]
  while (pending.length) {
    const [value, depth] = pending.pop()
    if (depth > 64) fail('EDN nesting exceeds 64 levels')
    if (Array.isArray(value)) value.forEach(child => pending.push([child, depth + 1]))
    else if (value instanceof Map) for (const [key, child] of value) pending.push([key, depth + 1], [child, depth + 1])
    else if (value && typeof value === 'object') Object.values(value).forEach(child => pending.push([child, depth + 1]))
  }
  return ednExpression(forms[0])
}

export function resolveFlowAction (name, long = {}, quick = {}) {
  return name === 'run' ? null : long[name] ?? quick[name] ?? null
}

// The original object-list API remains a separate syntax, normalized into the
// same sequential executor. It deliberately does not receive EDN's limits or
// grammar; callers keep its historical unbounded list behavior.
export function normalizeLegacySteps (steps) {
  if (!Array.isArray(steps)) fail('legacy steps must be a list')
  return ['seq', ...steps.map(({ action, ...args }) => ['action', action, args])]
}

function validateCondition (expr, env, depth, seen, counter) {
  counter.nodes++
  if (counter.nodes > env.limits.nodes) fail(`program exceeds ${env.limits.nodes} nodes`)
  if (depth > env.limits.depth) fail(`program exceeds nesting depth ${env.limits.depth}`)
  if (!Array.isArray(expr) || !CONDITION_OPS.has(expr[0])) {
    if (!jsonValue(expr)) fail('condition literals must be JSON values')
    return
  }
  if (seen.has(expr)) fail('program cannot contain cyclic arrays')
  seen.add(expr)
  const [op, ...args] = expr
  const arity = (min, max = min) => { if (args.length < min || args.length > max) fail(`${String(op)} condition has wrong number of operands`) }
  if (op === 'read') {
    arity(3)
    const [name, readArgs, path] = args
    if (typeof name !== 'string' || !env.observations.has(name)) fail(`unknown or disallowed observation ${JSON.stringify(name)}`)
    if (!isObject(readArgs) || !jsonValue(readArgs)) fail(`read ${name} needs a JSON args object`)
    if (typeof path !== 'string' || !/^[A-Za-z][A-Za-z0-9]*(\.(?:[A-Za-z][A-Za-z0-9]*|\d+))*$/.test(path) || path.split('.').some(p => ['__proto__', 'prototype', 'constructor'].includes(p))) fail('read path must be a safe dotted field path')
    return
  }
  if (op === 'literal') { arity(1); if (!jsonValue(args[0])) fail('literal must be a JSON value'); return }
  if (['and', 'or'].includes(op)) {
    arity(2, 16); args.forEach(x => validateCondition(x, env, depth + 1, seen, counter)); return
  }
  if (op === 'not' || op === 'truthy') { arity(1); validateCondition(args[0], env, depth + 1, seen, counter); return }
  if (['eq', 'ne', 'lt', 'lte', 'gt', 'gte', 'contains'].includes(op)) {
    arity(2); args.forEach(x => validateCondition(x, env, depth + 1, seen, counter)); return
  }
  fail(`unsupported condition operator ${JSON.stringify(op)}`)
}

function validateAction (node, env, depth, seen, counter, counted = false, seenAlready = false) {
  if (!counted) {
    counter.nodes++
    if (counter.nodes > env.limits.nodes) fail(`program exceeds ${env.limits.nodes} nodes`)
  }
  if (depth > env.limits.depth) fail(`program exceeds nesting depth ${env.limits.depth}`)
  if (!Array.isArray(node) || node[0] !== 'action' || node.length !== 3) fail('expected an action node')
  if (!seenAlready) {
    if (seen.has(node)) fail('program cannot contain cyclic arrays')
    seen.add(node)
  }
  const [, name, args] = node
  if (typeof name !== 'string' || name === 'run' || !env.actions.has(name)) fail(`unknown or disallowed action ${JSON.stringify(name)}`)
  if (!isObject(args) || !jsonValue(args)) fail(`action ${name} needs a JSON args object`)
}

function validateNode (node, env, depth, seen, counter) {
  counter.nodes++
  if (counter.nodes > env.limits.nodes) fail(`program exceeds ${env.limits.nodes} nodes`)
  if (depth > env.limits.depth) fail(`program exceeds nesting depth ${env.limits.depth}`)
  if (!Array.isArray(node)) fail('nodes must be tagged arrays')
  if (seen.has(node)) fail('program cannot contain cyclic arrays')
  seen.add(node)
  const [op, ...args] = node
  if (op === 'seq') {
    if (!args.length) fail('seq needs at least one node')
    args.forEach(child => validateNode(child, env, depth + 1, seen, counter))
    return
  }
  if (op === 'when') {
    if (args.length !== 3) fail('when syntax is ["when", condition, timeoutSeconds, flowNode]')
    const [condition, timeout, action] = args
    if (!Number.isFinite(timeout) || timeout <= 0 || timeout > env.limits.waitSeconds) fail(`when timeout must be between 0 and ${env.limits.waitSeconds} seconds`)
    validateCondition(condition, env, depth + 1, seen, counter)
    validateNode(action, env, depth + 1, seen, counter)
    return
  }
  if (op === 'any') {
    if (args.length < 2 || args.length > 16) fail('any needs 2..16 competing when branches')
    args.forEach(branch => {
      if (!Array.isArray(branch) || branch[0] !== 'when') fail('any accepts only when branches')
      validateNode(branch, env, depth + 1, seen, counter)
    })
    return
  }
  if (op === 'action') { validateAction(node, env, depth, seen, counter, true, true); return }
  fail(`unsupported flow node ${JSON.stringify(op)}`)
}

export function validateFlow (program, { actions = [], observations = [], limits = {} } = {}) {
  const env = {
    actions: new Set(actions),
    observations: new Set(observations),
    limits: { ...DEFAULT_LIMITS, ...limits }
  }
  let encoded
  try { encoded = JSON.stringify(program) } catch { fail('program must be finite JSON data') }
  if (Buffer.byteLength(encoded ?? '') > env.limits.bytes) fail(`program exceeds ${env.limits.bytes} bytes`)
  const counter = { nodes: 0 }
  validateNode(program, env, 0, new Set(), counter)
  return { program: structuredClone(program), nodes: counter.nodes, limits: env.limits }
}

const canonical = value => Array.isArray(value) ? value.map(canonical) : isObject(value) ? Object.fromEntries(Object.keys(value).sort().map(k => [k, canonical(value[k])])) : value
const stable = value => JSON.stringify(canonical(value))
const equal = (a, b) => Object.is(a, b) || (a !== null && b !== null && typeof a === 'object' && typeof b === 'object' && stable(a) === stable(b))
function pathValue (value, path) {
  for (const part of path.split('.')) {
    if (value === null || value === undefined || !Object.hasOwn(value, part)) return UNKNOWN
    value = value[part]
  }
  return value
}
async function valueOf (expr, env) {
  if (!Array.isArray(expr) || !CONDITION_OPS.has(expr[0])) return expr
  const [op, ...args] = expr
  if (op === 'read') {
    const [name, readArgs, path] = args
    const key = `${name}:${stable(readArgs)}`
    if (!env.cache.has(key)) env.cache.set(key, Promise.resolve(env.observe(name, structuredClone(readArgs))).then(value => {
      if (value === undefined || value === null || (name === 'block_at' && value.name === null)) return UNKNOWN
      return value
    }))
    return pathValue(await env.cache.get(key), path)
  }
  if (op === 'and' || op === 'or') {
    let unknown = false
    for (const expr of args) {
      const value = await valueOf(expr, env)
      if (value === UNKNOWN) { unknown = true; continue }
      if (op === 'and' && !value) return false
      if (op === 'or' && value) return true
    }
    return unknown ? UNKNOWN : op === 'and'
  }
  if (op === 'literal') return args[0]
  const values = []
  for (const arg of args) values.push(await valueOf(arg, env))
  if (values.includes(UNKNOWN)) return UNKNOWN
  if (op === 'not') return !values[0]
  if (op === 'truthy') return Boolean(values[0])
  const [a, b] = values
  if (op === 'eq') return equal(a, b)
  if (op === 'ne') return !equal(a, b)
  if (op === 'lt') return typeof a === 'number' && typeof b === 'number' && a < b
  if (op === 'lte') return typeof a === 'number' && typeof b === 'number' && a <= b
  if (op === 'gt') return typeof a === 'number' && typeof b === 'number' && a > b
  if (op === 'gte') return typeof a === 'number' && typeof b === 'number' && a >= b
  if (op === 'contains') return (typeof a === 'string' && typeof b === 'string' && a.includes(b)) || (Array.isArray(a) && a.some(x => equal(x, b)))
  return UNKNOWN
}

export async function evaluateFlowCondition (condition, { observe, cache = new Map(), observations = [] } = {}) {
  const env = { observe, cache, observations: new Set(observations) }
  const result = await valueOf(condition, env)
  return result === UNKNOWN ? null : result === true
}

// Execute conditions serially and actions one at a time. `any` polls every
// branch from one cached observation sample and picks the first ready branch.
export async function executeFlow (program, {
  act, observe, waitTicks, alive = () => {}, actions = [], observations = [],
  limits = {}, pollSeconds = DEFAULT_LIMITS.pollSeconds, now = () => performance.now(), legacy = false
} = {}) {
  const validated = legacy
    ? { program, limits: { ...DEFAULT_LIMITS, ...limits } }
    : validateFlow(program, { actions, observations, limits })
  if (typeof act !== 'function' || typeof observe !== 'function' || typeof waitTicks !== 'function' || typeof now !== 'function') fail('host needs act, observe, waitTicks and now callbacks')
  if (!Number.isFinite(pollSeconds) || pollSeconds <= 0 || pollSeconds > 5) fail('pollSeconds must be between 0 and 5')
  const maxActions = legacy ? Infinity : limits.actions ?? DEFAULT_LIMITS.actions
  const maxWait = legacy ? Infinity : limits.waitSeconds ?? DEFAULT_LIMITS.waitSeconds
  const flowStartedAt = now()
  let actionCount = 0, waitedTotal = 0
  const globalExpired = () => (now() - flowStartedAt) / 1000 > maxWait
  const runAction = async ([, name, args], legacyStep) => {
    alive()
    if (!legacy && globalExpired()) fail(`total flow time limit ${maxWait}s reached; no later action ran`)
    if (actionCount >= maxActions) fail(`action limit ${maxActions} reached`)
    actionCount++
    const result = await act(name, structuredClone(args), legacyStep)
    alive()
    return legacy ? { action: name, ...(result ?? {}) } : { action: name, result }
  }
  const awaitBranch = async branches => {
    const branchStartedAt = now(), expired = branches.map(() => false)
    const branchElapsed = () => (now() - branchStartedAt) / 1000
    for (;;) {
      alive()
      if (globalExpired()) fail(`total flow time limit ${maxWait}s reached; no later action ran`)
      const elapsed = branchElapsed()
      for (let i = 0; i < branches.length; i++) if (elapsed > branches[i][2]) expired[i] = true
      const cache = new Map()
      let winner = -1
      for (let i = 0; i < branches.length; i++) {
        if (expired[i]) continue
        const [, condition] = branches[i]
        if (globalExpired()) fail(`total flow time limit ${maxWait}s reached; no later action ran`)
        const ready = await evaluateFlowCondition(condition, { observe, cache, observations })
        const afterRead = branchElapsed()
        if (globalExpired()) fail(`total flow time limit ${maxWait}s reached; no later action ran`)
        if (afterRead > branches[i][2]) { expired[i] = true; continue }
        if (ready !== true) { if (afterRead >= branches[i][2]) expired[i] = true; continue }
        // Re-read the winner immediately before dispatch; if it ceased to be
        // true, keep waiting instead of firing from a stale sample.
        const confirmed = await evaluateFlowCondition(condition, { observe, cache: new Map(), observations }) === true
        const afterConfirm = branchElapsed()
        if (globalExpired()) fail(`total flow time limit ${maxWait}s reached; no later action ran`)
        if (afterConfirm > branches[i][2]) { expired[i] = true; continue }
        if (confirmed) { winner = i; break }
      }
      if (winner >= 0) return run(branches[winner][3])
      if (expired.every(Boolean)) {
        const labels = branches.map(([, , timeout]) => `${timeout}s`).join(', ')
        fail(`condition timed out after ${labels}; no action ran`)
      }
      // Poll on a server tick no later than the nearest still-live deadline;
      // otherwise a short timeout could wake late and run a newly-true action.
      let ticks = Math.max(1, Math.round(pollSeconds * 20))
      for (let i = 0; i < branches.length; i++) {
        if (expired[i]) continue
        const remainTicks = Math.floor((branches[i][2] - branchElapsed()) * 20 + 1e-9)
        if (remainTicks < 1) expired[i] = true
        else ticks = Math.min(ticks, remainTicks)
      }
      if (expired.every(Boolean)) {
        const labels = branches.map(([, , timeout]) => `${timeout}s`).join(', ')
        fail(`condition timed out after ${labels}; no action ran`)
      }
      if (globalExpired() || waitedTotal + ticks / 20 > maxWait) fail(`total flow time limit ${maxWait}s reached; no later action ran`)
      alive()
      const beforeWait = now()
      await waitTicks(ticks)
      const afterWait = now()
      // The branch deadline uses actual monotonic time, including server lag.
      // For hosts with a coarse clock, account at least the requested ticks.
      waitedTotal += Math.max(ticks / 20, Math.max(0, afterWait - beforeWait) / 1000)
    }
  }
  const run = async (node, legacyStep) => {
    alive()
    const [op, ...args] = node
    if (op === 'seq') {
      const results = []
      for (let i = 0; i < args.length; i++) {
        const childContext = legacy ? { index: i + 1, total: args.length, action: args[i][1] } : undefined
        results.push(await run(args[i], childContext))
      }
      return results
    }
    if (op === 'action') return runAction(node, legacyStep)
    if (op === 'when') return awaitBranch([node])
    if (op === 'any') return awaitBranch(args)
  }
  const result = await run(validated.program)
  return { result, actions: actionCount, waited: waitedTotal }
}

export async function executeLegacySteps (steps, host = {}) {
  if (!Array.isArray(steps)) fail('legacy steps must be a list')
  if (!steps.length) return { results: [] }
  const program = normalizeLegacySteps(steps)
  const { result } = await executeFlow(program, { ...host, legacy: true })
  return { results: result }
}

export const FLOW_LIMITS = DEFAULT_LIMITS
