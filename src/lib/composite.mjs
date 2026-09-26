// The composite/routine runner's pure parts: argument checking, pause/hand-back text, and step parsing.

// wraps an action's arguments and remembers which were read, so a reply can name the ones that were not (a misspelt name, mostly)
export function trackReads (given) {
  const read = new Set()
  const args = new Proxy(given, { get: (target, key) => { read.add(key); return target[key] } })
  return { args, given, unread: () => Object.keys(given).filter(key => !read.has(key)) }
}
// after a failure only keys the action's source never mentions are blamed: it may have failed before it got to read the others
export function ignoredParams (unread, ok, source) {
  const ignored = unread.filter(key => ok || !new RegExp(`\\b${key}\\b`).test(source))
  return ignored.length ? `${ignored.join(' ')} (not a parameter here, or not used this time: check the name in the guide)` : null
}

// ---------------------------------------------------------------- composite actions: the runner's rules
// A composite declares the key=values it takes, so a typo is refused before the body walks anywhere.
// These belong to the runner and every composite accepts them.
const RUNNER_ARGS = { timeout: 'number', force: 'boolean', dig: 'boolean' }
export function checkArgs (name, spec, given) {
  const full = { ...RUNNER_ARGS, ...spec }
  const wanted = Object.entries(spec).map(([k, t]) => String(t).endsWith('!') ? k : `${k}?`).join(', ')
  const missing = Object.entries(spec).find(([k, t]) => String(t).endsWith('!') && given[k] === undefined)
  if (missing) return `${name} needs ${missing[0]}=`
  // a composite forwards its own optional arguments unset (range: a.range): that is no argument at all, not a wrong one
  for (const [key, value] of Object.entries(given).filter(([, v]) => v !== undefined)) {
    const type = full[key]
    if (!type) return `${name}: ${key}= is not an argument here (${wanted})`
    const want = String(type).replace('!', '')
    if (want !== 'any' && typeof value !== want) return `${name}: ${key}= wants a ${want}, got ${JSON.stringify(value)}`
  }
  return null
}

// A composite cannot opt out of these: the runner checks them between steps and hands control back to the driver, ending
// the task with stopped=<reason>. Night WITH a bed is not here: the runner sleeps and the composite never sees it.
// what `state` says the body is doing. A composite asleep at its night checkpoint read as a wedge (Chani stopped two):
// it is paused, and it goes on at dawn by itself
export const PAUSES = { night: 'asleep for the night, goes on at dawn by itself (./mc stop takes the body back now)' }
export const doingText = ({ name, seconds, paused } = {}) =>
  name ? `${name} ${seconds}s${paused ? `, paused: ${PAUSES[paused] ?? paused}` : ''}` : null

export function handBackReason (s) {
  if (s.spoken) return `spoken to (${s.spoken})`
  if (s.health <= 8) return `health ${Math.round(s.health)}`
  if (s.food <= 6 && !s.edible) return `food ${s.food} and nothing edible carried`
  if (s.failedTwice) return `twice in a row: ${s.failedTwice}`
  if (s.invFull && !s.canDeposit) return 'inventory full and no chest to deposit in'
  if (s.night && !s.bedNear) return 'night and no bed within 32 blocks'
  if (s.days !== undefined && s.elapsedDays >= s.days) return 'days'
  if (s.count !== undefined && s.done >= s.count) return 'count'
  if (s.until !== undefined && s.now >= s.until) return 'until'
  return null
}

// a library module must declare itself before the body will run it: a bad one is caught at body start, not mid-errand
const ARG_TYPES = ['string', 'number', 'boolean', 'any']
export function compositeError (name, mod) {
  if (typeof mod?.run !== 'function') return `library/${name}.mjs: needs \`run\``
  if (typeof mod.doc !== 'string' || !mod.doc) return `library/${name}.mjs: needs \`doc\` (its one line in the guide)`
  if (!mod.args || typeof mod.args !== 'object') return `library/${name}.mjs: needs \`args\` ({} when it takes none)`
  const bad = Object.entries(mod.args).find(([, t]) => !ARG_TYPES.includes(String(t).replace('!', '')))
  return bad ? `library/${name}.mjs: args.${bad[0]} is ${JSON.stringify(bad[1])}; use ${ARG_TYPES.join(', ')} (with ! for required)` : null
}

// ---------------------------------------------------------------- routines
// A routine is a plain list of {action, ...args}: handed over as steps=, or shipped by a role as roles/<role>/<name>.json
// so that "the farmer's day" is a file anyone can read and edit rather than something baked into an action.
const parseSteps = text => {
  try { return { list: JSON.parse(text) } } catch (e) { return { error: e.message } }
}

// a routine a role ships cannot know which farm it will be run on, so it writes $place and routine place= fills it in
// $place is filled from place=; $places with EVERY place the routine runs over (a,b,c as given: farm.maintain's
// reserve_for= keeps the seed of the whole homestead); $store from store= (where the produce goes: a chest cell or a
// marked storage place, deposit= in the steps); any other $name from vars= (a JSON object on the command
// line), and one nobody gave is left out of the step so the action's own default holds (farm.maintain's compost=
// falls back to the plan's K cell)
const varName = v => typeof v === 'string' && /^\$\w+$/.test(v) ? v.slice(1) : null
const fillVars = (step, vars) => Object.fromEntries(Object.entries(step).filter(([, v]) => !varName(v) || vars[varName(v)] !== undefined).map(([k, v]) => [k, varName(v) ? vars[varName(v)] : v]))
const wantsPlace = steps => steps.some(step => Object.values(step).includes('$place'))
const readVars = vars => {
  if (vars === undefined) return {}
  const parsed = typeof vars === 'string' ? parseSteps(vars) : { list: vars }
  const ok = !parsed.error && parsed.list && typeof parsed.list === 'object' && !Array.isArray(parsed.list)
  return ok ? parsed.list : { error: 'vars= must be an object like {"compost":"shared-composter"}' }
}

export function routineSteps ({ steps, name, place, places, store, vars }, readRole) {
  if (!steps && !name) return { error: 'routine needs steps= or name= (a routine shipped in roles/<role>/<name>.json)' }
  const where = steps ? 'steps= must be a list of {"action":...} objects' : `roles/${name}.json is not a list of steps`
  const raw = steps ?? readRole(name)
  if (raw === null || raw === undefined) return { error: `no routine called ${name} (roles/${name}.json)` }
  const read = typeof raw === 'string' ? parseSteps(raw) : { list: raw }
  if (read.error) return { error: `${where}: ${read.error}` }
  if (!Array.isArray(read.list)) return { error: where }
  const bad = read.list.findIndex(step => !step || !step.action)
  if (bad >= 0) return { error: `step ${bad + 1} has no action=` }
  if (wantsPlace(read.list) && !place) return { error: `routine name=${name} needs place=<the name of a marked farm> to work on` }
  const given = readVars(vars)
  if (given.error) return { error: given.error }
  return { steps: read.list.map(step => fillVars(step, { ...given, place, places, ...(store !== undefined ? { store } : {}) })) }
}

// A composite's `until`: look, wait `every` seconds of ticks, look again, give up after `timeout` seconds OF WAITING.
// Counted in the rounds waited and never on the wall clock: darkstar suspends when idle, and a 46-minute freeze (09-24,
// 00:13Z) made a Date.now() deadline fail every routine with "the day never ended" over a night in which nothing
// happened. Ticks stop when the machine does, so a wait counted in ticks simply resumes with it.
export const makeUntil = ({ waitTicks, alive, composite }) => async (pred, opts = {}) => {
  const timeout = opts.timeout ?? 60
  const every = opts.every ?? 1
  for (let waited = 0; ; waited += every) {
    alive()
    if (await pred()) return true
    if (waited >= timeout) throw new Error(`${composite}: waited ${timeout}s and ${opts.what ?? 'it never happened'}`)
    await waitTicks(Math.max(1, Math.round(every * 20)))
  }
}
