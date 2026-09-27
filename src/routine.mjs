// A routine over several places. A farmstead is several fields 40 blocks apart (a crop field, a melon patch, a cane
// stand), and `routine name=farmer/homestead place=a,b,c` is the whole day: the role's routine expanded once per
// place, in the order given. The one-place call is the routine exactly as it was.
import { routineSteps, placeRefusal, compact } from './lib.mjs'
import { carriedOfKind } from './inventory/kit.mjs'
import { FARM_ISSUE_FIELDS, farmIssues } from './farm/attention.mjs'

// place=a,b,c as the CLI hands it over (one string), or a list already
export const placeList = place => (Array.isArray(place) ? place : String(place ?? '').split(','))
  .map(name => String(name).trim())
  .filter(Boolean)

// the day's steps: routineSteps once per place, in order. A routine with no $place in it run over several places would
// do the same chores N times, which is never what was meant, so that call is refused rather than repeated.
export function routinePlan (a, readRole) {
  const places = placeList(a.place)
  if (places.length === 0) {
    const { steps, error } = routineSteps({ ...a, place: undefined }, readRole)
    return error ? { error } : { places, steps }
  }
  const rounds = []
  for (const place of places) {
    const { steps, error } = routineSteps({ ...a, place, places: places.join(',') }, readRole)
    if (error) return { error }
    rounds.push(steps)
  }
  const same = places.length > 1 && JSON.stringify(rounds[0]) === JSON.stringify(rounds[1])
  if (same) return { error: `${a.name ? `roles/${a.name}.json` : 'steps='} has no $place to fill: place=${places.join(',')} would run the same steps ${places.length} times` }
  return { places, steps: rounds.flat() }
}

// A routine is days long, so it refuses before day one rather than failing the same way once a round (#144): a place
// nobody marked is named (every place asked for, so one call fixes them all), and somebody else's ground answers as it
// always does, wherever in the list it sits.
export function unmarkedPlaces (places, names) {
  const unknown = names.filter(name => !(places ?? []).some(p => p.name === name))
  return unknown.length ? `no place called ${unknown.join(', ')} on the shared map (places lists what is marked): a routine over places you name stops here rather than failing the same way once a round` : null
}

export function placesRefusal (places, names, me) {
  const unknown = unmarkedPlaces(places, names)
  if (unknown) return unknown
  for (const name of names) {
    const refusal = placeRefusal(places, name, me)
    if (refusal) return refusal
  }
  return null
}

// ---------------------------------------------------------------- autopilot (autopilot card)
// A routine on autopilot (days=0) runs with no driver reading its results, so every stop writes a routine_stopped
// event and every day a routine_day one: the driver that is spawned for it reads those, not the log.

// the step as the events name it: the action and the place it worked on
export const stepLabel = (action, args = {}) => `${action}${args.place ? ` place=${args.place}` : ''}`

// one sentence a driver can act on, for each way the runner ends a routine (handBackReason's words, `cancelled` from
// ./mc stop or a stall cancel, `days` from the routine itself)
export function stopAdvice (reason, days) {
  if (reason === 'days') return `the routine ran its ${days} day${days === 1 ? '' : 's'}: start it again (days=0 runs until stopped) or move on`
  if (reason === 'cancelled') return 'stopped from outside (./mc stop, or a stall or circling cancel: events type=task_cancelled last=1 says which): fix what it names and start the routine again'
  if (reason === 'until') return 'its until= minutes are up: start it again when there is time for another'
  if (/^health /.test(reason)) return 'the body is hurt: eat to food 18 and rest until health is back, then start the routine again'
  if (/^food /.test(reason)) return 'nothing edible carried: fetch or grow food (WORLD.md says where the shared food is), then start the routine again'
  if (/broke|tool|no (\w+_)?(pickaxe|axe|hoe|shovel|shears|sword)\b/i.test(reason)) return 'a tool broke or is missing: craft another and carry a spare, then start the routine again'
  if (/^twice in a row/.test(reason)) return 'the same step failed twice: run it by hand and read its FAIL, fix what it names, then start the routine again'
  if (/^night and no bed/.test(reason)) return 'put a bed within 32 blocks of the places, or mark your own bed (mark name=<you>-bed kind=bed, standing on it) or pass bed=<place> so the routine walks to it at nightfall when it is within bed_range (default 200) blocks; or quit for the night (./mc quit, then ./mc dawn); then start the routine again'
  if (/^spoken to/.test(reason)) return 'answer them in chat, then start the routine again'
  if (/^inventory full/.test(reason)) return 'deposit into a chest near the places (deposit=true in the farm steps does it), then start the routine again'
  return 'read the error, fix what it names, then start the routine again'
}

// bed: at a night stop, which bed the routine knew of and why it did not walk there (library/routine.mjs)
export const stopEvent = ({ reason, step = null, place = null, days, bed = null }) => ({ reason, step, place, advice: stopAdvice(reason, days), ...(bed ? { bed } : {}) })

// what each step reported, per place ("here" for a step with no place), short enough for one event line. What went
// wrong comes first and is never cut: on 09-26 a farm's stuck= and missing= stood behind lowSlabs= and clutter=, past
// the cut, and the day line read as a field maintained while its summary said otherwise
const SAID_FIRST = [...new Set(['stopped', 'stuck', 'missing', 'bare', 'storage_full', 'chest_missing', 'rekit', 'kit_short', 'unfinished', 'skipped', ...FARM_ISSUE_FIELDS])]
const OUTCOME_MAX = 120
const stoppedEarly = outcome => Boolean(outcome.stopped && !['done', 'days', 'count', 'until'].includes(outcome.stopped))
// Missing seed can recover as the remaining crops ripen. Report that work as incomplete without telling the
// stuck watch it failed. A blocked walk, early hand-back, or bare beds lacking a hoe needs intervention.
export const outcomeStalled = outcome => Boolean(outcome.failed || outcome.stuck || stoppedEarly(outcome) || /untilled:\d+ no hoe/.test(outcome.bare ?? ''))
export const outcomeStatus = outcome => outcome.failed ? 'FAILED'
  : outcomeStalled(outcome) || Object.keys(farmIssues(outcome)).length ? 'incomplete' : 'ok'
export function outcomeText (outcome) {
  if (outcome.failed) return `FAILED ${outcome.failed}${outcome.rekit ? ` (${outcome.rekit})` : ''}`
  const wrong = compact(Object.fromEntries(SAID_FIRST.filter(k => outcome[k] !== undefined).map(k => [k, outcome[k]])), false)
  const rest = compact(Object.fromEntries(Object.entries(outcome).filter(([k]) => !SAID_FIRST.includes(k))), false)
  const head = [outcomeStatus(outcome), wrong].filter(Boolean).join(' ')
  const room = OUTCOME_MAX - head.length - 1
  return rest && room > 0 ? `${head} ${rest.slice(0, room)}` : head
}
// night: one line on the night before this day when the routine walked to its bed for it
export function dayEvent (day, outcomes, night = null) {
  const places = {}
  for (const { action, place, outcome } of outcomes) {
    places[place ?? 'here'] = { ...places[place ?? 'here'], [action]: outcomeText(outcome) }
  }
  return { day, places, ...(night ? { night } : {}) }
}

// a step ended with fewer of a kind than it began (src/inventory/kit.mjs toolsLost) and the kit step ran again: which kinds are
// back in the pockets and which are not, in the words the day line carries ("hoe replaced", "hoe broke, no spare")
export function rekitVerdict (lost, items) {
  const replaced = lost.filter(kind => carriedOfKind(items, kind) > 0)
  const missing = lost.filter(kind => !replaced.includes(kind))
  return { replaced, missing, text: [...replaced.map(k => `${k} replaced`), ...missing.map(k => `${k} broke, no spare`)].join(', ') }
}

// the routine_bed_walk event for one leg, and the day summary's line on the night
export const posKey = p => `${Math.floor(p.x)},${Math.floor(p.y)},${Math.floor(p.z)}`
export const bedWalkEvent = ({ leg, bed, from, to, distance, seconds }) => ({ leg, bed: bed.name, from: posKey(from), to: posKey(to), distance, seconds })
export const nightLine = ({ bed, distance, seconds, back }) => `walked ${distance} blocks to ${bed.name} (${seconds}s), back at dawn${back === null ? ' failed' : ` (${back}s)`}`
