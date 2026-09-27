import { hasPlan, parsePlacePlan } from './plan.mjs'
// Named places: parsing/matching them, marking one, and the refusals around asking for or working at a place.

import { within } from './world.mjs'
const awayFrom = from => p => Math.round(Math.hypot(p.x - from.x, p.y - from.y, p.z - from.z))
const holds = (text, want) => String(text ?? '').toLowerCase().includes(String(want).toLowerCase())

// The search behind `places`: every marked point that matches, nearest first. places.json is shared by every body and
// passed 60 entries in a fortnight, so nobody should ever read it whole - q= (name or note), by=, kind= and within= are
// how you find one. Separate from describePlaces so the caller can say how many it did not show.
export const matchPlaces = (places, from, { q, by, kind, within = Infinity, maxDist = within } = {}) => {
  const dist = awayFrom(from)
  return places
    .filter(p => (!kind || p.kind === kind) && (!by || holds(p.by, by)) && (!q || holds(p.name, q) || holds(p.note, q)) && dist(p) <= maxDist)
    .sort((a, b) => dist(a) - dist(b))
}

// Shared points of interest, nearest first: "name kind 12m @x,y,z (who: note)".
// A place belongs to whoever made it. `mark` used to stamp the marker's own name on every save, so appending one line
// to Chani's carrot patch took the patch over, and there is no `by=` to give it back (backlog #141). The same call cut
// the note to 80 characters in silence, dropping the half that said what was owed, and the only way to find out was to
// read the place back. Both are one mistake: a write that quietly changes what it was not asked to change. So the
// owner survives every later mark, and a note that does not fit is refused out loud with nothing saved.
export const NOTE_MAX = 80
// The shared map is a RECORD, and `mark` REPLACES what is saved under a name while `unmark` deletes the entry
// outright: one agent could move somebody else's field, overwrite their plan or wipe the entry, and neither asked
// anything at all. Adding to the NOTE stays open - that is how agents leave each other word about a place, and
// markFields has kept the owner through it since #141 - so only the substance is gated: the plan, where it is, what
// it is. An invitation on the ground does not open even that: "anyone welcome, harvest and replant" is permission to
// work the crop and says nothing about rewriting the entry that describes it. So ownership alone decides here, where
// workRefusal opens on the note.
export const mapRefusal = (saved, me) => {
  if (!saved?.by || String(saved.by).toLowerCase() === String(me ?? '').toLowerCase()) return null
  return `${saved.name} is on the shared map as ${saved.by}'s, and this is their own record of it: the plan, where it is and what it is are theirs to change or take off the map. Save yours under a name of your own, or ask ${saved.by} in chat to change theirs. Adding to its note= is still open to you, and working the ground is a different question (the note on it answers that one)`
}

export function markFields ({ saved, by, note }) {
  const text = String(note ?? saved?.note ?? '')
  if (text.length > NOTE_MAX) return { error: `note= is ${text.length} characters and a place note holds ${NOTE_MAX}: shorten it. Nothing was marked` }
  return { by: saved?.by ?? by, note: text }
}

// Where a mark puts a place. A note-only mark moved Chani's carrot patch to the feet of whoever left word on it, twice
// (BUGS.md 09-24 12:42Z): an existing place keeps its saved anchor unless x= says otherwise or a fresh map= is laid
// where I stand. A move is said out loud (`moved=`), and a move off a plan that still stands where it was (`stands`,
// from planStands) is refused unless move=true says it is meant.
export function markMove ({ saved, args = {}, here, stands = false }) {
  const cell = p => ({ x: Math.floor(p.x), y: Math.floor(p.y), z: Math.floor(p.z) })
  const given = args.x !== undefined
  if (!saved || saved.x === undefined) return { at: cell(given ? args : here) }
  const old = cell(saved)
  const at = given ? cell(args) : args.map !== undefined ? cell(here) : old
  const key = p => `${p.x},${p.y},${p.z}`
  if (key(at) === key(old)) return { at }
  const moved = `${key(old)} -> ${key(at)}`
  if (stands && !args.move) {
    return { error: `${saved.name} still stands where it is marked, at ${key(old)}: marking it at ${key(at)} would move the place off what is built. Pass move=true if that is what you mean, or mark a new name. Nothing was marked` }
  }
  return { at, moved }
}

// A mark is its owner speaking, and `anyone welcome, harvest and replant` is permission written down. Honouring it is
// the whole point of writing it, so this is the ONE question every composite asks before it digs, plants or carries
// away on ground somebody else marked - one rule, one wording, one place to change it (#144).
// The words are few and plain on purpose: a long list of near-synonyms turns a description of a farm ("wheat, harvest
// rounds weekly") into an invitation to strip it. A note that plainly withholds permission vetoes them all, because
// "ask first" written beside "harvest" is not an invitation, and the safe way to be wrong here is to refuse.
export const INVITE_WORDS = ['welcome', 'anyone', 'take', 'harvest']
const HOLDS_BACK = /\bask (me |us )?first\b|\bdo not\b|\bdon'?t\b|\bprivate\b/i
export const invited = note => {
  const said = String(note ?? '')
  if (HOLDS_BACK.test(said)) return false
  return INVITE_WORDS.some(word => new RegExp(`\\b${word}`, 'i').test(said))
}

// null when the work may go ahead, else the refusal - which names the place, its owner, the note exactly as they wrote
// it, and what would change the answer. An unsigned place is nobody's and my own is mine, whatever the note says.
// Nothing that only READS may call this: a body that may not look at a farm cannot plan work on it or answer a
// question about it, and test/lib.test.mjs holds every read-only action to that.
export function workRefusal (place, me) {
  if (!place?.by || String(place.by).toLowerCase() === String(me ?? '').toLowerCase()) return null
  if (invited(place.note)) return null
  return `${place.name} is ${place.by}'s ground and the note on it does not invite work: "${place.note ?? ''}". Ask ${place.by} in chat and leave it alone until they answer. It opens by itself when the note says one of: ${INVITE_WORDS.join(', ')}`
}

// the same question asked by name, for the composites that resolve a place through placeTarget rather than api.plan:
// apiary.inspect shares that resolver and only READS, so the gate cannot live in it (#144) and its writing siblings
// ask here instead. No place asked for, or a name nobody has marked, is nobody's ground and nobody's business.
export const placeRefusal = (places, name, me) => {
  const place = name ? (places ?? []).find(p => p.name === name) : null
  return place ? workRefusal(place, me) : null
}

export function describePlaces (places, from, options = {}) {
  const { limit = 12, notes = true } = options
  const dist = awayFrom(from)
  return matchPlaces(places, from, options)
    .slice(0, limit)
    .map(p => `${p.name} ${p.kind} ${dist(p)}m @${p.x},${p.y},${p.z}${notes ? ` (${p.by}${p.note ? `: ${p.note}` : ''})` : ''}`)
}

// One marked place, whole: what `places name=` answers. A plan is reported by its size, never printed - farm.plan name= prints it.
export const describePlace = (places, name, from) => {
  const place = places.find(p => p.name === name)
  if (!place) return null
  const parsed = hasPlan(place) ? parsePlacePlan(place) : null
  return {
    name: place.name,
    kind: place.kind,
    at: `${place.x},${place.y},${place.z}`,
    away: `${awayFrom(from)(place)}m`,
    by: place.by,
    ...(place.note ? { note: place.note } : {}),
    ...(parsed && !parsed.error ? { plan: `${parsed.width}x${parsed.maxY - parsed.minY + 1}x${parsed.height}` } : {})
  }
}

export function coordsError (a, withY = true) {
  const axes = withY ? ['x', 'y', 'z'] : ['x', 'z']
  if (!axes.every(k => typeof a[k] === 'number' && Number.isFinite(a[k]))) return `${withY ? 'x, y and z' : 'x and z'} must be numbers (got ${axes.map(k => `${k}=${a[k]}`).join(' ')})`
  return withY && (a.y < -64 || a.y > 319) ? `y=${a.y} is outside the world (-64 to 319): x and y swapped?` : null
}

// place=<name> or x= y= z=: the cell an action was pointed at, floored, or why it is not one
export function placeTarget (places, a, who) {
  const place = a.place ? places.find(p => p.name === a.place) : null
  if (a.place && !place) return { error: `no place called ${a.place}: places lists them` }
  const to = place ?? a
  if (to.x === undefined || to.y === undefined || to.z === undefined) return { error: `${who} needs place=<name> or x= y= z=` }
  return { at: { x: Math.floor(to.x), y: Math.floor(to.y), z: Math.floor(to.z) } }
}
