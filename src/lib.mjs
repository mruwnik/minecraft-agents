// Pure helpers, kept apart from bot.mjs so they can be tested without a server.
import { compact, between } from './cli.mjs'
import { WORK_RANGE } from './walk.mjs'
import { range, inAnyZone, within, isGroundCover, looksBuilt, FLUIDS, isAir, holdsWater, hasWaterSource, STEPS } from './lib/world.mjs'
import { ripeCrop } from './lib/farm.mjs'
import { PLAN_LEGEND, planLane } from './lib/plan.mjs'
import { sameFamily, WEEDS } from './lib/anchor.mjs'
export * from './cli.mjs'
export * from './players.mjs'
export * from './lib/world.mjs'
export * from './lib/agents.mjs'
export * from './lib/code.mjs'
export * from './lib/events.mjs'
export * from './lib/chests.mjs'
export * from './lib/craft.mjs'
export * from './lib/inventory.mjs'
export * from './lib/burrow.mjs'
export * from './lib/fight.mjs'
export * from './lib/death.mjs'
export * from './lib/wedge.mjs'
export * from './lib/fences.mjs'
export * from './lib/farm.mjs'
export * from './lib/collect.mjs'
export * from './lib/water.mjs'
export * from './lib/villager.mjs'
export * from './lib/ferry.mjs'
export * from './lib/composite.mjs'
export * from './lib/patches.mjs'
export * from './lib/jar.mjs'
export * from './lib/scan.mjs'
export * from './lib/places.mjs'
export * from './lib/path.mjs'
export * from './lib/place.mjs'
export * from './lib/animals.mjs'
export * from './lib/dig.mjs'
export * from './lib/sleep.mjs'
export * from './lib/food.mjs'
export * from './lib/flee.mjs'
export * from './lib/herd.mjs'
export * from './lib/plan.mjs'
export * from './lib/anchor.mjs'

// The actions that changed name when the library was namespaced. Journals, habits and old notes still say the left-hand
// side, so every "unknown action" names its successor rather than leaving the driver to guess. No aliases: the old name stays dead.
export const RENAMED = {
  harvest: 'farm.harvest',
  maintain_farm: 'farm.maintain',
  compost: 'farm.compost',
  get_seeds: 'farm.get_seeds',
  plan: 'farm.plan',
  fields: 'farm.fields',
  mine: 'mine.get',
  collect_items: 'collect',
  lead: 'flock.lead',
  breed: 'flock.breed',
  pen_check: 'pen.check'
}
// where to read about the successor: its section is the half before the dot, and a top-level one is its own topic
export const renamedTo = typed => RENAMED[typed] ? `it is now ${RENAMED[typed]} (./mc help ${RENAMED[typed].split('.')[0]})` : null
export const renamedList = () => `renamed: ${Object.entries(RENAMED).map(([was, now]) => `${was} -> ${now}`).join(', ')}`

// an action name nobody knows: the real ones that share a word with it
// words drivers reach for that share nothing with the real name
const OTHER_WORDS = { cancel: 'stop', abort: 'stop', halt: 'stop', nearby: 'look_around', entities: 'look_around', mobs: 'look_around', say: 'chat', walk: 'goto', move: 'goto', eat: 'eat', attack: 'attack', kill: 'attack', bed: 'sleep', open: 'toggle', close: 'toggle', store: 'deposit', take: 'withdraw', drop: 'toss', throw: 'toss' }
export function didYouMean (typed, actions) {
  if (RENAMED[typed]) return `unknown action ${typed}: ${renamedTo(typed)}`
  const words = typed.toLowerCase().split(/[^a-z]+/).filter(w => w.length >= 3)
  const meant = words.map(w => OTHER_WORDS[w]).filter(a => actions.includes(a))
  const like = [...new Set([...meant, ...actions.filter(a => words.some(w => a.split(/[_.]/).some(part => part.startsWith(w) || w.startsWith(part))))])]
  return `unknown action ${typed}: ${like.length ? `did you mean ${like.slice(0, 4).join(', ')}?` : './mc help lists them all'}`
}
const JOB_ORDER = ['skip', 'clear', 'till', 'pour', 'cover', 'plant', 'place']
// A bed tilled and left bare goes back to dirt: dry within minutes, and any of it the moment something jumps on it.
// A field tilled in one pass and sown in the next loses the beds the body walked back over (15 of 28, round 2 item 3),
// so every till is followed at once by the planting of its own cell, and the walk does each bed once.
const sowAsTilled = jobs => {
  const plants = new Map(jobs.filter(j => j.do === 'plant').map(j => [`${j.x},${j.y},${j.z}`, j]))
  const sown = new Set()
  const out = []
  for (const job of jobs) {
    if (job.do === 'plant' && sown.has(job)) continue
    out.push(job)
    const after = job.do === 'till' ? plants.get(`${job.x},${job.y + 1},${job.z}`) : null
    if (!after) continue
    out.push(after)
    sown.add(after)
  }
  return out
}

// what a hand-tilled bed has to be told, whether or not it has water: nothing keeps bare farmland
export const tillWarning = (dry, total) => dry
  ? `${dry} of ${total} have no water within 4 blocks (level with them or one up): plant them AT ONCE or they turn back to dirt within minutes; to keep a field, pour water beside it`
  : 'plant them now: bare farmland turns back to dirt the moment anything jumps on it, and a field tilled in one pass and sown in the next loses the beds it walked back over'
// slab-merge safety (jizo-melon-patch, 09-26): a covered channel cell must never get another cover job (src/cover.mjs)
import { channelCovered } from './cover.mjs'
export function farmJobs ({ cells, worldAt, items = {} }) {
  const jobs = []
  const push = (job, item) => jobs.push({ ...job, ...(item ? { item, have: (items[item] ?? 0) > 0 } : {}) })
  // A farm's own channel drowns the work beside it: `dig` refuses a block with water in the three above it (the body
  // would dive for it and run out of air), and Chani's farm.maintain gave that up as "twice in a row" on the block
  // under her own channel. A cell like that is never dug: it is listed as skipped, with what would have to happen first.
  const drowned = (x, y, z) => [1, 2, 3].some(dy => worldAt(x, y + dy, z)?.name === 'water')
  const clear = (job, item) => drowned(job.x, job.y, job.z)
    ? (jobs.push({ ...job, do: 'skip', why: `${job.why}, and water stands over it: drain it or dig it from the shore first` }), true)
    : (push(job, item), false)
  for (const cell of cells) {
    const spec = PLAN_LEGEND[cell.ch]
    if (!spec) continue
    // the plan's y is the ground the cell is made of; what the plan puts on it stands one above
    const ground = worldAt(cell.x, cell.y, cell.z)
    const here = worldAt(cell.x, cell.y + 1, cell.z)
    const standing = here && here.name !== 'air' ? here.name : null
    if (spec.kind === 'water') {
      // a cell nobody can see is nobody's job; a slab already laid in the source is a finished channel, water and floor both
      if (!ground || channelCovered(ground)) continue
      // nothing of a dry channel is worth starting without the water: the dig leaves a pit and the cover lays a slab
      // on bare ground. One bucket does the whole field, so this asks only whether any water is carried at all
      const dry = !holdsWater(ground)
      if (dry && !((items.water_bucket ?? 0) > 0)) {
        // and a cell already dug out (the bucket emptied between the dig and the pour) is filled back in rather than
        // left as the pit Chani could not path past. What is missing is still the water, so that is what is reported
        const hole = isAir(ground.name)
        if (hole) push({ do: 'fill', x: cell.x, y: cell.y, z: cell.z, why: `the channel at ${cell.x},${cell.y},${cell.z} is an open hole I carry no water to fill` }, 'dirt')
        jobs.push({ do: 'skip', x: cell.x, y: cell.y, z: cell.z, item: 'water_bucket', have: false, why: `the channel at ${cell.x},${cell.y},${cell.z} is dry and I carry no water: ${hole ? 'filled back in rather than left as a hole' : 'fill a bucket first'}` })
        continue
      }
      if (dry) {
        // a channel somebody walked over and filled in: dig the cell out before pouring, or the water lands on the ground beside it
        // (and pouring onto a bed that could not be dug out would only put the water one block too high)
        // the dig and the pour are one job in two halves: a body with no water in hand must not open the hole it
        // cannot fill. Carrying water_bucket on the DIG stops it at the bill and again when the job runs, which is
        // the case that left a pit in Chani's field (one bucket bills for a whole field but empties on first use)
        if (ground.name !== 'air' && clear({ do: 'clear', x: cell.x, y: cell.y, z: cell.z, why: `${ground.name} where the channel should be` }, 'water_bucket')) continue
        // `pour` names the solid block to pour ONTO and the water lands one above it: the source belongs at y, so pour onto y-1
        push({ do: 'pour', x: cell.x, y: cell.y - 1, z: cell.z, why: `the channel at ${cell.x},${cell.y},${cell.z} is dry` }, 'water_bucket')
      }
      // water that is only flowing through the cell (level 1-7) is nobody's source: a slab laid into it is not
      // waterlogged and cuts the flow off (the human, 09-24), and the cell dries the tick the flow recedes. So a flowing
      // cell is made a source first, a bucket poured onto the block under it, and without a bucket it is left alone
      // and said so; see src/cover.mjs
      const flowing = ground.name === 'water' && !hasWaterSource(ground)
      if (flowing && !((items.water_bucket ?? 0) > 0)) {
        jobs.push({ do: 'skip', x: cell.x, y: cell.y, z: cell.z, item: 'water_bucket', have: false, why: `flowing water at ${cell.x},${cell.y},${cell.z}: pour a source first, then cover` })
        continue
      }
      if (flowing) push({ do: 'pour', x: cell.x, y: cell.y - 1, z: cell.z, why: `the channel at ${cell.x},${cell.y},${cell.z} is flowing water, not a source` }, 'water_bucket')
      // and cover it: open water in a field is a hole to fall into and a wall to the pathfinder
      push({ do: 'cover', x: cell.x, y: cell.y, z: cell.z, why: `the channel at ${cell.x},${cell.y},${cell.z} is open water` }, spec.cover)
      continue
    }
    if (spec.kind === 'path') continue
    if (spec.kind === 'crop') {
      if (standing === spec.crop) continue
      if (standing && !WEEDS.has(standing)) continue
      if (standing && clear({ do: 'clear', x: cell.x, y: cell.y + 1, z: cell.z, why: `${standing} grew on the bed` })) continue
      if (spec.ground === 'farmland' && ground && ground.name !== 'farmland') push({ do: 'till', x: cell.x, y: cell.y, z: cell.z, why: `${ground.name} where farmland should be` })
      push({ do: 'plant', x: cell.x, y: cell.y + 1, z: cell.z, why: 'an empty bed' }, spec.seed)
      continue
    }
    if (!spec.item || sameFamily(spec.item, standing) || (spec.kind === 'gate' && standing?.endsWith('_fence_gate'))) continue
    // a plant in the way of a wall is weeding, not somebody's block: a bush grew into claude-test-pen's west wall and
    // the build walked past it, leaving a pen that looked finished and leaked
    if (standing && !WEEDS.has(standing)) continue
    if (standing && clear({ do: 'clear', x: cell.x, y: cell.y + 1, z: cell.z, why: `${standing} grew where the ${spec.item} goes` })) continue
    push({ do: 'place', x: cell.x, y: cell.y + 1, z: cell.z, why: `no ${spec.item} there` }, spec.item)
  }
  return sowAsTilled(jobs.sort((a, b) => JOB_ORDER.indexOf(a.do) - JOB_ORDER.indexOf(b.do)))
}

// each job a plan asks for, as the primitive that does it. Shared by farm.maintain and farm.build: the same list of
// jobs builds a farm from bare ground and puts a tired one back the way its plan says.
const JOB_ACTION = { clear: 'dig', till: 'till', pour: 'pour', plant: 'place', place: 'place', fill: 'place', cover: 'place' }
export const jobCall = job => {
  const action = JOB_ACTION[job.do]
  if (!action) return null
  const at = { x: job.x, y: job.y, z: job.z }
  // a cover is a TOP slab laid into the water: waterlogged, so the source stays and the farmland beside it wet, and
  // flush with the ground, so the body walks over it level. A bottom slab was a half-step down into every channel
  // that bodies floated and wedged on (card 1ccb0ea1; the human, 09-24). `place` picks a side face for it: see
  // src/cover.mjs facesForHalf
  return [action, action === 'place' ? { item: job.item, ...at, ...(job.do === 'cover' ? { half: 'top' } : {}) } : at]
}

// what a list of jobs will use up. Counted from the jobs, not from the plan, so what already stands is not asked for
// twice: a finished farm needs nothing. One bucket does a whole field, however many channel cells are dry.
export function jobsBill (jobs) {
  const bill = {}
  for (const { item } of jobs) {
    if (!item) continue
    bill[item] = item === 'water_bucket' ? 1 : (bill[item] ?? 0) + 1
  }
  return bill
}

// one line of "what I am short of", the way every farm composite says it: wheat_seeds:12 oak_fence:4
export const shortLine = short => Object.entries(short).map(([item, n]) => `${item}:${n}`).join(' ')

// Where to ask pen.check whether a pen already stands around a plan: over the floor cells the plan marks inside its
// walls, nearest the middle first, and one level lower as well - a pen whose floor is sunk below its plan is the case
// this guards, and its feet stand at the plan's own y. A wall cell is never a spot to stand on, so only `.` cells count.
export function penProbes (cells, limit = 2) {
  const floor = cells.filter(c => PLAN_LEGEND[c.ch]?.kind === 'path')
  if (!floor.length) return []
  const mid = { x: (Math.min(...floor.map(c => c.x)) + Math.max(...floor.map(c => c.x))) / 2, z: (Math.min(...floor.map(c => c.z)) + Math.max(...floor.map(c => c.z))) / 2 }
  const near = (a, b) => (Math.abs(a.x - mid.x) + Math.abs(a.z - mid.z)) - (Math.abs(b.x - mid.x) + Math.abs(b.z - mid.z))
  return [...floor].sort(near).slice(0, limit).flatMap(({ x, y, z }) => [{ x, y: y + 1, z }, { x, y, z }])
}

// Where to stand to ask whether a pen holds: over the floor cell the plan marks inside its walls, nearest the middle,
// because penAround starts from where my feet are and a wall cell is not a spot to stand on. The plan's y is the floor
// block itself, so feet go one above it.
export const penInside = cells => penProbes(cells, 1)[0] ?? null

// A build fills and digs before it places anything, and every one of those jobs takes a floor or a wall apart for as
// long as the list runs: a floor raised beside a standing fence leaves half a block, and an animal steps over half a
// block. Chani ran pen.build over a pen holding 4 sheep and all four were 20 blocks away by the time it failed
// (BUGS.md 2026-09-23 02:55Z). Placing only ever adds, so a plan with nothing to fill or clear may be built over a
// pen that is full. `census` is what pen.check answered for the pen the plan's cells lie in (null: no pen there).
const OPENS = new Set(['fill', 'clear'])
export const openingJobs = jobs => jobs.filter(j => OPENS.has(j.do))
export function penOpenRefusal (name, jobs, census) {
  const opens = openingJobs(jobs)
  if (!opens.length || !census?.inside) return null
  const counts = ['fill', 'clear'].map(verb => [verb, opens.filter(j => j.do === verb).length]).filter(([, n]) => n > 0)
  const what = counts.map(([verb, n], i) => i === 0 ? `${n} ${n === 1 ? 'cell' : 'cells'} to ${verb}` : `${n} to ${verb}`).join(' and ')
  return `${name} holds ${census.inside} and the build would open it (${what}): a floor raised or a block dug out beside a standing fence leaves half a block, and an animal steps over half a block. Lead them out first (flock.lead), or mark a plan that matches the pen as it stands - then build`
}

// what a plan needs that I do not carry. Counted before a build starts: half a farm is worse than none.
export const billShortfall = (bill, items = {}) =>
  Object.fromEntries(Object.entries(bill).map(([item, n]) => [item, n - (items[item] ?? 0)]).filter(([, n]) => n > 0))

// what to build a floor out of, by the ground the plan's legend asks for
const FLOOR_ITEM = { sand: 'sand' }
// a cell that already holds what the plan wants there is never dug out (a chest full of seed, a crop halfway grown)
const planHas = (spec, name) => sameFamily(spec.item, name) || name === spec.crop || (spec.kind === 'gate' && name.endsWith('_fence_gate'))
// The levelling a plan needs before any of its jobs can be done: a floor under every cell and open air in the cell and
// over it. Read-only judgement; `solid(name)` answers whether a block stands in the way (grass and flowers do not).
export function groundJobs ({ cells, worldAt, solid }) {
  const jobs = []
  const fills = []
  for (const cell of cells) {
    const spec = PLAN_LEGEND[cell.ch]
    if (!spec) continue
    // the plan's y IS the floor of a cell, so it is never dug out; a channel holds its source at that level, and the
    // block the water is poured onto is the one below it
    const floorY = spec.kind === 'water' ? cell.y - 1 : cell.y
    const floor = worldAt(cell.x, floorY, cell.z)
    if (floor && !solid(floor.name)) fills.push({ do: 'fill', x: cell.x, y: floorY, z: cell.z, why: `${floor.name} where the floor should be`, item: FLOOR_ITEM[spec.ground] ?? 'dirt' })
    for (const y of [cell.y + 1, cell.y + 2]) {
      const here = worldAt(cell.x, y, cell.z)
      if (!here || !solid(here.name) || planHas(spec, here.name)) continue
      jobs.push({ do: 'clear', x: cell.x, y, z: cell.z, why: y === cell.y + 1 ? `${here.name} stands in the cell` : `${here.name} stands where the plan wants open air` })
    }
  }
  // dig first, fill afterwards: the boulder often stands over the hole
  return [...jobs, ...fills]
}

// ---------------------------------------------------------------- the catalogue behind ./mc help
// A primitive is ONE game operation that needs the body's innards (a window, the pathfinder, a reflex, entity
// tracking). Anything that loops over primitives or decides between them is a composite in library/<folder>/<file>.mjs
// and is called folder.file. Sections below are for primitives; composites are grouped by their folder.
export const SECTIONS = {
  sense: 'what I can see from here',
  map: 'the shared map everyone reads',
  move: 'getting about',
  block: 'blocks and the ground',
  item: 'the things I carry',
  creature: 'animals and monsters',
  self: 'my own body',
  control: 'driving the body',
  farm: 'crops, fields and what comes off them',
  pen: 'fences and gates',
  flock: 'animals as a herd',
  mine: 'digging for stone and ore',
  work: 'whole jobs that run themselves'
}

// a composite declares its arguments as {name: 'type'}, 'type!' for one it cannot do without
export const argsUsage = args => Object.entries(args ?? {})
  .map(([name, type]) => (String(type).endsWith('!') ? `${name}=` : `[${name}=]`))
  .sort((a, b) => Number(a.startsWith('[')) - Number(b.startsWith('[')))
  .join(' ')
  // a point is one argument to whoever types it
  .replace('[x=] [y=] [z=]', '[x= y= z=]')

// a composite's doc reads 'name args=: what it does'; the catalogue prints the usage from the args declaration instead

export const docText = doc => String(doc).includes(': ') ? String(doc).slice(String(doc).indexOf(': ') + 2) : String(doc)

// farm.maintain belongs to farm; a composite with no folder (routine, hunt) is a job in its own right
const sectionOf = entry => entry.section ?? (entry.name.includes('.') ? entry.name.split('.')[0] : 'work')
const usageOf = entry => `${entry.name}${entry.args ? ` ${entry.args}` : ''}`

// ./mc help: every action and its arguments; ./mc help <section|action>: what it does, and what makes it hand back
export function helpText (topic, entries) {
  const groups = new Map()
  for (const entry of entries) groups.set(sectionOf(entry), [...(groups.get(sectionOf(entry)) ?? []), entry])
  const order = [...Object.keys(SECTIONS).filter(s => groups.has(s)), ...[...groups.keys()].filter(s => !SECTIONS[s]).sort()]
  const header = section => `${section}: ${SECTIONS[section] ?? 'its own corner of the world'}`
  const brief = entry => `  ${usageOf(entry)}${entry.stops ? ` | stops: ${entry.stops}` : ''}`
  const detail = entry => [`  ${usageOf(entry)} - ${entry.doc}`, ...(entry.stops ? [`    stops: ${entry.stops}`] : [])]
  if (!topic) {
    return [...order.flatMap(s => [header(s), ...groups.get(s).map(brief)]),
      './mc help <section> or ./mc help <action> for what one does',
      renamedList()].join('\n')
  }
  if (groups.has(topic)) return [header(topic), ...groups.get(topic).flatMap(detail)].join('\n')
  const one = entries.find(entry => entry.name === topic)
  if (!one) return didYouMean(topic, entries.map(entry => entry.name))
  return [usageOf(one), one.doc, ...(one.stops ? [`stops: ${one.stops}`] : [])].join('\n')
}

// Every primitive, its section, what it reads and one line of what it does: this IS ./mc help, so an action missing
// from here is invisible to whoever drives the body. bot.mjs says so at start-up if a dispatch entry has no line here.
export const PRIMITIVES = {
  // ---- sense
  state: { section: 'sense', args: '', doc: 'health, food, xp, the time, where I stand, what I hold and who else is about' },
  look: { section: 'sense', args: '[pano=] [dir=north] [x= y= z=]', doc: 'a picture of what I see, rendered to a PNG file' },
  look_at: { section: 'sense', args: 'x= y= z=', doc: 'turn my head to face a point' },
  look_around: { section: 'sense', args: '[range=16] [blocks=] [blockRange=] [mob=] [limit=]', doc: 'what stands around me: players, mobs, dropped items and any blocks you name' },
  entity: { section: 'sense', args: 'name= [count=2]', doc: 'the raw server metadata of the nearest entities with that name (a debugging aid)' },
  animals: { section: 'sense', args: '[mob=] [within=24] [x= y= z=]', doc: 'the farm animals near me: kind, id, where, grown or a baby, and whether it is in a pen (the one around me, or around x= y= z=)' },
  find_blocks: { section: 'sense', args: 'block= [maxDistance=64] [count=10]', doc: 'where the nearest blocks of a kind are; * wildcards work (*_log)' },
  block_at: { section: 'sense', args: 'x= y= z=', doc: 'the name and properties of one block' },
  scan: { section: 'sense', args: 'x1= y1= z1= x2= y2= z2= [where=]', doc: 'an ASCII map of a box of the world, or with where= just the coordinates of one kind of block' },
  path_to: { section: 'sense', args: 'x= y= z= [range=] [dig=] [stroll=] [route=] [live=]', doc: 'what the pathfinder makes of a walk from here, without walking it; route=true names the gates it opens and a waypoint every six steps; live=true plans with the movements walks use right now and names what differs from a fresh set' },
  inventory: { section: 'sense', args: '', doc: 'what I carry, what I wear and how many slots are free' },
  chest_contents: { section: 'sense', args: '[x= y= z=]', doc: 'what is in a chest' },
  events: { section: 'sense', args: '[type=] [last=]', doc: 'my own event log: what happened while you were not looking' },
  // ---- map
  places: { section: 'map', args: '[name=] [q=] [by=] [kind=] [within=] [limit=]', doc: 'search the shared map: bases, farms, mines, villages, dangers. name= gives one place whole; never read places.json yourself' },
  mark: { section: 'map', args: 'name= [kind=] [note=] [x= y= z=] [move=]', doc: 'put a place on the shared map, here or at a point. Marking an existing place again keeps where it is unless x= y= z= is given; moving one off what still stands there needs move=true' },
  unmark: { section: 'map', args: 'name=', doc: 'take a place off the shared map' },
  zones: { section: 'map', args: '', doc: 'the protected areas: what nobody may dig through' },
  protect: { section: 'map', args: 'name= x1= y1= z1= x2= y2= z2=', doc: 'protect a box of the world, mine or shared' },
  unprotect: { section: 'map', args: 'name=', doc: 'drop a protected area' },
  // ---- move
  goto: { section: 'move', args: 'place= | player= | x= z= [y=] [range=] [dig=]', doc: 'walk there, opening doors and swimming; it does not dig or bridge unless dig=true, and crosses planted cells only where there is no other way, at a walking pace' },
  follow: { section: 'move', args: 'player=', doc: 'keep walking after someone until stop' },
  boat_state: { section: 'move', args: '[id=]', doc: 'read nearby boats, their passenger IDs and UUIDs, and the boat I ride' },
  boat_place: { section: 'move', args: 'item= x= y= z=', doc: 'place one carried boat at a checked water or ground cell and report its entity ID' },
  boat_leash: { section: 'move', args: 'id=', doc: 'attach a carried lead to one nearby boat and verify that I hold its leash' },
  boat_unleash: { section: 'move', args: 'id=', doc: 'detach the lead from one boat after its passenger is secured' },
  boat_recover: { section: 'move', args: 'id=', doc: 'break and reclaim a verified empty, unleashed boat for reuse' },
  boat_mount: { section: 'move', args: 'id=', doc: 'board a nearby boat and confirm I am its controlling first passenger' },
  boat_dismount: { section: 'move', args: '', doc: 'leave the boat and verify that I am on foot' },
  boat_release: { section: 'move', args: 'id= passengerUuid=', doc: 'break a nearby boat after I dismount and verify the named passenger is safely on foot' },
  boat_swim: { section: 'move', args: 'x= y= z= [ms=700]', doc: 'swim toward a checked water waypoint with forward and jump held together for one bounded stroke' },
  // ---- block
  dig: { section: 'block', args: 'x= y= z= [wet=] [dig=] [batch=]', doc: 'break one block and pick up what it drops (dig=true: the walk to it may tunnel; batch=true: one cell of a sweep, no wait for the drop and no chase after it, collect afterwards). Walks only when the cell is out of arm\'s reach. Not water or lava: use fill or place' },
  place: { section: 'block', args: 'item= x= y= z= [facing=] [half=] | blocks=', doc: 'build: one block, or a whole list of them in the order given' },
  clear: { section: 'block', args: 'x1= y1= z1= x2= y2= z2= [keep=]', doc: 'dig out a whole box top-down, up to 400 blocks; beds, containers and fluids are kept' },
  till: { section: 'block', args: 'x= y= z= | blocks=', doc: 'hoe dirt or grass into farmland (give the ground block, not the air above it)' },
  path: { section: 'block', args: 'x= y= z= | blocks=', doc: 'shovel grass into a walking path' },
  fertilize: { section: 'block', args: 'x= y= z= | blocks=', doc: 'bone meal on a crop, a sapling or a grass block' },
  fill: { section: 'block', args: 'x= y= z=', doc: 'scoop a water or lava source block into a bucket' },
  pour: { section: 'block', args: 'x= y= z=', doc: 'empty the bucket onto the solid block you name; the water lands one above it' },
  toggle: { section: 'block', args: 'x= y= z= [open=]', doc: 'work a gate, door, trapdoor, lever or button by hand' },
  use: { section: 'block', args: 'x= y= z= [item=] [ticks=]', doc: 'right-click a block with what I hold: a composter, a lectern, anything toggle refuses' },
  // ---- item
  craft: { section: 'item', args: 'item= [count=1]', doc: 'craft, using a crafting table within 32 blocks when the recipe needs one. Answers made= (a batch can overshoot what you asked for). A failure says whether the ingredients were consumed: if they were not, retry, the second call usually works' },
  smelt: { section: 'item', args: 'item= [count=] [fuel=] [fuelCount=] [wait=] [x= y= z=]', doc: 'cook or melt in the nearest furnace and wait for it, by day' },
  furnace_take: { section: 'item', args: '[x= y= z=]', doc: 'take what is done out of a furnace' },
  deposit: { section: 'item', args: 'items= | item= [count=] | all=true [x= y= z=]', doc: 'put things into a chest, then open it again to check they really went in' },
  withdraw: { section: 'item', args: 'items= | item= [count=] [x= y= z=]', doc: 'take things out of a chest, checking the same way' },
  equip: { section: 'item', args: 'item= [destination=]', doc: 'hold it, or wear it: armour finds its own slot' },
  toss: { section: 'item', args: 'item= [count=]', doc: 'drop something on the ground' },
  villager_food: { section: 'creature', args: 'uuid= item= count=', doc: 'drop a bounded breeding-food portion toward one observed on-foot adult and report server-confirmed item pickup by UUID' },
  give: { section: 'item', args: 'player= item= [count=] [dig=]', doc: 'hand something to a player and watch that it was taken' },
  enchant: { section: 'item', args: 'item= [slot=] [x= y= z=]', doc: 'enchant one item I carry at an enchanting table, paying lapis and levels' },
  trades: { section: 'item', args: '[x= y= z=] [id=]', doc: 'read one nearby villager profession and numbered offers' },
  trade: { section: 'item', args: 'offer= [times=1] [x= y= z=] [id=]', doc: 'buy a numbered offer from one nearby villager and verify the inventory change' },
  // ---- creature
  attack: { section: 'creature', args: 'mob= [id=] [leash=24]', doc: 'hunt one animal or monster: the nearest of its kind, or the id= that animals gave you; it leaves the drops lying where they fall' },
  shear: { section: 'creature', args: '[count=] [within=40]', doc: 'wool without killing: needs shears' },
  feed: { section: 'creature', args: 'mob= [id=]', doc: 'walk to one animal and hold out the food it breeds on (id= from animals)' },
  escort: { section: 'creature', args: 'mob= x= y= z= [count=] [within=32] [penned=] [range=]', doc: 'the walk itself: fetch the animals and bring them to a spot, stopping for stragglers (flock.lead is the whole job)' },
  'pen.check': { section: 'pen', args: '[x= y= z=] [radius=]', doc: 'walk a fence and find where a pen leaks: gaps, corner gates, rims an animal can hop' },
  // ---- self
  eat: { section: 'self', args: '[item=] [anyway=]', doc: 'eat one of the foods I carry now: the reflex should beat you to it, but when it cannot this says what went wrong. At food 6 or less with nothing else edible I eat the never-eat list too (rotten flesh: its hunger cannot take me below where the empty belly already would); anyway=true does that at any hunger, on your say-so' },
  sleep: { section: 'self', args: '[any=] [bed=] [bed_range=]', doc: 'sleep in the nearest free bed within 32 blocks; with none, walk to your own bed (bed=<place>, else your nearest kind=bed mark) when it is within bed_range (default 200) and sleep there' },
  wake: { section: 'self', args: '', doc: 'get out of bed' },
  quit: { section: 'self', args: '', doc: 'stop my body; ./start in the background brings it back' },
  chat: { section: 'self', args: 'message=', doc: 'say something to everyone' },
  whisper: { section: 'self', args: 'player= message=', doc: 'say something to one player' },
  // ---- control
  run: { section: 'control', args: 'steps=', doc: 'run a list of actions in order, one after another' },
  stop: { section: 'control', args: '', doc: 'cancel whatever the body is doing' },
  watch: { section: 'control', args: 'name= block=|mob=|item= [where=] [count=] [atMost=] [within=] [x= y= z=] [repeat=]', doc: 'tell me when the world comes to look like this' },
  unwatch: { section: 'control', args: 'name=', doc: 'drop a watch' },
  watches: { section: 'control', args: '', doc: 'the watches I have set' },
  reflexes: { section: 'control', args: '[on=]', doc: 'switch the body reflexes (eating, fleeing, bedtime, shutting gates) on or off' },
  control: { section: 'control', args: 'state= [ms=]', doc: 'hold one movement key down by hand (a debugging aid)' },
  wait: { section: 'control', args: '[seconds=100]', doc: 'block until something happens that needs me; reads the event log, so it needs no body' },
  dawn: { section: 'control', args: '', doc: 'block until morning; needs no body, so a bodiless night is spent here' },
  clock: { section: 'control', args: '', doc: 'the world time as last seen by any body; needs no body' },
  help: { section: 'control', args: '[<section or action>]', doc: 'this catalogue, one section of it, or everything about one action' }
}
