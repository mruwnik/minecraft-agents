// Turning a plan and the world into an ordered job list (till, plant, clear, fill...), and what the jobs cost.
import { isAir, holdsWater, hasWaterSource } from './world.mjs'
import { PLAN_LEGEND, planSpec, planItemMatches, planClaimAt, groundItem, planCropMatches, FARMLAND_CROPS, GENERIC_SEED } from './plan.mjs'
import { sameFamily, WEEDS } from './anchor.mjs'
// slab-merge safety (jizo-melon-patch, 09-26): a covered channel cell must never get another cover job (src/build/cover.mjs)
import { channelCovered } from '../build/cover.mjs'
import { itemShortfall } from './inventory.mjs'

// a bed tilled before its channel holds water dries back to dirt, so the water goes in first
const JOB_ORDER = ['skip', 'clear', 'pour', 'cover', 'till', 'plant', 'place']
// farmland stays farmland with water within 4 on x and z, level with it or one up (the rule dryCells and planErrors use)
export const hydrated = (worldAt, { x, y, z }) => [y, y + 1].some(wy => {
  for (let dx = -4; dx <= 4; dx++) for (let dz = -4; dz <= 4; dz++) if (holdsWater(worldAt(x + dx, wy, z + dz))) return true
  return false
})
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
export function farmJobs ({ cells, worldAt, items = {} }) {
  const jobs = []
  // Specific beds get first claim on their seed; generic beds share what remains.
  const seeds = { ...items }
  for (const cell of cells) {
    const spec = planSpec(cell)
    if (['tree', 'reserved'].includes(spec?.kind)) continue
    if (spec?.kind !== 'crop' || spec.generic) continue
    const name = worldAt(cell.x, cell.y + 1, cell.z)?.name
    if (!planCropMatches(spec, name) && (!name || isAir(name) || WEEDS.has(name))) seeds[spec.seed] = Math.max(0, (seeds[spec.seed] ?? 0) - 1)
  }
  let groundY
  const push = (job, item) => jobs.push(Object.defineProperty({ ...job, ...(item ? { item, have: (items[item] ?? 0) > 0 } : {}) }, 'groundY', { value: groundY }))
  // A farm's own channel drowns the work beside it: `dig` refuses a block with water in the three above it (the body
  // would dive for it and run out of air), and Chani's farm.maintain gave that up as "twice in a row" on the block
  // under her own channel. A cell like that is never dug: it is listed as skipped, with what would have to happen first.
  const drowned = (x, y, z) => [1, 2, 3].some(dy => worldAt(x, y + dy, z)?.name === 'water')
  const clear = (job, item) => drowned(job.x, job.y, job.z)
    ? (jobs.push({ ...job, do: 'skip', why: `${job.why}, and water stands over it: drain it or dig it from the shore first` }), true)
    : (push(job, item), false)
  for (const cell of cells) {
    const spec = planSpec(cell)
    if (['tree', 'reserved'].includes(spec?.kind)) continue
    if (!spec) continue
    groundY = cell.y
    // the plan's y is the ground the cell is made of; what the plan puts on it stands one above
    const ground = worldAt(cell.x, cell.y, cell.z)
    const here = worldAt(cell.x, cell.y + 1, cell.z)
    const standing = here && here.name !== 'air' ? here.name : null
    if (spec.kind === 'air') {
      if (here && !isAir(here.name)) jobs.push({ do: 'skip', x: cell.x, y: cell.y + 1, z: cell.z, why: `planned air occupied by ${here.name}; tidy safely before proceeding` })
      continue
    }
    if (spec.kind === 'ground') {
      if (spec.ground === 'farmland' && ground && ground.name !== 'farmland') push({ do: 'till', x: cell.x, y: cell.y, z: cell.z, why: `${ground.name} where farmland should be` })
      continue
    }
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
      // and said so; see src/build/cover.mjs
      const flowing = ground.name === 'water' && !hasWaterSource(ground)
      if (flowing && !((items.water_bucket ?? 0) > 0)) {
        jobs.push({ do: 'skip', x: cell.x, y: cell.y, z: cell.z, item: 'water_bucket', have: false, why: `flowing water at ${cell.x},${cell.y},${cell.z}: pour a source first, then cover` })
        continue
      }
      if (flowing) push({ do: 'pour', x: cell.x, y: cell.y - 1, z: cell.z, why: `the channel at ${cell.x},${cell.y},${cell.z} is flowing water, not a source` }, 'water_bucket')
      // and cover it: open water in a field is a hole to fall into and a wall to the pathfinder
      if (spec.cover) push({ do: 'cover', x: cell.x, y: cell.y, z: cell.z, why: `the channel at ${cell.x},${cell.y},${cell.z} is open water` }, spec.cover)
      continue
    }
    if (spec.kind === 'path') continue
    if (spec.kind === 'crop') {
      if (planCropMatches(spec, standing)) continue
      if (standing && !WEEDS.has(standing)) continue
      if (standing && clear({ do: 'clear', x: cell.x, y: cell.y + 1, z: cell.z, why: `${standing} grew on the bed` })) continue
      if (spec.ground === 'farmland' && ground && ground.name !== 'farmland') push({ do: 'till', x: cell.x, y: cell.y, z: cell.z, why: `${ground.name} where farmland should be` })
      const seed = spec.generic ? FARMLAND_CROPS.find(s => (seeds[s.seed] ?? 0) > 0)?.seed ?? GENERIC_SEED : spec.seed
      if (spec.generic && seed !== GENERIC_SEED) seeds[seed]--
      push({ do: 'plant', x: cell.x, y: cell.y + 1, z: cell.z, why: 'an empty bed' }, seed)
      continue
    }
    if (spec.kind === 'torch') {
      if (!planItemMatches(spec, standing, sameFamily)) {
        if (standing && !WEEDS.has(standing)) continue
        if (standing && clear({ do: 'clear', x: cell.x, y: cell.y + 1, z: cell.z, why: `${standing} grew where the torch post goes` })) continue
        push({ do: 'place', x: cell.x, y: cell.y + 1, z: cell.z, why: 'no torch post there' }, spec.item)
      }
      const above = worldAt(cell.x, cell.y + 2, cell.z)?.name
      // Existing lights or somebody's block on the post are left intact.
      if (above && !isAir(above) && !WEEDS.has(above)) continue
      if (above && WEEDS.has(above) && clear({ do: 'clear', x: cell.x, y: cell.y + 2, z: cell.z, why: `${above} grew where the torch goes` })) continue
      push({ do: 'place', x: cell.x, y: cell.y + 2, z: cell.z, why: 'no torch on the post' }, 'torch')
      continue
    }
    if (!spec.item || planItemMatches(spec, standing, sameFamily) || (!spec.literal && spec.kind === 'gate' && standing?.endsWith('_fence_gate'))) continue
    // a plant in the way of a wall is weeding, not somebody's block: a bush grew into claude-test-pen's west wall and
    // the build walked past it, leaving a pen that looked finished and leaked
    if (standing && !WEEDS.has(standing)) continue
    if (standing && clear({ do: 'clear', x: cell.x, y: cell.y + 1, z: cell.z, why: `${standing} grew where the ${spec.item} goes` })) continue
    push({ do: 'place', x: cell.x, y: cell.y + 1, z: cell.z, why: `no ${spec.item} there` }, spec.item)
  }
  return sowAsTilled([...new Map(jobs.map(j => [`${j.do},${j.x},${j.y},${j.z},${j.item ?? ''}`, j])).values()].sort((a, b) => JOB_ORDER.indexOf(a.do) - JOB_ORDER.indexOf(b.do)))
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
  // src/build/cover.mjs facesForHalf
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
  const floor = cells.filter(c => planSpec(c)?.kind === 'path')
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
export const billShortfall = itemShortfall

// a cell that already holds what the plan wants there is never dug out (a chest full of seed, a crop halfway grown)
const planHas = (spec, name) => planItemMatches(spec, name, sameFamily) || planCropMatches(spec, name) || (!spec.literal && spec.kind === 'gate' && name.endsWith('_fence_gate')) || (spec.kind === 'torch' && ['torch', 'soul_torch'].includes(name))
// The levelling a plan needs before any of its jobs can be done: a floor under every cell and open air in the cell and
// over it. Read-only judgement; `solid(name)` answers whether a block stands in the way (grass and flowers do not).
export function groundJobs ({ cells, worldAt, solid }) {
  const jobs = []
  const fills = []
  for (const cell of cells) {
    const spec = planSpec(cell)
    if (['tree', 'reserved', 'air'].includes(spec?.kind)) continue
    if (!spec) continue
    // the plan's y IS the floor of a cell, so it is never dug out; a channel holds its source at that level, and the
    // block the water is poured onto is the one below it
    const floorY = spec.kind === 'water' ? cell.y - 1 : cell.y
    const floor = worldAt(cell.x, floorY, cell.z)
    if (floor && !solid(floor.name)) fills.push({ do: 'fill', x: cell.x, y: floorY, z: cell.z, why: `${floor.name} where the floor should be`, item: groundItem(spec) })
    for (const y of [cell.y + 1, cell.y + 2]) {
      const here = worldAt(cell.x, y, cell.z)
      if (!here || !solid(here.name) || planHas(spec, here.name) || planClaimAt(cells, { x: cell.x, y, z: cell.z }, cell)) continue
      jobs.push({ do: 'clear', x: cell.x, y, z: cell.z, why: y === cell.y + 1 ? `${here.name} stands in the cell` : `${here.name} stands where the plan wants open air` })
    }
  }
  // dig first, fill afterwards: the boulder often stands over the hole
  return [...jobs, ...fills]
}
