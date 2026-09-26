// Pure helpers, kept apart from bot.mjs so they can be tested without a server.
import { compact, between } from './cli.mjs'
import { WORK_RANGE } from './walk.mjs'
import { range, inAnyZone, within, isGroundCover, looksBuilt, FLUIDS, isAir, holdsWater, hasWaterSource, STEPS } from './lib/world.mjs'
import { NEVER_FIGHT, ENDERMAN_RANGE } from './lib/fight.mjs'
import { ripeCrop } from './lib/farm.mjs'
import { invited } from './lib/places.mjs'
import { BREEDING_FOOD } from './lib/animals.mjs'
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

// A meal on the way is not a loss: whatever the body ate comes off lost= and is said as ate= (my goto said "lost bread:1")
export function mealTally ({ gained, lost, ate }) {
  if (!Object.keys(ate).length) return { gained, lost }
  const left = Object.fromEntries(Object.entries(lost).map(([k, n]) => [k, n - (ate[k] ?? 0)]).filter(([, n]) => n > 0))
  return { gained, lost: left, ate }
}

// hostile-looking mobs the flee/fight reflexes should leave alone: endermen always, spiders in bright daylight (neutral then)
export const ignorableMob = (name, { day, skyLight }) => name === 'enderman' || (name === 'spider' && day && skyLight >= 12)

// can I dig this cell from where I stand: the arm reaches 4.5 from the eyes. dig used to walk to within 3 of every cell
export const DIG_REACH = 4.5
export const digFromHere = (feet, cell, reach = DIG_REACH) =>
  Math.hypot(cell.x + 0.5 - feet.x, cell.y + 0.5 - (feet.y + 1.62), cell.z + 0.5 - feet.z) <= reach

// what a dig does before it moves (card 150b3ee1): a dig at a cell that was already air walked for 148 s before it looked.
// name: the cell as read from here (undefined: its chunk is not loaded). needed: a tool the body lacks for it. near: digFromHere
export const digPlan = ({ name, needed, near }) => {
  if (name === undefined) return near ? 'air' : 'walk'
  if (isAir(name)) return 'air'
  if (needed) return 'tool'
  return near ? 'dig' : 'walk'
}
// a dig's walk that has not arrived by then gives up, and says so
export const DIG_WALK_MS = 30000
export const digUnreached = p =>
  `could not reach ${p.x},${p.y},${p.z} to dig it in ${DIG_WALK_MS / 1000}s (no standing spot within reach, or a drop or mob in the way): goto a spot beside it, then dig again`

// night is when beds work; state, the shared clock, night_fell and the reflexes must all agree on it
export const isNight = tick => tick > 12542 && tick < 23460

// suffocating (died once under gravel that fell while a digging walk tunnelled beneath it): the block to dig free is the one
// round our head; a solid block at the feet alone does no harm
// only blocks that fall: a door or a slab at head height is no burial, and digging it would wreck someone's house
const FALLS = /^(gravel|sand|red_sand|suspicious_sand|suspicious_gravel|.*_concrete_powder)$/
export const buriedIn = ([head]) => head?.boundingBox === 'block' && FALLS.test(head.name) ? head : undefined

// fight or run. Running at 8 health was too late for a body without armour: two zombies took 20 health in 12 s, and it died fleeing.
// So run earlier the less armour it wears and the more there are of them (never above 16: a fresh body may always try)
export function shouldFlee (s) {
  if (!s.armed) return true
  return s.health <= Math.min(8 + (4 - s.armorPieces) + 3 * (s.attackers - 1), 16)
}

// archers shoot from beyond the 7 blocks the other reflexes watch: a body that stood still was shot dead without ever reacting.
// Just hurt, not in a fight, an archer in sight: charge it (it cannot be outwaited), or run when unarmed or badly hurt
export const ARCHERS = new Set(['skeleton', 'stray', 'bogged', 'pillager'])
// #138. The flee reflex had no end and no way home. `GoalInvert(GoalFollow(mob, 16))` runs for as long as the mob is
// followed, `fleeingUntil` was re-armed every tick a threat sat within 7 blocks, and `stop` cleared neither, so a body
// chased once drifted until something else stopped it: hundreds of blocks from its work, and one of them starved on
// the way. A run has a phase now. It goes AWAY until the threat is `bound` blocks off, then BACK to the cell the run
// started from, and both halves can end badly: a run that covers no ground is boxed in and says so, and a second run
// from the same mob within a minute of the last walk back does not walk back at all, because the walk back is what
// was feeding the loop. Either way the agent is told and gets the legs, because the body has run out of ideas and the
// agent has not: it can dig down, wall the hole behind it, fight, or wait for dawn.
export const FLEE_AWAY = 16
export const FLEE_ARCHER_AWAY = 28
export const FLEE_NEAR = 6
export const FLEE_STUCK_MS = 6000
export const FLEE_HOME = 2
export const FLEE_HOLD_MS = 60000
export const FLEE_GIVEUP_MS = 15000
export const FLEE_STUCK_NOTE = 'the run has covered no ground in 6 seconds: I am boxed in. Dig straight down and wall the hole behind me, or turn and fight'
export const FLEE_HELD_NOTE = 'the same threat drove me off again within a minute of the last walk back, so I am not walking back this time. Dig down, fight it, or wait for dawn: your call'
export const fleeRange = name => ARCHERS.has(name) ? FLEE_ARCHER_AWAY : FLEE_AWAY
// where a run heads: a fixed point `dist` straight away from the mob. A goal that moves with the mob reset the path at every one of its
// steps and the body never sprinted (Perrin: 10.6 blocks in 7.7 s, slower than the zombie that killed him)
export const fleeGoal = (me, mob, dist) => {
  const [dx, dz] = [me.x - mob.x, me.z - mob.z]
  const len = Math.hypot(dx, dz)
  const [ux, uz] = len > 0.01 ? [dx / len, dz / len] : [1, 0]
  return { x: Math.round(me.x + ux * dist), z: Math.round(me.z + uz * dist) }
}
// the same mob, too soon: used for the oscillation guard (a minute after a walk back) and, with FLEE_GIVEUP_MS, to
// keep a run the body already gave up on from starting itself again the moment the legs come back
export const fleeOscillating = ({ mob, last, now, within = FLEE_HOLD_MS }) => Boolean(last && last.mob === mob && now - last.at < within)

// One tick of the run. `stillMs` is how long since the body last covered ground, `homeDist` how far it is from where
// the run began, `bound` how far off this threat has to be to count as left behind. Nothing here moves anything.
export function fleeStep ({ phase = null, threatDist = Infinity, homeDist = 0, stillMs = 0, held = false, bound = FLEE_AWAY, underground = false }) {
  if (phase === 'away') {
    // clear before stuck: a body walled in whose threat has wandered off did not fail, it finished
    if (threatDist >= bound) return held ? { phase: null, event: 'flee_held', note: FLEE_HELD_NOTE } : { phase: 'back', event: 'flee_clear' }
    // #147(d): running downhill into the dark is running into the cave, not away from the mob
    if (underground) return { phase: null, event: 'flee_stuck', note: FLEE_CAVE_NOTE }
    if (stillMs >= FLEE_STUCK_MS) return { phase: null, event: 'flee_stuck', note: FLEE_STUCK_NOTE }
    return { phase: 'away' }
  }
  if (phase === 'back') {
    // the threat beats arriving: being home with it on my heels is not being safe
    if (threatDist <= FLEE_NEAR) return { phase: 'away', event: 'flee_started' }
    if (homeDist <= FLEE_HOME) return { phase: null, event: 'flee_returned' }
    if (stillMs >= FLEE_STUCK_MS) return { phase: null, event: 'flee_stuck', note: FLEE_STUCK_NOTE }
    return { phase: 'back' }
  }
  return { phase: null }
}

// (d) my own body, 2026-09-24 11:26Z: it fled a creeper at 10 hp, went down into the cave under my test pits and a zombie
// killed it there. A run heading underground or into an unlit cell is running INTO the thing it is running from. A lit room
// is shelter, so darkness counts only where the sky does not reach: both dark together is a cave mouth
export const FLEE_DROP = 4
export const FLEE_DARK = 4
// Darkness alone is no dive: my body met an enderman on a cave floor lit through its own shaft and holed up on the
// first tick of the run, twice (13:17Z, 13:20Z). Dark counts only on the way DOWN, two below where the run began
export const FLEE_DARK_DROP = 2
export const fleeIntoCave = ({ startY = 0, y = 0, skyLight = 15, light = 15 } = {}) =>
  startY - y >= FLEE_DROP || (startY - y >= FLEE_DARK_DROP && skyLight <= FLEE_DARK && light <= FLEE_DARK)
export const FLEE_CAVE_NOTE = 'the only way clear of it led underground or into the dark, and that is where bodies die: I stopped instead of running into a cave. Dig down and cap the hole, fight it, or wait for dawn: `goto x= y= z= dig=true` brings me back up'

// (b) and (c). Perrin's flee_returned walked his respawned body straight back into the skeleton and the zombie that had just
// killed it. A body that has just died does not walk anywhere while it is night or the killer is still standing there
// The #147 addendum: Mariel respawned on her bed beside the two zombies that had just killed her, fled unarmed, was boxed
// in and died, three times in five minutes. At night a respawn beside a hostile goes under the ground (zombies burn at
// sunrise: wait them out, do not outrun them); by day it only stays put
export function respawnPlan ({ night = false, bedNear = false, killerNear = false } = {}) {
  if (night && killerNear) return { do: 'burrow', why: 'respawned at night beside a hostile: digging down three and capping the hole until it is gone or the sun burns it' }
  if (killerNear) return { do: 'stay', why: 'whatever killed me is still here: staying put, not walking back to it' }
  if (night && bedNear) return { do: 'sleep', why: 'respawned at night with a bed in reach: sleeping it off rather than walking home in the dark' }
  if (night) return { do: 'stay', why: 'respawned at night with no bed: staying where I am until dawn rather than walking back through it' }
  return { do: 'free' }
}

export const crowdSize = hostiles => hostiles.filter(h => h.dist <= 5 || (ARCHERS.has(h.name) && h.dist <= 24)).length
export function rangedThreat (s) {
  if (s.hurtMsAgo > 5000 || s.fighting || s.meleeNear || !s.archerNear) return null
  return s.armed && s.health >= 14 && !s.inWater ? 'charge' : 'flee'
}

// how many of the wanted blocks a round of mining really brought in: the blocks that are gone, if anything at all was picked up
// (drops from tree tops are often still falling when we count, so the items gained are no upper limit).
// Counting every gained item called a round that only dug dirt on the way a success
export const minedCount = (gone, gained) => gained > 0 ? gone : 0
// a neutral enderman keeps its distance and teleports about; one standing next to the body has almost always been
// provoked already, and by then the body has about five seconds
export const fleeUnwinnable = (mobs, range = ENDERMAN_RANGE) =>
  mobs.filter(m => NEVER_FIGHT.has(m.name) && m.dist <= range).sort((a, b) => a.dist - b.dist)[0] ?? null

// leading animals with food in hand: they follow from up to 10 blocks and are slower than I am. distances = how far each one still with me is
// heldFor: seconds I have stood waiting. An animal that does not come (a fence between us) would keep me waiting for ever: go and get it,
// and after 3 fetches that brought me no nearer the goal, give up and tell the driver
export function leadVerdict ({ distances, holding, heldFor = 0, fetchesSinceProgress = 0, noPath = false }) {
  if (!distances.length) return 'lost'
  const farthest = Math.max(...distances)
  const fetch = farthest > 9 || (holding && heldFor >= 12 && farthest > 4)
  if (fetch) return fetchesSinceProgress >= 3 ? 'giveup' : 'fetch'
  if (farthest > (holding ? 4 : 6)) return 'hold'
  return noPath ? 'noway' : 'go'
}

// when others skip the night the server's "leave bed" sometimes never reaches the body, and it lies in bed all day. In bed in broad
// daylight (past the first 15 s of the day, no thunderstorm) means: get up myself
export const oversleeping = s => s.asleep && !s.thundering && s.timeOfDay > 300 && s.timeOfDay < 12000

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

// auto-eat starts below this food level: health only comes back at food 18 and up, so a hurt body eats sooner than a whole one
export const eatBelow = health => health < 20 ? 18 : 15

// food in my hand draws every animal that sees it after me, and out through any gate I open: put it away at a gate unless that is the plan
// luring: a lead is on, from its first step TOWARDS the animal (not only once it follows: started beside a gate, lead lost its wheat and gave up with=0)
// eating: a meal is running. The food in my hand is going into my mouth: taking it out cancelled every meal by a gate (Chani, food 7, 1743 [food away] lines)
export const foodAway = ({ held, luring, feeding, gateNear, eating = false }) => gateNear && !luring && !feeding && !eating && Object.values(BREEDING_FOOD).some(foods => foods.includes(held))
// why an animal that was shown food stayed put. rises: for its four neighbour cells, how far up the first free standing room is (Infinity: none within reach)
export function pitAdvice (mob, at, rises) {
  if (rises.some(r => r <= 1)) return null
  if (rises.every(r => r === Infinity)) return `the ${mob} at ${at} is walled in on all four sides: open a side (dig), then lead again`
  return `the ${mob} at ${at} stands in a pit (every way out is ${Math.min(...rises)}+ blocks up, it jumps 1): give it a step (place a block beside it, or dig the rim down), then lead again`
}

// in bed in broad daylight: 'ask' the server to let me up; when that changes nothing for 6 s the server has me up already and only my own flag is stale: 'declare' myself awake
export const wakeStep = ({ oversleeping, forMs }) => !oversleeping ? null : forMs >= 6000 ? 'declare' : 'ask'

// the collect plugin's way of saying "inventory full" (it wants a chest to empty into, and I give it none)
export const mineFailure = message => /no defined chest locations/i.test(message) ? 'your inventory is full: deposit or drop something, then mine again' : message

// a lead must end on a spot to stand on: a marker set on the fence line ends the walk OUTSIDE the pen
export const leadTargetError = (to, block) => block?.solid ? `flock.lead: ${to.x},${to.y},${to.z} is inside a ${block.name}, not a spot to stand on: give a free floor cell INSIDE the pen (or mark the place again there)` : null

// which of the blocks found `mine` goes for. Never inside a protected zone; and not in or beside water unless asked: the walk to a
// submerged block ends in a pocket under water, the air reflex cancels the task, and three bodies drowned on 09-19 fetching sand
export function mineTargets ({ nearby, wanted, inZone, wet, allowWet, what, placed = () => false }) {
  if (!nearby.length) return { error: `no more ${what}` }
  const free = nearby.filter(p => !inZone(p))
  if (!free.length) return { error: `the only ${what} is inside protected zones (someone's build): go further away and retry` }
  const grown = free.filter(p => !placed(p))
  if (!grown.length) return { error: `the only ${what} is placed wood (someone's build: posts, beams), not trees: go where trees grow and retry` }
  const dry = grown.filter(p => allowWet || !wet(p))
  if (!dry.length) return { error: `the only ${what} lies in or next to water, where mining bodies drown: dig it block by block from the shore (dig x= y= z=), or pass wet=true if you take the risk` }
  const skippedWet = grown.length - dry.length
  const skippedPlaced = free.length - grown.length
  return { found: dry.slice(0, wanted), ...(skippedWet ? { skippedWet } : {}), ...(skippedPlaced ? { skippedPlaced } : {}) }
}

// A log is a tree's when leaves grow within 2 of it or of the top of its log column, and nothing built touches the
// column. A house post (stairs on top, planks against it) is not: mine.get *_log cut a cherry village corner post at
// 61,68,-184 and put it back on the wrong axis. `nameAt` takes {x,y,z} and gives a block name
const cube = r => [...Array(2 * r + 1).keys()].map(i => i - r).flatMap(dx => [...Array(2 * r + 1).keys()].map(i => i - r).flatMap(dy => [...Array(2 * r + 1).keys()].map(i => i - r).map(dz => [dx, dy, dz])))
export function isTreeLog (p, nameAt) {
  const isLog = at => /_log$/.test(nameAt(at) ?? '')
  const up = (c, dy) => ({ x: c.x, y: c.y + dy, z: c.z })
  const reach = dir => { const out = []; for (let c = up(p, dir); out.length < 16 && isLog(c); c = up(c, dir)) out.push(c); return out }
  const above = reach(1)
  const column = [...reach(-1), p, ...above]
  const top = above.at(-1) ?? p
  const around = (c, r) => cube(r).map(([dx, dy, dz]) => nameAt({ x: c.x + dx, y: c.y + dy, z: c.z + dz }) ?? 'air')
  const built = column.some(c => around(c, 1).some(looksBuilt))
  const leaves = [p, top].some(c => around(c, 2).some(n => /_leaves$/.test(n)))
  return leaves && !built
}

// on: the block I stand in, above: the bounding box two cells up. A bed is 0.56 high, so a ceiling 2 above the floor leaves 1.44: nobody fits
export const bedTrap = (on, above) => /_bed$/.test(on ?? '') && above === 'block'
  ? 'you are standing ON a bed under a low ceiling (1.4 blocks of headroom, nobody fits, so no walk can start): dig the bed, walk out, place it back. For good: leave one free floor cell beside the bed, or raise the ceiling over it by one'
  : null

// beds: nearest first. A bed in another agent's zone is theirs (zones are named owner-something; starter-* is shared, and so is
// a *-village: protected from digging, not owned, its beds are for any body caught out at night) and a bed holds one sleeper
export function bedChoice (beds, zones, me, any = false, occupied = new Set()) {
  if (!beds.length) return { error: 'no bed within 32 blocks' }
  const shared = name => new RegExp(`^(${me.toLowerCase()}|starter)-|-village$`).test(name)
  const theirs = bed => zones.find(z => inAnyZone([z], bed) && !shared(z.name))
  const free = beds.filter(b => !occupied.has(`${b.x},${b.y},${b.z}`))
  const bed = any ? free[0] : free.find(b => !theirs(b))
  if (bed) return { bed }
  const spare = 'Place your own (spare beds are in the starter chest at 112,70,-138)'
  if (any || beds.some(b => !theirs(b))) return { error: `every bed you may use within 32 blocks is occupied: a bed holds one sleeper. ${spare}` }
  return { error: `the only bed within 32 blocks is in ${theirs(beds[0]).name}: that is their bed, and a bed holds one sleeper. ${spare}, or sleep any=true if they invited you` }
}
// the bedtime reflex failed with this error: what to tell the driver (null: nothing, the driver's own order took over)
export const bedtimeReport = error => /^cancelled: superseded/.test(error)
  ? null
  : /monsters nearby/.test(error) ? `${error}: the server lets nobody sleep with a monster within 8 blocks of the bed. Kill it (attack mob=<its name>) and sleep again, or wait it out indoors; walls and light around the bed keep them off` : error
const fluidCure = 'Scoop the source with fill x= y= z= (an empty bucket), or fill the cell in with place item=dirt'

// target: the block to dig. above: the names of the 3 blocks over it. Water beside it is fine (a trench by a pond); water over it means a dive
export const digRefusal = (target, above, allowWet) => {
  if (FLUIDS.has(target)) {
    const burns = target === 'lava' || target === 'flowing_lava'
    return `${target} is a fluid, not a block: digging it never finishes (one such dig ran 167 seconds before it was cancelled)${burns ? ', and it burns whatever reaches into it' : ''}. ${fluidCure}`
  }
  return !allowWet && above.includes('water')
    ? 'that block is under water: the body would dive for it and run out of air. Work from the shore (dig down beside it, or drain it with sand or dirt first), or pass wet=true if it is shallow and you watch your air'
    : null
}

// clear sweeps a box: one fluid cell in it would hang the whole sweep, so they are skipped and named. counts: {water: 3}
export const fluidsLeft = counts => {
  const named = Object.entries(counts).sort((a, b) => b[1] - a[1]).map(([name, n]) => `${name} x${n}`)
  return named.length ? `${named.join(', ')} left in the box: a fluid cannot be dug. ${fluidCure}` : null
}

// floor: the "x,y,z" cells pen.check walked; animals: {name,x,y,z}. In = standing in a column of the pen, whatever the height
const onFloor = floor => {
  const columns = new Set(floor.map(k => k.split(',').filter((_, i) => i !== 1).join(',')))
  return p => columns.has(`${Math.floor(p.x)},${Math.floor(p.z)}`)
}
// the animals that are not already standing in the pen (floor = penLeak's floor cells, null when the goal is no pen)
export const unpenned = (floor, animals, posOf) => floor ? animals.filter(a => !onFloor(floor)(posOf(a))) : animals
// a pen that leaks (penLeak's via) through nothing but an open gate: where that gate is, so lead can shut it and see the pen
export function gateLeak (via, blockAt) {
  const spots = via.split(' ')
  if (spots.length !== 1) return null
  const [x, y, z] = spots[0].split(',').map(n => Math.floor(Number(n)))
  const block = blockAt(x, y, z)
  return block?.open && /_fence_gate$/.test(block.name) ? [x, y, z] : null
}
// where to stand in a pen so that animals following 2.5 blocks behind end up inside it: the floor cell furthest from them
export function deepestCell (floor, from) {
  const far = ([x, , z]) => (x + 0.5 - from.x) ** 2 + (z + 0.5 - from.z) ** 2
  return floor.map(k => k.split(',').map(Number)).sort((a, b) => far(b) - far(a))[0]
}
// farm animals that are NOT on the pen floor but within a few blocks of its gate: the ones that slipped out with whoever just walked through
export function strays (floor, animals, [gx, , gz], within = 6) {
  const out = animals.filter(a => !onFloor(floor)(a) && Math.hypot(a.x - gx - 0.5, a.z - gz - 0.5) <= within)
  return out.length ? out.map(a => `${a.name}@${Math.floor(a.x)},${Math.floor(a.y)},${Math.floor(a.z)}`).join(' ') : null
}
export function penCensus (floor, animals) {
  const isIn = onFloor(floor)
  const counts = animals.filter(isIn).reduce((n, a) => ({ ...n, [a.name]: (n[a.name] ?? 0) + 1 }), {})
  const inside = Object.entries(counts).map(([name, n]) => `${name}:${n}`).join(' ')
  const outside = animals.filter(a => !isIn(a)).map(a => `${a.name}@${Math.floor(a.x)},${Math.floor(a.y)},${Math.floor(a.z)}`).join(' ')
  return { ...(inside ? { inside } : {}), ...(outside ? { outside } : {}) }
}

// mineflayer-auto-eat marks itself eating BEFORE it equips the food and only unmarks after the meal: an equip that throws leaves it "eating" for
// ever and the body starves with bread in its pockets (Jizo). A meal takes 1.6 s
export const eatJammed = eatingForMs => eatingForMs >= 15000

// #149(a). `eatBelow(health)` raises minHunger as health drops, which sends a HURT body to eat - and a hurt body is
// usually a body in a fight. Mariel died at food 17 with a sword in her pack: 42 meals in the minute the zombie took
// to kill her, each one putting bread in the hand the sword needed, and the fight and the run both went unarmed.
// Food is for after. Nothing here is about hunger: a body clear of all three eats as it always did.
export const eatHold = ({ hostileNear = false, fighting = false, fleeing = false } = {}) => {
  if (fighting) return 'a fight of my own is running: the sword stays in my hand until the mob is dead'
  if (fleeing) return 'a run is on: nothing goes in my hand until it is clear'
  if (hostileNear) return 'a hostile is within reach: the sword stays in my hand, food is for afterwards'
  return null
}

// #149(c): Mariel's "jammed, reset" arrived on the tick she died, and until then the meal held the sword out of her hand:
// every equip waits for a meal in flight (afterTheMeal). So a meal is dropped the moment eatHold would not have started
// it. Damage alone is not a reason: a starving body is hurt every 4 s, and the meal is its cure
export const mealToDrop = ({ eating = false, hostileNear = false, fighting = false, fleeing = false } = {}) =>
  eating ? eatHold({ hostileNear, fighting, fleeing }) : null
// strictErrors is off, so a meal that fails RESOLVES and reports only through eatFail. A meal I cancelled myself for a
// fight is not a failure and must not grow the backoff
export const mealFailed = error => !/manually cancel/i.test(String(error?.message ?? error ?? ''))

// #149(b): a meal that will not go down is not worth asking for every 3 s. Each consecutive failure waits twice as
// long, to half a minute; one meal that works clears the count (see eatFailedAt), so a fed body is never held back
export const EAT_BACKOFF_FIRST = 5000
export const EAT_BACKOFF_CAP = 30000
export const eatBackoff = failures => Math.min(EAT_BACKOFF_FIRST * 2 ** Math.max(0, Number(failures) - 1), EAT_BACKOFF_CAP)

// SAFETY. The plugin's reflex ends in `catch {}`: across every body's log there are 140 jam lines and not one word of why
// a meal never finished, while Chani's body walked 20 minutes at health 7 / food 4 with five carrots in its pockets. Its
// errors name its own internals and append the whole item object after a newline, so the advice comes first and the real
// first line after it: whoever reads eat_failed has to know what to DO.
const EAT_ADVICE = [
  [/couldn't find a choice/i, 'nothing I carry is food I am willing to eat'],
  [/^Failed to equip/i, 'the food would not go into my hand'],
  [/^Already eating/i, 'a meal was already running'],
  [/timed out/i, 'the server never said the meal finished'],
  [/switched early/i, 'my hand was emptied mid-meal (a reflex that re-equips?)'],
  [/manually canceled/i, 'the meal was called off']
]
// The reflex runs on every physics tick. While every meal was timing out that was 20 failed eats a second, each one a pair
// of window clicks at the server: after a failure it waits. A meal that works needs no cooldown, a fed body stops asking.
export const eatRetryDue = (failedAt, now, wait = 5000) => failedAt === null || now - failedAt >= wait

// A meal is 1.6 s of holding the food still, and anything that swaps my hand or right-clicks a block inside that window
// cancels it where the server counts: on ClaudeProbe an `equip wheat` 0.9 s into a meal left the bread uneaten and the
// food number where it was. The pathfinder does both whenever it is stuck -- a tool in hand before every dig, a gate
// worked by hand on every retry -- which is why Chani stood at health 7 with five carrots in her pockets until she dug
// the gate out, and ate five in a row the moment the stall ended. So the meal goes first: these calls wait for it. A
// meal always settles, it has a timeout of its own, so nothing here waits for ever.
export const afterTheMeal = (mealInFlight, fn) => async (...args) => {
  await (mealInFlight() ?? Promise.resolve()).catch(() => {})
  return fn(...args)
}

// What I carry, sorted the way the eat reflex sorts it. "nothing edible carried" told an agent nothing: a body holding
// rotten flesh and a body holding cobblestone got the same line, and neither knew whether to cook, to hunt, or to walk to
// a chest (backlog #134). isFood: does the game call it food at all. banned: what this body will never eat.
export function foodSort (carried, isFood, banned) {
  const seen = [...new Set(carried)]
  return {
    edible: seen.filter(name => isFood(name) && !banned.includes(name)),
    banned: seen.filter(name => isFood(name) && banned.includes(name)),
    notFood: seen.filter(name => !isFood(name))
  }
}

// The never-eat list is a preference, and a preference that outlives the body holding it is a bug. At food 3 with 8
// rotten flesh in its pockets this body was told it carried nothing edible, over and over, and walked 95 blocks to a
// bread chest to answer it (backlog #139). Hunger damage on Normal stops at half health, so the flesh costs nothing
// the starving was not already costing, and its own hunger cannot take a body below what the empty belly already
// would. So below the floor, with nothing better in the pockets, the banned food IS the food; and `anyway=true` lets
// the agent make that call itself at any hunger, because the agent can see reasons this code cannot.
export const HUNGER_FLOOR = 6
export const eatAllowed = ({ food, carried, anyway = false, floor = HUNGER_FLOOR }) => {
  const desperate = Boolean(anyway) || (food <= floor && !carried.edible.length)
  return { desperate, allowed: desperate ? [...carried.edible, ...carried.banned] : [...carried.edible] }
}

// ./mc eat: why I will not, said before the plugin is touched at all -- its own refusals name its internals. It names what
// it passed over, so the answer says what to do next: cook the flesh away, go and find real food, or say anyway=true.
const floorNote = floor => `, which I eat only at food ${floor} or less or with anyway=true`
const whyNot = ({ banned, notFood }, item, floor) => banned.includes(item) ? `it is on the never-eat list${floorNote(floor)}` : notFood.includes(item) ? 'it is not food' : 'I do not carry it'
export function eatRefusal ({ food, item, carried, anyway = false, floor = HUNGER_FLOOR }) {
  const { banned, notFood } = carried
  const { allowed } = eatAllowed({ food, carried, anyway, floor })
  const have = allowed.length ? `what I carry is ${allowed.join(', ')}` : 'I carry nothing edible'
  if (item && !allowed.includes(item)) return `no ${item} I would eat: ${whyNot(carried, item, floor)}; ${have}`
  if (!item && !allowed.length) {
    const skipped = [...(banned.length ? [`never eaten: ${banned.join(', ')}${floorNote(floor)}`] : []), ...(notFood.length ? [`not food: ${notFood.join(', ')}`] : [])]
    return skipped.length ? `nothing I carry is food (${skipped.join('; ')})` : 'nothing I carry is food: my pockets are empty'
  }
  return food >= 20 ? 'food is already 20: the game refuses a meal at a full belly' : null
}

// ./mc eat said ok with nothing eaten: a meal counts only if the item left my pockets
export const uneatenMeal = ({ item, before, after }) => !item || after < before ? null
  : `the meal did not happen: I still carry ${after} ${item}, as many as before. Something took the food out of my hand mid-meal (the gate reflex puts tempting food away; see [food away] in bot.log) or the server never finished it. Step 5 blocks clear of any fence gate and eat again`

// edible: what I carry that I would eat. With food in my pockets the plugin's "couldn't find a choice" is about my hand, not my pockets
export function eatFailure (error, edible = []) {
  const first = String(error?.message ?? error ?? '').split('\n')[0].trim()
  if (!first) return 'the eat failed and said nothing'
  if (edible.length && /couldn't find a choice/i.test(first)) return `I carry ${edible.join(', ')}, but the eat reflex found no meal to take (it looked while the food was being moved and in no slot: near a fence gate the gate reflex puts tempting food away): ${first}`
  const advice = EAT_ADVICE.find(([pattern]) => pattern.test(first))
  return advice ? `${advice[1]}: ${first}` : first
}

// will this bed let me go in the morning? The server wakes a sleeper ON the bed when it finds no better spot, 0.56 above the floor, and from there
// a cell with a block 2 above its floor cannot be entered (Aviendha, every morning). exits: the cells around the bed, { at: 'x,y,z', free, lintel }
export function bedExit (exits) {
  if (!exits.length || exits.some(e => e.free && !e.lintel)) return null
  const low = exits.find(e => e.free)
  if (!low) return 'this bed has no free cell beside it: you will wake up standing ON it with nowhere to step. Clear one cell next to it (feet and head, and the block above those)'
  const [x, y, z] = low.at.split(',').map(Number)
  return `this bed is a trap: you wake up standing ON it (0.56 high) and the only free cell beside it (${low.at}) has a block 2 above its floor, which leaves 1.44: nobody fits and no walk will start. Dig the block at ${x},${y + 2},${z} (or move the bed next to a cell with 3 of headroom). In the morning, if stuck: dig the bed, walk out, place it back`
}

// mine started inside a stocked pen: it digs down from where the body stands and the shaft stays (mine in the starter pen, 09-19)
export const penShaftRefusal = (inside, force) => inside && !force
  ? `you stand in a pen with animals in it (${inside}): mine digs its way down from where you stand and would leave a shaft for them to fall into. Walk out through the gate first, then mine (force=true if you really mean it, and fill the hole after)`
  : null

// the ground mine broke open around its start and left open: what to put back, with what. solid(name) says whether a block was ground at all
const FILLERS = ['dirt', 'cobblestone', 'cobbled_deepslate', 'stone', 'andesite', 'diorite', 'granite', 'netherrack']
export function holesLeft (before, after, carried, solid) {
  const item = FILLERS.find(f => carried.includes(f))
  if (!item) return []
  return Object.keys(before).filter(k => solid(before[k]) && !solid(after[k])).map(k => { const [x, y, z] = k.split(',').map(Number); return { x, y, z, item } })
}

// which animal a lead goes for: nearest first, but one standing in a pen belongs to somebody (my lead went for Aviendha's cow, 60 blocks off)
// and a calf is next year's herd, not this year's breeding pair. Perrin's `flock.lead count=2` out of a 24-cow herd
// delivered one adult and two calves, silently, and the breed that followed did nothing: the grown one is taken even
// when a calf stands nearer, and a calf is only taken when there was no grown one to take, which is said out loud.
export function leadPick (candidates, allowPenned, mob = 'animal') {
  const free = candidates.filter(c => allowPenned || !c.penned)
  const grown = free.find(c => c.grown !== false)
  if (grown) return { id: grown.id }
  const calf = free[0]
  if (calf) return { id: calf.id, note: `the only ${mob} in range is a calf (at ${calf.at}): it will not breed, and it stays with its herd until it grows` }
  return { error: candidates.length ? `the only ones in range stand in a pen (nearest at ${candidates[0].at}): they are somebody's. penned=true takes one anyway: only from the starter pen or a pen of your own` : 'none in range' }
}

// and which of the ones standing round me come along. Grown first, distance order kept within each: a calf only fills a
// place no grown animal was there to take. An age I could not read is not a reason to leave an animal behind.
export const herdOrder = herd => [...herd.filter(a => a.grown !== false), ...herd.filter(a => a.grown === false)]

// what the reply says came. `with=3` was true and useless: nothing in the line said the pen now holds one cow and two
// calves, so the driver bred an empty pair and saw nothing wrong.
export const ledReport = (mob, came) => {
  const calves = came.filter(a => a.grown === false).length
  return calves
    ? `${mob}:${came.length} (${came.length - calves} grown, ${calves} ${calves === 1 ? 'calf' : 'calves'}: a calf will not breed)`
    : `${mob}:${came.length}`
}

// A lead is walked with the food in my hand, and food in the hand is visible to every animal of its kind that can see
// me, not only to the ones that were picked. So a lead for two out of a big herd walks a queue in, and `with=2` was
// true and said nothing about the four others now standing in the pen (Perrin, item 17). Shedding them is not on
// offer: the food is what the walk is MADE of, and an animal that follows food cannot be told to stop. They are
// counted instead - by id, because one sheep is not told from another by looks or by where it stands. The ones that
// were in the pen before I arrived are not followers, and neither are the ones I asked for.
export function tagalongs (invited, before, now) {
  const known = new Set([...invited, ...before])
  return now.filter(id => !known.has(id)).length
}

export const ledExtra = (mob, extra) => extra
  ? {
      extra,
      extraNote: `${extra} more ${mob} followed the food in uninvited: ${extra === 1 ? 'it is' : 'they are'} in there too. Lead ${extra === 1 ? 'it' : 'them'} out, or feed the pen for ${extra === 1 ? 'one' : extra} more`
    }
  : {}

// ---------------------------------------------------------------- composite actions: plans
// A plan is an ASCII map of a farm or pen, one character per block, anchored at its NORTH-WEST corner: rows run south (z),
// columns east (x). `y` is the crop/floor level, so the ground under a cell is y-1: a crop stands at y on farmland at y-1,
// water lies AT y-1, a fence stands at y on dirt. The plan is the truth of what SHOULD be there; the world is what is.
// `ground` is what must lie at y-1; `seed` is the item a crop is planted from; `item` the block a structure is placed from.
export const PLAN_LEGEND = {
  w: { kind: 'crop', crop: 'wheat', seed: 'wheat_seeds', ground: 'farmland' },
  c: { kind: 'crop', crop: 'carrots', seed: 'carrot', ground: 'farmland' },
  p: { kind: 'crop', crop: 'potatoes', seed: 'potato', ground: 'farmland' },
  b: { kind: 'crop', crop: 'beetroots', seed: 'beetroot_seeds', ground: 'farmland' },
  s: { kind: 'crop', crop: 'sugar_cane', seed: 'sugar_cane', ground: 'sand' },
  m: { kind: 'crop', crop: 'melon_stem', seed: 'melon_seeds', ground: 'farmland' },
  k: { kind: 'crop', crop: 'pumpkin_stem', seed: 'pumpkin_seeds', ground: 'farmland' },
  B: { kind: 'crop', crop: 'bamboo', seed: 'bamboo', ground: 'dirt' },
  // a channel is built COVERED: a slab laid in the source keeps the water (and the farmland wet) but leaves a floor to
  // walk on. Open water in a field is a trap - the body wades in, `dig` refuses every block beside it, and the
  // pathfinder will not cross it, which is how Chani ended up walled into her own plan
  '~': { kind: 'water', ground: 'water', cover: 'oak_slab' },
  '.': { kind: 'path', ground: 'dirt' },
  '#': { kind: 'fence', item: 'oak_fence', ground: 'dirt' },
  G: { kind: 'gate', item: 'oak_fence_gate', ground: 'dirt' },
  T: { kind: 'torch', item: 'oak_fence', ground: 'dirt' },
  C: { kind: 'chest', item: 'chest', ground: 'dirt' },
  K: { kind: 'composter', item: 'composter', ground: 'dirt' },
  F: { kind: 'flower', item: 'dandelion', ground: 'grass_block' },
  t: { kind: 'sapling', item: 'oak_sapling', ground: 'dirt' },
  A: { kind: 'table', item: 'crafting_table', ground: 'dirt' }
}
const PLAN_MAX = 64

// the ASCII map as rows and cells; a space is a hole in the plan, not a cell
export function parsePlan (map) {
  const rows = String(map ?? '').replace(/\t/g, ' ').split('\n')
  const first = rows.findIndex(r => r.trim())
  if (first < 0) return { error: 'the map has no cells: give rows of legend characters, one character per block' }
  const last = rows.length - [...rows].reverse().findIndex(r => r.trim())
  const kept = rows.slice(first, last).map(r => r.replace(/\s+$/, ''))
  const width = Math.max(...kept.map(r => r.length))
  if (width > PLAN_MAX || kept.length > PLAN_MAX) return { error: `a plan is at most ${PLAN_MAX}x${PLAN_MAX} blocks (got ${width}x${kept.length})` }
  return { rows: kept, width, height: kept.length, cells: kept.flatMap((row, dz) => [...row].flatMap((ch, dx) => ch === ' ' ? [] : [{ dx, dz, ch }])) }
}

// Every cell in world coordinates: x east of the anchor, z south of it, y the GROUND block — the farmland, pen floor
// or path the plan describes, the level `till` asks for. What the plan puts on it (crop, fence, gate, torch, chest,
// composter, flower, sapling) stands at y+1; a water source lies AT y, with its cover at y+1.
export const planCells = place => (parsePlan(place.plan).cells ?? []).map(c => ({ ...c, x: place.x + c.dx, y: place.y, z: place.z + c.dz }))

// farmland stays wet within 4 blocks of a water source, level with it or one above it: a plan that breaks that rule
// turns back into dirt within minutes of being built. A gate in a corner is one nothing can ever walk through (see blindGates)
const BARRIER_KINDS = new Set(['fence', 'gate'])
export function planErrors (parsed) {
  if (parsed.error) return [parsed.error]
  const unknown = parsed.cells.filter(c => !PLAN_LEGEND[c.ch])
  if (unknown.length) return unknown.map(c => `${c.ch} at ${c.dx},${c.dz} is not in the legend (${Object.keys(PLAN_LEGEND).join(' ')})`)
  const waters = parsed.cells.filter(c => PLAN_LEGEND[c.ch].kind === 'water')
  const dry = parsed.cells.filter(c => PLAN_LEGEND[c.ch].ground === 'farmland' &&
    !waters.some(w => Math.abs(w.dx - c.dx) <= 4 && Math.abs(w.dz - c.dz) <= 4))
  const at = (dx, dz) => parsed.cells.find(c => c.dx === dx && c.dz === dz)
  const kindAt = (dx, dz) => PLAN_LEGEND[at(dx, dz)?.ch]?.kind ?? null
  const walkable = kind => kind && !BARRIER_KINDS.has(kind)
  const blind = parsed.cells.filter(c => PLAN_LEGEND[c.ch].kind === 'gate').filter(g =>
    ![[1, 0], [-1, 0], [0, 1], [0, -1]].some(([dx, dz]) =>
      walkable(kindAt(g.dx + dx, g.dz + dz)) && !BARRIER_KINDS.has(kindAt(g.dx - dx, g.dz - dz))))
  // one complaint for the whole dry patch: a plan that forgot its channel used to answer with a line per cell
  const dryLine = dry.length
    ? [`${dry.length} cell${dry.length === 1 ? ' is' : 's are'} farmland with no water within 4 blocks (${dry.slice(0, 4).map(c => `${c.dx},${c.dz}`).join(' ')}${dry.length > 4 ? ` and ${dry.length - 4} more` : ''}): move the channel or shorten the row`]
    : []
  return [
    ...dryLine,
    ...blind.map(g => `the gate at ${g.dx},${g.dz} is in a corner: nothing can walk through it. Put it in the middle of a wall`)
  ]
}

// A walk into a field steps AROUND planted cells rather than trample them, and a job stands within WORK_RANGE of the
// cell it works: one up and four across is 4.12, so a lane four blocks from a bed serves it and a covered channel every
// eight rows serves a whole field. A crop with nothing to stand on within that reach is a crop no job can be done on:
// `goto` beside it answers "nowhere to stand" and so does every till, plant and pour there. Chani's carrot patch
// sandwiched its water row between two carrot rows and left nothing but crops between the gate and the far row, and she
// took her own plan's fault for a tool bug (BUGS.md 09-23); jizo-melon-patch's census called 140 cells stranded for want
// of a cell BESIDE them, on a field a sweep had just harvested end to end from its channels (09-26). A plan can be told
// this before it is built, and a field that already stands can be asked. Stood on: a path, a covered channel, a gate, a
// flower, a sapling - the ones a body passes without breaking - and anything outside the plan, because the plan says
// nothing about it and the ground around a farm is where a walk starts from. Reached over: crops, which are seen over
// and walked round. In the way of the arm as of the walk: fences, gate panels seen from outside, torch posts, chests,
// composters, tables - anything taller than a crop on the straight line between the lane cell and the bed.
const LANE_KINDS = new Set(['path', 'water', 'gate', 'flower', 'sapling'])
// the arm works a bed from a lane cell one up and beside it: dx²+dz²+1 <= WORK_RANGE², four across at most
const LANE_REACH2 = WORK_RANGE * WORK_RANGE - 1
const LANE_REACH = Math.floor(Math.sqrt(LANE_REACH2))
// seen over from a lane: crops, and every lane kind but a gate, whose panel is a fence's height when it is not the cell stood in
const SEEN_OVER = new Set(['crop', 'path', 'water', 'flower', 'sapling'])
const tallKind = kind => Boolean(kind) && !SEEN_OVER.has(kind)
// nothing taller than a crop on the line between two cell centres, sampled every quarter block
const clearBetween = (kindAt, from, to) => {
  const steps = Math.ceil(Math.hypot(to.dx - from.dx, to.dz - from.dz) * 4)
  return Array.from({ length: Math.max(steps - 1, 0) }, (_, i) => (i + 1) / steps)
    .map(t => [Math.floor(from.dx + 0.5 + (to.dx - from.dx) * t), Math.floor(from.dz + 0.5 + (to.dz - from.dz) * t)])
    .filter(([dx, dz]) => !(dx === from.dx && dz === from.dz) && !(dx === to.dx && dz === to.dz))
    .every(([dx, dz]) => !tallKind(kindAt(dx, dz)))
}
export function planLane (cells) {
  const crops = (cells ?? []).filter(c => PLAN_LEGEND[c.ch]?.kind === 'crop')
  if (!crops.length) return {}
  const key = (dx, dz) => `${dx},${dz}`
  const map = new Map(cells.map(c => [key(c.dx, c.dz), c]))
  const kindAt = (dx, dz) => PLAN_LEGEND[map.get(key(dx, dz))?.ch]?.kind ?? null
  const bounds = ['dx', 'dz'].map(k => [Math.min(...cells.map(c => c[k])) - 1, Math.max(...cells.map(c => c[k])) + 1])
  const inside = (dx, dz) => dx >= bounds[0][0] && dx <= bounds[0][1] && dz >= bounds[1][0] && dz <= bounds[1][1]
  const standable = (dx, dz) => {
    const cell = map.get(key(dx, dz))
    return !cell || LANE_KINDS.has(PLAN_LEGEND[cell.ch]?.kind)
  }
  // flood in from the ring around the plan, which is all ground the plan never claimed
  const reached = new Set()
  const queue = []
  for (let dx = bounds[0][0]; dx <= bounds[0][1]; dx++) for (const dz of bounds[1]) queue.push([dx, dz])
  for (let dz = bounds[1][0]; dz <= bounds[1][1]; dz++) for (const dx of bounds[0]) queue.push([dx, dz])
  while (queue.length) {
    const [dx, dz] = queue.pop()
    if (reached.has(key(dx, dz)) || !inside(dx, dz) || !standable(dx, dz)) continue
    reached.add(key(dx, dz))
    for (const [sx, sz] of STEPS) queue.push([dx + sx, dz + sz])
  }
  const lanes = [...reached].map(k => k.split(',').map(Number)).map(([dx, dz]) => ({ dx, dz }))
  const served = crop => lanes.some(lane =>
    (lane.dx - crop.dx) ** 2 + (lane.dz - crop.dz) ** 2 <= LANE_REACH2 && clearBetween(kindAt, lane, crop))
  const stranded = crops.filter(c => !served(c))
  if (!stranded.length) return {}
  const named = stranded.slice(0, 4).map(c => `${c.dx},${c.dz}`).join(' ')
  const more = stranded.length > 4 ? ` and ${stranded.length - 4} more` : ''
  const subject = stranded.length === 1
    ? `1 crop cell has nothing to stand on within ${LANE_REACH} of it`
    : `${stranded.length} crop cells have nothing to stand on within ${LANE_REACH} of them`
  return { noLane: `${subject} (${named}${more}): lay a . path or a ~ channel through the rows, eight rows apart at most, or every job there answers nowhere to stand` }
}

// what it takes to build this plan from nothing: one water bucket does the whole field, a torch cell needs its post too
export function planBill (parsed) {
  const bill = {}
  const add = (item, n = 1) => { bill[item] = (bill[item] ?? 0) + n }
  for (const { ch } of parsed.cells ?? []) {
    const cell = PLAN_LEGEND[ch]
    if (!cell) continue
    if (cell.kind === 'crop') add(cell.seed)
    if (cell.cover) add(cell.cover)
    if (cell.kind === 'torch') add('torch')
    if (cell.item) add(cell.item)
  }
  if ((parsed.cells ?? []).some(c => PLAN_LEGEND[c.ch]?.kind === 'water')) add('water_bucket')
  return bill
}

// one line: how big it is and what is in it
export function planSummary (parsed) {
  if (parsed.error) return parsed.error
  const counts = {}
  for (const { ch } of parsed.cells) {
    const cell = PLAN_LEGEND[ch]
    const label = cell?.kind === 'crop' ? cell.crop.replace(/s$/, '') : cell?.kind ?? ch
    counts[label] = (counts[label] ?? 0) + 1
  }
  return `${parsed.width}x${parsed.height} ${compact(counts)}`
}

// ---------------------------------------------------------------- composite actions: what a farm needs
// worldAt(x,y,z) answers { name, properties } or null (not loaded). Both of these are read-only judgements: no walking, no digging.
// what stands on a farm right now, judged against its plan
export function fieldCensus (cells, worldAt) {
  const out = { crops: {}, cells: cells.length, ripe: 0, growing: 0, empty: 0, untilled: 0, dry: 0 }
  for (const cell of cells) {
    const spec = PLAN_LEGEND[cell.ch]
    if (!spec) continue
    const ground = worldAt(cell.x, cell.y, cell.z)
    const here = worldAt(cell.x, cell.y + 1, cell.z)
    if (spec.kind === 'water') {
      if (ground && !holdsWater(ground)) out.dry++
      // open water in a field is a hole: the body wades in, `dig` refuses the blocks beside it and no walk will cross it
      else if (ground?.name === 'water') out.open = (out.open ?? 0) + 1
      continue
    }
    if (spec.kind !== 'crop') continue
    if (spec.ground === 'farmland' && ground && ground.name !== 'farmland') out.untilled++
    if (here?.name !== spec.crop) { out.empty++; continue }
    out.crops[spec.crop] = (out.crops[spec.crop] ?? 0) + 1
    if (ripeCrop(here.name, here.properties?.age)) out.ripe++
    else out.growing++
  }
  // and whether the body can stand next to what it is being asked to work: read off the plan, not the world, because a
  // lane is a property of the SHAPE - a field with no lane through it has none whether or not its crops have grown
  return { ...out, ...planLane(cells) }
}

// A plan whose y is one off reads as a field of empty, untilled beds (Chani's wheat field: 28 wheat stood one block
// above where the code looked) or has a build dig the turf out and lay its floor one lower (her sheep pen). A plan's y
// is the GROUND block, and this answers `off`: what to add to that y to reach the level the world is really at.
// Two ways to tell, in order:
//  - the world's own copy of the plan, standing one block up or one down. Water is no evidence of a shift on its own
//    (a channel reads as water either way), and one matching block is a coincidence, so it takes two.
//  - nothing built there yet, but every cell the plan names is open air (or grass) over solid ground: that is the level
//    you STAND on, one above the ground block a plan wants.
// The legend names ONE wood for every wooden thing - oak_fence, oak_fence_gate, oak_slab, oak_sapling - because a plan
// is a drawing, not a shopping list. A pen actually built of birch or cherry fence is the same pen. Until this, the
// levelling pass called every post of such a wall a boulder standing in the cell and dug the wall out, and the anchor
// check found no evidence of the plan at all, so a field one block low read as bare ground.
const WOODS = ['dark_oak', 'pale_oak', 'oak', 'spruce', 'birch', 'jungle', 'acacia', 'mangrove', 'cherry', 'bamboo', 'crimson', 'warped']
const woodless = name => {
  const wood = WOODS.find(w => String(name).startsWith(`${w}_`))
  return wood ? String(name).slice(wood.length + 1) : null
}
export const sameFamily = (want, got) => {
  if (!want || !got) return false
  if (want === got) return true
  const [a, b] = [woodless(want), woodless(got)]
  return Boolean(a) && a === b
}

const anchorHit = (spec, here) => {
  if (!here) return false
  if (spec.kind === 'crop') return here.name === spec.crop
  if (spec.kind === 'gate') return here.name.endsWith('_fence_gate')
  return sameFamily(spec.item, here.name)
}
const CONVENTION = "a plan's y is the GROUND block (the farmland, pen floor or path itself; crops, fences, gates, chests and a water cover stand at y+1)"
// what you can stand in: air, or the grass and flowers that grow on open ground
const isOpenCell = name => isAir(name) || isGroundCover(name) || WEEDS.has(name)
// what you can stand on
const isFooting = name => Boolean(name) && !isOpenCell(name) && name !== 'water' && name !== 'lava'
function freshGround (cells, worldAt, y) {
  const seen = cells.filter(c => PLAN_LEGEND[c.ch] && worldAt(c.x, c.y, c.z))
  const standing = seen.filter(c => isOpenCell(worldAt(c.x, c.y, c.z).name) && isFooting(worldAt(c.x, c.y - 1, c.z)?.name))
  if (seen.length < 2 || standing.length !== seen.length) return { off: 0 }
  return {
    off: -1,
    fresh: standing.length,
    note: `the plan says y=${y}, but all ${standing.length} of its cells are open air over solid ground at y=${y - 1}: you gave the level you stand on. ${CONVENTION}, so re-save it with y=${y - 1}`
  }
}
// planAnchor looks one block up and one block down, and nowhere to the side: a pen ring standing a few cells ACROSS
// from its plan is bare ground as far as it can tell. Chani's pen was marked 2 east and 3 south of the ring it
// describes, so pen.build read an empty field, laid half a second ring through the middle of the first and let 4
// sheep out (2026-09-23 02:55Z). This is the sideways half of the same question: where does the thing the plan
// describes actually stand? Answers null unless a shift within `reach` explains more than half the plan's solid
// cells and more of them than the plan's own spot does - a neighbour's wall brushing the plan is not its own ring.
const compassOf = (dx, dz) => [dx && `${Math.abs(dx)} ${dx < 0 ? 'west' : 'east'}`, dz && `${Math.abs(dz)} ${dz < 0 ? 'north' : 'south'}`]
  .filter(Boolean).join(' and ')
export function planBeside (cells, worldAt, { name = 'the place', reach = 3 } = {}) {
  const solidCells = cells.filter(c => PLAN_LEGEND[c.ch] && PLAN_LEGEND[c.ch].kind !== 'path' && PLAN_LEGEND[c.ch].kind !== 'water')
  if (solidCells.length < 6) return null
  const score = (dx, dy, dz) => solidCells.filter(c => anchorHit(PLAN_LEGEND[c.ch], worldAt(c.x + dx, c.y + 1 + dy, c.z + dz))).length
  // the plan's own spot, judged the way planAnchor judges it: a build that is simply unfinished is not a plan in the
  // wrong place, and a plan that already stands where it says is not searched for at all (the whole of a big farm, every shift)
  const here = Math.max(...[0, -1, 1].map(dy => score(0, dy, 0)))
  if (here === solidCells.length) return null
  const shifts = range(-reach, reach).flatMap(dx => range(-reach, reach).flatMap(dz =>
    dx === 0 && dz === 0 ? [] : [0, -1, 1].map(dy => ({ dx, dy, dz, found: score(dx, dy, dz) }))))
  const away = s => Math.abs(s.dx) + Math.abs(s.dz) + Math.abs(s.dy)
  const best = [...shifts].sort((a, b) => b.found - a.found || away(a) - away(b))[0]
  if (!best || best.found < Math.max(6, solidCells.length / 2) || best.found <= here) return null
  const at = { x: Math.min(...cells.map(c => c.x)) + best.dx, y: cells[0].y + best.dy, z: Math.min(...cells.map(c => c.z)) + best.dz }
  return {
    ...best,
    at,
    note: `${best.found} of the ${solidCells.length} blocks it describes stand ${compassOf(best.dx, best.dz)} of where the plan puts them, on ground at y=${at.y}: what the plan describes is already built, just not where the place is marked. A place's x,z is the NORTH-WEST corner cell of its plan (its lowest x and lowest z) and its y is the GROUND block, so mark it where the thing itself stands and build again: ./mc mark name=${name} x=${at.x} y=${at.y} z=${at.z} (marking again keeps the plan). If you meant to build a SECOND one here, move the plan further off: this one would run through the middle of what stands`
  }
}
// Does what a plan describes stand at its own anchor? Half its solid cells (fences, gates, crops, chests...) at least,
// and two: the evidence a mark needs before it moves a place off something built (see markMove)
export function planStands (cells, worldAt) {
  const solidCells = cells.filter(c => PLAN_LEGEND[c.ch] && PLAN_LEGEND[c.ch].kind !== 'path' && PLAN_LEGEND[c.ch].kind !== 'water')
  const found = solidCells.filter(c => [0, -1, 1].some(dy => anchorHit(PLAN_LEGEND[c.ch], worldAt(c.x, c.y + 1 + dy, c.z)))).length
  return found >= 2 && found >= solidCells.length / 2
}
// The GROUND a plan's cells sit on is evidence its own contents cannot spoil. Chani's carrot patch was stored at y=72
// over farmland built at y=71, and nothing caught it: farm.maintain got stuck twice on ten cells it could not till
// (BUGS.md 09-24 01:23Z). The crop test could not see it, because somebody had planted wheat in a patch the plan calls
// carrots, so no cell matched at any level and the whole check fell through to "no evidence". A crop cell is farmland
// whatever grew in it, and a channel is water whoever covered it. Only those two count: the `dirt` under a path is the
// same dirt one block down and one block up, and scoring it would invent a shift under every plan laid on soil.
const hasGround = spec => Boolean(spec) && (spec.kind === 'water' || spec.ground === 'farmland')
const groundHit = (spec, here) => {
  if (!here) return false
  return spec.kind === 'water' ? holdsWater(here) : here.name === 'farmland'
}
export function planAnchor (cells, worldAt) {
  const solidCells = cells.filter(c => PLAN_LEGEND[c.ch] && PLAN_LEGEND[c.ch].kind !== 'path' && PLAN_LEGEND[c.ch].kind !== 'water')
  const groundCells = cells.filter(c => hasGround(PLAN_LEGEND[c.ch]))
  const score = dy =>
    solidCells.filter(c => anchorHit(PLAN_LEGEND[c.ch], worldAt(c.x, c.y + 1 + dy, c.z))).length +
    groundCells.filter(c => groundHit(PLAN_LEGEND[c.ch], worldAt(c.x, c.y + dy, c.z))).length
  const here = score(0)
  const best = [{ dy: 1, n: score(1) }, { dy: -1, n: score(-1) }].sort((a, b) => b.n - a.n)[0]
  const y = cells[0]?.y
  if (!best || best.n < 2 || best.n <= here) return cells.length ? freshGround(cells, worldAt, y) : { off: 0 }
  return {
    off: best.dy,
    found: best.n,
    note: `the plan says y=${y}, but ${best.n} of the cells it describes match the world one block ${best.dy > 0 ? 'UP' : 'DOWN'}: ${CONVENTION}, so re-save it with y=${y + best.dy}`
  }
}

// the work that would put a farm back the way its plan says, in the order it has to happen: clear the bed, till it,
// refill the channel, plant, then build what is missing. A cell with somebody's block on it is left alone.
// what may be broken to clear a crop bed: weeds and the flowers that spring up around bone meal. A crop cell holding anything
// else (cobblestone, someone's torch) is left alone and shows up as `empty` in the census instead.
const WEEDS = new Set(['short_grass', 'tall_grass', 'fern', 'large_fern', 'dead_bush', 'snow', 'dandelion', 'poppy', 'cornflower',
  'oxeye_daisy', 'azure_bluet', 'blue_orchid', 'allium', 'lily_of_the_valley', 'red_tulip', 'orange_tulip', 'white_tulip', 'pink_tulip',
  'bush', 'firefly_bush', 'leaf_litter', 'wildflowers', 'short_dry_grass', 'tall_dry_grass'])
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

// Where the block a plan marks with one character really stands: the plan's y is the ground it sits on, so a chest,
// composter or torch is at y+1. Every composite that walks to one asks for it this way.
export function planStructure (cells, ch) {
  const cell = cells.find(c => c.ch === ch)
  return cell ? { x: cell.x, y: cell.y + 1, z: cell.z } : null
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
  goto: { section: 'move', args: 'place= | player= | x= z= [y=] [range=] [dig=]', doc: 'walk there, opening doors and swimming; it does not dig or bridge unless dig=true' },
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
  sleep: { section: 'self', args: '[any=]', doc: 'sleep in the nearest free bed within 32 blocks' },
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
