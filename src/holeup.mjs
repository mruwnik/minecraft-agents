// The hole-up's site check and its refusals (card c3f387d7). Pure: nothing here reads a body or touches a block.
//
// A body (15:02Z) holed up on a farm beside a water channel: the three cells straight below were dirt, so the column check
// said dig, and the channel's water one cell to the side poured into the shaft as it went down; it surfaced for air and could
// not climb out. The floor is not only the column: whatever stands beside the shaft flows into it. And a bad site is no
// reason to wall in on the spot when a dry cell stands a step away, or when the pack holds three blocks for a pillar (a
// creeper is out-waited from above as well as from below).
import { HOLE_DEPTH, HOLE_UNSAFE, HOLE_MELEE, HOLE_HURT_MS, holeUpRefusal, holedUpNote, isGroundCover } from './lib.mjs'
export { HOLE_DEPTH, HOLE_MELEE } from './lib.mjs'

export const HOLE_STEP = 2 // how far out (in cells) a dry site is looked for
const FLUIDS = new Set(['water', 'flowing_water', 'lava', 'flowing_lava', 'bubble_column'])
const CAVE = new Set(['air', 'cave_air', 'void_air'])
const unreadable = name => name === null || name === undefined
const fluidIn = names => names.find(n => FLUIDS.has(String(n))) ?? null

// The column under a cell and the four cells beside each of its depths. `beside[d]` is what stands around depth d+1: fluid at
// any depth pours in; open air beside the two deepest cells is a cave the hole would stand open to, unless there is
// something to wall it with (the ring closes those sides). Air beside the top cell is only the ground stepping down
export function floorVerdict ({ below = [], beside = [], cap = false }) {
  const floor = below.slice(0, HOLE_DEPTH)
  if (floor.length < HOLE_DEPTH || floor.some(unreadable)) return 'a floor I could not read under my feet'
  const bad = floor.find(name => HOLE_UNSAFE.has(String(name)))
  if (bad) return `${bad} under my feet`
  const fluid = fluidIn(beside.flat())
  if (fluid) return `${fluid} beside the shaft`
  const cave = !cap && beside.slice(1, HOLE_DEPTH).flat().some(name => CAVE.has(String(name)))
  return cave ? 'a shaft open to a cave beside it, with nothing to wall it' : null
}

const dirName = ({ dx, dz }) => Math.abs(dx) >= Math.abs(dz) ? (dx > 0 ? 'east' : 'west') : (dz > 0 ? 'south' : 'north')
const nearer = (a, b) => Math.hypot(a.dx, a.dz) - Math.hypot(b.dx, b.dz)

// Where the hole goes: here when the floor is good; else the nearest standable cell whose floor is good; else a pillar of
// HOLE_DEPTH blocks when the pack holds them; else the old fallback, walled in where the body stands
export function burrowSite ({ here = {}, around = [], blocks = 0 }) {
  const cap = blocks > 0
  const bad = floorVerdict({ ...here, cap })
  if (!bad) return { way: 'dig', step: null, why: null }
  const site = around.filter(s => s.standable).sort(nearer).find(s => !floorVerdict({ ...s, cap }))
  if (site) return { way: 'dig', step: { dx: site.dx, dz: site.dz }, why: `${bad}: one step ${dirName(site)} the floor is sound, so I am digging there instead` }
  // a pillar needs a solid block under the feet to build from: in water there is nothing to click on (18:22Z, a flooded shaft)
  const footing = here.below?.[0] && !HOLE_UNSAFE.has(String(here.below[0]))
  if (blocks >= HOLE_DEPTH && footing) return { way: 'pillar', step: null, why: `${bad} and no sound floor within ${HOLE_STEP}: going up instead, a pillar of ${HOLE_DEPTH}` }
  return { way: 'wall', step: null, why: `${bad}, no sound floor within ${HOLE_STEP} and only ${blocks} blocks to build with: walling myself in where I stand` }
}

// The cap: a full solid block only. Ground cover has no box, a gravity block falls on the head, and doors, gates and beds
// are not walls. A body (17:27Z) capped its hole with leaf_litter, the first block-shaped item in its pack
const NOT_A_CAP = /_bed$|_gate$|_door$|torch|sapling|sand$|gravel/
export const capChoice = items => items.find(i => i.boundingBox === 'block' && !NOT_A_CAP.test(i.name) && !isGroundCover(i.name))?.name ?? null

// A hole-up outlives neither its body nor its waking: one (17:26:53Z) died mid-dig, respawned into a bed and went on to report
// a hole from the bed six seconds later
export function holeUpAborted ({ life, lives, asleep = false }) {
  if (lives !== life) return 'the body died mid-hole: the respawned one does not finish it'
  if (asleep) return 'the body is asleep in bed: no hole from here'
  return null
}

// Which hurts count as a mob in reach for the refusal: not the water (17:46Z: drowning at health 5, the refusal said a hostile
// hit me 0.1 s ago and the body stood in the water to fight it), not a fall, not hunger, not a creeper already gone off
export const mobHit = cause => !cause || !/drowning|a fall of|starving|creeper blew up/.test(cause)

// Whether the hole-up may go ahead, and if not, what instead: 'fight' is holeUpRefusal's rule (armed, a hostile in reach); a
// creeper within reach is 'step', because a creeper is never fought, and a run the body just gave up on does not restart
// itself for fifteen seconds. Refused and standing still beside a creeper was the gap
export function holeUpBlock ({ armed = false, hostile = null, hostileDist = Infinity, hurtMsAgo = Infinity }) {
  if (hostile === 'creeper' && hostileDist <= HOLE_MELEE) return 'step'
  return holeUpRefusal({ armed, hostileDist, hurtMsAgo })
}
export function refusalNote ({ way, hostile, hostileDist, hurtMsAgo = Infinity }) {
  if (way === 'step') return `a ${hostile} ${Math.round(hostileDist)} blocks off: a hole takes seconds to dig and it would go off on top of me, so I step away from it instead`
  const hit = hurtMsAgo <= HOLE_HURT_MS ? `, and it hit me ${(hurtMsAgo / 1000).toFixed(1)} s ago` : ''
  return `a hostile ${Number.isFinite(hostileDist) ? Math.round(hostileDist) : 'I cannot see'} blocks off${hit}: a hole takes seconds to dig with it hitting me, so I fight instead`
}

// The report after the hole, whichever way it was made, always ending in the one command that brings the body back
export function shelterNote ({ way, open, surface, wet = false }) {
  const back = `goto x=${surface.x} y=${surface.y} z=${surface.z} dig=true`
  if (wet) return `the shaft filled with water and I am floating in it: not a shelter, but not a grave either. The way out: ${back}, or dig sideways from the shaft to dry ground`
  if (way === 'pillar') {
    return `pillared up ${HOLE_DEPTH} where I stood${open ? ', though it came up short of the full 3, so something may still reach me' : ''}. ` +
      `Nothing walks up here; an archer can still shoot. The way down when you want me back: ${back} (it digs the blocks under my feet)`
  }
  if (open) return `${way === 'dig' ? `dug ${HOLE_DEPTH} straight down` : 'walled myself in where I stood'} but the hole is OPEN: a side or the top could not be closed (no solid block to place, or nothing to place it against), so something can still reach me. The way out when you want me back: ${back}`
  return holedUpNote({ way, open, surface })
}
