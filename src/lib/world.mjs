// Geometry and block-reading primitives everything else in src/lib/ builds on: zones, cell shapes, what a
// block counts as (air, ground cover, water-holding, in-sight), and the four cardinal steps. No sibling imports.

import { between } from '../cli.mjs'
export const range = (a, b) => Array.from({ length: Math.abs(b - a) + 1 }, (_, i) => Math.min(a, b) + i)

// Is pos inside any of the protected boxes ({x1,y1,z1,x2,y2,z2}, corners in any order, inclusive)?
export const inAnyZone = (zones, pos) =>
  zones.some(z => between(pos.x, z.x1, z.x2) && between(pos.y, z.y1, z.y2) && between(pos.z, z.z1, z.z2))

// Does a block's properties object satisfy a `where` condition such as {age: 7}? Values compare as strings.
export const matchesProps = (props, where) => Object.entries(where ?? {}).every(([k, v]) => String(props?.[k]) === String(v))

// Some server replies never come (window opens through the version bridge, mostly); never wait forever for one.
export function within (ms, promise, what) {
  let timer
  const late = new Promise((resolve, reject) => { timer = setTimeout(() => reject(new Error(`${what} took longer than ${ms / 1000}s`)), ms) })
  return Promise.race([promise, late]).finally(() => clearTimeout(timer))
}

// the cell the pathfinder counts me in: standing on a block that is not a full one high (farmland, a slab, a dirt path) my feet are
// inside that block's cell, and the pathfinder plans from the cell above. Judging arrival from the floored cell fails by one block
export function feetCell (position, onGround) {
  const sunk = onGround && position.y - Math.floor(position.y) > 0.001
  return { x: Math.floor(position.x), y: Math.floor(position.y) + (sunk ? 1 : 0), z: Math.floor(position.z) }
}
export function realCell (position, free) {
  const [x, y, z] = [Math.floor(position.x), Math.floor(position.y), Math.floor(position.z)]
  const [fx, fz] = [position.x - x, position.z - z]
  // how far my centre is from each of the eight cells around; more than my half width (0.3) away and no part of me stands there
  const gapTo = { '-1': f => f, 0: () => 0, 1: f => 1 - f }
  const sides = [-1, 0, 1].flatMap(dx => [-1, 0, 1].map(dz => ({ d: Math.hypot(gapTo[dx](fx), gapTo[dz](fz)), x: x + dx, z: z + dz }))).filter(s => s.x !== x || s.z !== z)
  const side = sides.sort((a, b) => a.d - b.d).find(s => s.d <= 0.3 && free(s.x, y, s.z))
  return side ? { x: side.x, y, z: side.z } : null
}
// the names of the four cells beside a cell; none for the position-less block the pathfinder makes up for an unloaded cell
export const besideNames = (position, nameAt) => position ? [[1, 0], [-1, 0], [0, 1], [0, -1]].map(([dx, dz]) => nameAt(position.x + dx, position.y, position.z + dz)) : []

// blocks that somebody put there: a walk with dig=true must go round them, zone or no zone (Aviendha's goto dig=true tunnelled through
// her own cobble pen wall day after day, and the cows walked out of the hole). Logs and leaves stay diggable: forests are in the way a lot
const BUILT = /(^|_)(cobblestone|planks|fence|gate|wall|door|trapdoor|bed|stairs|slab|glass|pane|wool|carpet|torch|lantern|chest|barrel|furnace|smoker|table|farmland|bricks|ladder|sign|banner|rail|hopper|composter|campfire|anvil|bookshelf|concrete|terracotta)$|^(wheat|carrots|potatoes|beetroots|melon_stem|pumpkin_stem|cocoa|sugar_cane|bamboo|hay_block)$/
export const looksBuilt = name => BUILT.test(name)

// weeds on top of a block keep a hoe or shovel from working it: till and path clear these by themselves (not flowers or crops: someone may want those)
export const isGroundCover = name => /^(short_grass|tall_grass|fern|large_fern|dead_bush|snow|leaf_litter)$/.test(name)

// a fluid is not a block: the server never sends a break for one, so bot.dig on water sat at doing=dig for 167 seconds (#110)
export const FLUIDS = new Set(['water', 'lava', 'flowing_water', 'flowing_lava', 'bubble_column'])

// Item 14 (Perrin, BUGS.md 09-23). A block that comes back null is not air and not stone: it is a chunk this body has
// never been sent, which is every chunk more than a view away. `pen.check` on a pen 200 blocks off answered "not a spot
// to stand on", blaming his coordinates for a world his client had never seen, and the role's own case (fetch from the
// shared stock to your own pen) starts exactly there. Nothing may be guessed from an unloaded chunk: say so, or go.
export const outOfSight = (block, at, from) => block
  ? null
  : `${at.x},${at.y},${at.z} is too far to see: ${from ? `it is ${Math.round(Math.hypot(at.x - from.x, at.y - from.y, at.z - from.z))} blocks off and ` : ''}that chunk is not loaded, so nothing there can be read. goto it first, then ask again`
export const STEPS = [[1, 0], [-1, 0], [0, 1], [0, -1]]
export const isAir = name => /^(air|cave_air|void_air)$/.test(String(name))
// water still stands in a cell whose block was waterlogged (a slab or stairs laid into the source): the farmland beside
// it stays wet, so a covered channel is a full channel
export const holdsWater = block => Boolean(block) && (block.name === 'water' || String(block.properties?.waterlogged) === 'true')
// stricter than holdsWater, for the one place wetness alone is not enough: capping a cell with a slab. holdsWater is
// right for census - the farmland beside a cell does not care whether its water is a source or a neighbour's flow
// passing through. But flow (properties.level 1-7) has no source of its own in that cell; it can recede a tick after
// this is read, before the cover lands, leaving a slab capping ground that is not really wet. Only a settled source
// (level 0, or already waterlogged) is safe to cap
export const hasWaterSource = block => Boolean(block) && (block.name === 'water' ? Number(block.properties?.level ?? 0) === 0 : String(block.properties?.waterlogged) === 'true')
