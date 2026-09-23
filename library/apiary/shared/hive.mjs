// What makes one hive safe to work, read straight off the world. Kept beside the apiary composites rather than in
// src/lib.mjs: nothing else needs to know a campfire from a hive.
const HIVE_NAMES = new Set(['beehive', 'bee_nest'])
const CAMPFIRE_NAMES = new Set(['campfire', 'soul_campfire'])
const FACING_STEP = { north: [0, 0, -1], south: [0, 0, 1], east: [1, 0, 0], west: [-1, 0, 0] }

const isCampfire = block => CAMPFIRE_NAMES.has(block?.name)
const isLit = block => isCampfire(block) && block.properties?.lit !== false && String(block.properties?.lit) !== 'false'
// a moss carpet is a plant and burns; every dyed carpet is wool and sits on a fire without catching
const isCarpet = name => /^[a-z_]+_carpet$/.test(String(name)) && name !== 'moss_carpet'

// The standard column (Dan, 2026-09-23): the campfire at least one block underground at y, ground on all four sides of
// it, a carpet ON it at y+1 (flush with the ground), one air block, the hive at y+3. An open fire burns the bees that
// land in it, so a lit fire with nothing on it is `open`, and inspect, harvest and maintain all say so before anything
// is touched. A carpet covers it; so does anything with a collision box sitting straight on it, a wild nest on its fire
// above all: there is no cell to carpet, and nothing can land in the flame. A moss carpet is a plant, burns, and covers
// nothing. A fire with any side in the open is `raised`: bees fly into it sideways, so apiary.guard moves it down.
const SIDES = [[1, 0], [-1, 0], [0, 1], [0, -1]]
export function fireState ({ x, y, z, block, blockAt }) {
  const lit = isLit(block)
  const above = blockAt(x, y + 1, z)
  const guarded = isCarpet(above?.name) || (Boolean(above?.solid) && above.name !== 'moss_carpet')
  const sunk = SIDES.every(([dx, dz]) => Boolean(blockAt(x + dx, y, z + dz)?.solid))
  return { x, y, z, lit, guarded, open: lit && !guarded, sunk }
}

// The game's own smoke rule (CampfireBlock.isSmokeyPos): look down at most five cells; a lit campfire smokes the hive;
// the first block with a collision box in the way stops the search, and then only a lit fire DIRECTLY beneath it
// counts. So a carpet or a full block sitting on the fire lets the smoke through, and either with a gap under it does
// not. The client calls a carpet `solid` (it has a collision box), which is exactly the test the game applies.
function smokeSource (x, y, z, blockAt) {
  for (let down = 1; down <= 5; down++) {
    const seen = blockAt(x, y - down, z)
    if (isLit(seen)) return { x, y: y - down, z }
    if (isCampfire(seen)) return null
    if (!seen?.solid) continue
    const beneath = blockAt(x, y - down - 1, z)
    return isLit(beneath) ? { x, y: y - down - 1, z } : null
  }
  return null
}

export function hiveState ({ x, y, z, block, blockAt }) {
  if (!HIVE_NAMES.has(block?.name)) return null
  const honey = Number(block.properties?.honey_level ?? 0)
  const facing = block.properties?.facing
  const [dx, dy, dz] = FACING_STEP[facing] ?? [0, 0, 0]
  const front = blockAt(x + dx, y + dy, z + dz)
  // Unknown is not clear: harvesting is safety-sensitive, so require the entrance cell to be loaded and observed.
  const entranceClear = Boolean(facing) && Boolean(front) && !front.solid
  const campfire = smokeSource(x, y, z, blockAt)
  const fire = campfire ? fireState({ ...campfire, block: blockAt(campfire.x, campfire.y, campfire.z), blockAt }) : null
  return { x, y, z, name: block.name, honey, ripe: honey >= 5, facing, entranceClear, smoked: Boolean(campfire), guarded: Boolean(fire?.guarded), open: Boolean(fire?.open), raised: Boolean(fire?.lit && !fire.sunk), campfire }
}

// One census, read off the same per-hive facts `hiveLine` prints. Mariel read `unsafe=1` over four hives, checked the
// ripe one, found no NO-SMOKE tag on it and took the count for a lie (BUGS.md 09-23 01:12Z); the unsmoked hive was one
// of the other three and nothing in the line said which. A count you cannot check against the line below it is a count
// you cannot act on. So every count names its hives when it is not zero, and the one that means "no campfire under it"
// is called what the tag is called, not `unsafe`, which read as a verdict on the work about to be done. The fire
// counts stay counts of FIRES, because a fire no hive sits over still burns the bees that land in it and `details=`
// says nothing about it - but they name their fires too, so nobody mistakes one for a hive flag again.
export function apiaryCensus (hives, fires) {
  const count = (name, list) => list.length
    ? { [name]: list.length, [`${name}At`]: list.map(h => `${h.x},${h.y},${h.z}`).join(' ') }
    : { [name]: 0 }
  return {
    hives: hives.length,
    ...count('ripe', hives.filter(h => h.ripe)),
    ...count('noSmoke', hives.filter(h => !h.smoked)),
    ...count('blocked', hives.filter(h => !h.entranceClear)),
    ...count('openFires', fires.filter(f => f.open)),
    ...count('raisedFires', fires.filter(f => f.lit && !f.sunk))
  }
}

export const carpetCarried = items => Object.keys(items).find(name => items[name] > 0 && isCarpet(name)) ?? null
export const campfireCarried = items => CAMPFIRES.find(name => items[name] > 0) ?? null
export const CAMPFIRES = [...CAMPFIRE_NAMES]
export const CAMPFIRE_RECIPE = 'a campfire (3 sticks, 1 coal or charcoal, 3 logs)'
