// What makes one hive safe to work, read straight off the world. Kept beside the apiary composites rather than in
// src/lib.mjs: nothing else needs to know a campfire from a hive.
const HIVE_NAMES = new Set(['beehive', 'bee_nest'])
const CAMPFIRE_NAMES = new Set(['campfire', 'soul_campfire'])
const FACING_STEP = { north: [0, 0, -1], south: [0, 0, 1], east: [1, 0, 0], west: [-1, 0, 0] }

const isCampfire = block => CAMPFIRE_NAMES.has(block?.name)
const isLit = block => isCampfire(block) && block.properties?.lit !== false && String(block.properties?.lit) !== 'false'
// a moss carpet is a plant and burns; every dyed carpet is wool and sits on a fire without catching
const isCarpet = name => /^[a-z_]+_carpet$/.test(String(name)) && name !== 'moss_carpet'

// The standard column (Dan, 2026-09-23): campfire at y, a carpet ON it at y+1, one air block, the hive at y+3. An open
// fire burns the bees that fly through it, so a lit fire with no carpet is `open`, and inspect, harvest and maintain
// all say so before anything is touched.
export function fireState ({ x, y, z, block, blockAt }) {
  const lit = isLit(block)
  const guarded = isCarpet(blockAt(x, y + 1, z)?.name)
  return { x, y, z, lit, guarded, open: lit && !guarded }
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
  return { x, y, z, name: block.name, honey, ripe: honey >= 5, facing, entranceClear, smoked: Boolean(campfire), guarded: Boolean(fire?.guarded), open: Boolean(fire?.open), campfire }
}

export const carpetCarried = items => Object.keys(items).find(name => items[name] > 0 && isCarpet(name)) ?? null
export const CAMPFIRES = [...CAMPFIRE_NAMES]
