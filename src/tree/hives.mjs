import { parsePlacePlan } from '../lib/plan.mjs'

export const isHive = name => name === 'bee_nest' || name === 'beehive'

export function silkTouchTool (items) {
  return items.find(item => item && item.name !== 'enchanted_book' &&
    item.enchants?.some(enchant => String(enchant.name ?? '').replace(/^minecraft:/, '') === 'silk_touch')) ?? null
}

// The saved forest plan's anchor is its north-west corner. Authorization for
// taking a nest never extends to a neighboring plot merely because leaves or
// logs from one planned tree reach across that boundary.
export function forestHiveClaim (place, owner, at, name) {
  if (!isHive(name) || !place || place.kind !== 'forest' || place.by !== owner) return false
  const parsed = parsePlacePlan(place)
  if (parsed.error) return false
  return at.x >= place.x && at.x < place.x + parsed.width &&
    at.z >= place.z && at.z < place.z + parsed.height
}

export function authorizedHives (tree, place, owner) {
  const protectedBlocks = tree.protected ?? []
  if (!protectedBlocks.length || !protectedBlocks.every(block => forestHiveClaim(place, owner, block, block.name))) return []
  return protectedBlocks
}

export function allowClaimedHives (tree, place, owner, hasSilkTouch) {
  const hives = authorizedHives(tree, place, owner)
  if (!hives.length) return { tree, hives }
  const attention = tree.attention.filter(reason => !reason.startsWith('protected '))
  // Inspection must leave these owned nests actionable so maintain can either
  // move them with Silk Touch or establish smoke and safely destroy them. The
  // harvest step reports the exact prerequisite if neither route is possible.
  return { tree: { ...tree, attention }, hives }
}

// Minecraft only treats a nest as smoked when smoke can rise directly from a
// campfire below it. Require a clear vertical column and a lit campfire 1–5
// blocks below; callers still recheck this at the dig primitive boundary.
export function hiveSmokeCampfire (blockAt, hive) {
  for (let dy = 1; dy <= 5; dy++) {
    const campfire = blockAt(hive.x, hive.y - dy, hive.z)
    if (!['campfire', 'soul_campfire'].includes(campfire?.name) || campfire.properties?.lit !== true) continue
    let clear = true
    for (let y = hive.y - dy + 1; y < hive.y; y++) {
      const b = blockAt(hive.x, y, hive.z)
      if (!b || !['air', 'cave_air', 'void_air'].includes(b.name)) { clear = false; break }
    }
    if (clear) return { x: hive.x, y: hive.y - dy, z: hive.z }
  }
  return null
}
