// The fake's useOn: right-click a block with an item (or an empty hand), applying the few rules the jobs rely on.
// Mirrors the result shape of use-on.mjs: { status, before, after, consumed }, `missing` carrying only the status.

import { tieToPost } from './fake-leash.mjs'

const key = ({ x, y, z }) => `${x},${y},${z}`
const REACH = 4.5
const MAX_AGE = { wheat: 7, carrots: 7, potatoes: 7, beetroots: 3 }
const TILLABLE = new Set(['dirt', 'grass_block', 'dirt_path'])
const COMPOSTABLE = new Set([
  'wheat_seeds', 'beetroot_seeds', 'melon_seeds', 'pumpkin_seeds', 'wheat', 'carrot', 'potato', 'beetroot', 'apple',
  'melon_slice', 'short_grass', 'tall_grass', 'oak_leaves', 'birch_leaves', 'spruce_leaves', 'kelp', 'sweet_berries',
  'oak_sapling', 'birch_sapling', 'spruce_sapling', 'bread', 'baked_potato', 'cookie', 'pumpkin', 'melon'
])

const HIVES = new Set(['beehive', 'bee_nest'])
const BED = /_bed$|^respawn_anchor$/
// The generic window guard is a last resort: a window mineflayer misparses can desync the inventory, so refuse window-opening blocks before the click.
const CONTAINER = /chest$|barrel$|shulker_box$|furnace$|smoker$|hopper$|dispenser$|dropper$|brewing_stand$|crafting_table$|anvil$|enchanting_table$|grindstone$|loom$|stonecutter$|cartography_table$|smithing_table$|lectern$|beacon$|^crafter$|command_block$|^structure_block$|^jigsaw$|^vault$/
const HAZARDS = new Set(['flint_and_steel', 'fire_charge', 'lava_bucket'])
const BLOCK_ITEMS = new Set(['dirt', 'cobblestone', 'stone', 'oak_planks', 'oak_leaves', 'pumpkin', 'melon', 'hay_block'])

export function fakeUseOn (s, { pos, item, face = 'up' }, { spawnItem, near }) {
  const k = key(pos)
  const name = () => s.blocks.get(k) ?? 'air'
  const snapshot = () => ({ name: name(), properties: { ...(s.states.get(k) ?? {}), ...(s.ages.has(k) && { age: s.ages.get(k) }) } })
  const before = snapshot()
  if (before.name === 'air') return { status: 'missing' }
  const result = (status, consumed = 0, extra = {}) => ({ status, ...extra, before, after: snapshot(), consumed })
  if (BED.test(before.name)) return result('cannot', 0, { reason: 'bed' })
  if (CONTAINER.test(before.name)) return result('cannot', 0, { reason: 'container' })
  if (HAZARDS.has(item)) return result('cannot', 0, { reason: 'hazard' })
  if (BLOCK_ITEMS.has(item) && before.name !== 'composter') return result('cannot', 0, { reason: 'use-place' })
  if (item !== undefined && !s.inventory.some(i => i.name === item)) return result('no-item')
  if (!near(pos)) return result('unreachable', 0, { reason: 'too-far', distance: Math.round(Math.hypot(s.self.pos.x - (pos.x + 0.5), s.self.pos.y - (pos.y + 0.5), s.self.pos.z - (pos.z + 0.5)) * 100) / 100 })
  s.self.held = item ?? null

  const consume = () => {
    const stack = s.inventory.find(i => i.name === item)
    stack.count -= 1
    if (stack.count === 0) s.inventory.splice(s.inventory.indexOf(stack), 1)
    return 1
  }
  const level = s.states.get(k)?.level ?? 0
  const setLevel = (n) => s.states.set(k, { ...s.states.get(k), level: n })
  const here = name()

  if (/_fence$/.test(here) && (item === 'lead' || item === undefined)) {
    return result(tieToPost(s, pos) > 0 ? 'used' : 'unchanged')
  }
  // doors, trapdoors and gates flip `open` (iron ones ignore a hand); a lever flips `powered`; a button sets it (it never falls back here).
  // A block with `locked: true` models a protected area: the click is lost.
  if (item === undefined && s.states.get(k)?.locked) return result('unchanged')
  if (item === undefined && /_(fence_gate|door|trapdoor)$/.test(here) && !/^iron_/.test(here)) {
    s.states.set(k, { ...s.states.get(k), open: !s.states.get(k)?.open })
    return result('used')
  }
  if (item === undefined && here === 'lever') {
    s.states.set(k, { ...s.states.get(k), powered: !s.states.get(k)?.powered })
    return result('used')
  }
  if (item === undefined && /_button$/.test(here) && !s.states.get(k)?.powered) {
    s.states.set(k, { ...s.states.get(k), powered: true })
    return result('used')
  }
  if (item?.endsWith('_hoe') && TILLABLE.has(here) && face !== 'down' && !s.blocks.has(key({ ...pos, y: pos.y + 1 }))) {
    s.blocks.set(k, 'farmland')
    return result('used')
  }
  if (item === 'bone_meal' && MAX_AGE[here] !== undefined) {
    if (s.ages.get(k) >= MAX_AGE[here]) return result('unchanged')
    s.ages.set(k, Math.min(MAX_AGE[here], (s.ages.get(k) ?? 0) + 2))
    return result('used', consume())
  }
  if (item === 'bone_meal' && (here.endsWith('_sapling') || here === 'grass_block')) return result('used', consume())
  if (HIVES.has(here) && (s.states.get(k)?.honey_level ?? 0) >= 5 && (item === 'shears' || item === 'glass_bottle')) {
    s.states.set(k, { ...s.states.get(k), honey_level: 0 })
    if (item === 'shears') spawnItem({ ...pos, y: pos.y + 1 }, 'honeycomb', 3)
    if (item === 'glass_bottle') {
      const consumed = consume()
      const have = s.inventory.find(i => i.name === 'honey_bottle')
      if (have) have.count += 1
      else s.inventory.push({ name: 'honey_bottle', count: 1 })
      return result('used', consumed)
    }
    return result('used')
  }
  if (here === 'composter' && level === 8) {
    setLevel(0)
    spawnItem({ ...pos, y: pos.y + 1 }, 'bone_meal', 1)
    return result('used')
  }
  if (here === 'composter' && level < 7 && COMPOSTABLE.has(item)) {
    const consumed = consume()
    setLevel(level + 1 === 7 ? 8 : level + 1)
    return result('used', consumed)
  }
  return result('unchanged')
}
