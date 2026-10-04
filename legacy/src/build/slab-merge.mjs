// Placing a slab against a cell that already holds a BOTTOM slab merges the two into a double slab: a full block,
// no gap for water underneath (jizo-melon-patch, 09-26: a farm.build cover job aimed a new TOP oak_slab at a
// channel cell still holding an OLD bottom slab from before the top-slab cards, and it merged into a full block,
// sealing the channel; dug out by hand, the next pass poured and covered it normally). Vanilla combines a slab item
// with the opposite half of an existing slab occupying the same cell regardless of which face was clicked, so this
// has to run BEFORE `place` ever tries - after the fact there is nothing left to undo, only a full block to dig.
// A TOP slab is no risk: farmJobs never covers a cell that already holds one (see cover.mjs), and place's own
// occupiedBy treats a matching name there as already done. Only a bottom slab is unsafe, wet or dry, whatever wood.
const isSlabItem = name => typeof name === 'string' && /_slab$/.test(name)

// `existing` is whatever the caller has to hand: a real mineflayer block (properties live behind getProperties(),
// there is no plain .properties field on it) or an already-wrapped block-like object (api.block's shape, and what
// this module's own tests build). Read either without caring which: a live field test against a real bottom slab
// found this the hard way - existing.properties was always undefined on the real thing, so the refusal never fired.
const propsOf = block => block?.getProperties?.() ?? block?.properties ?? {}

export function slabMergeRefusal ({ x, y, z }, existing, item) {
  if (!isSlabItem(item)) return null
  if (!existing || !isSlabItem(existing.name)) return null
  if (propsOf(existing).type !== 'bottom') return null
  return `${x},${y},${z} holds a bottom slab: dig it first, placing another merges them into a full block`
}
