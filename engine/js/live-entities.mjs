// Why JavaScript: Mineflayer boundary; reads the raw entity table and the collect packet Mineflayer only half uses.
// Mineflayer creates a bare entity for any packet naming an id it no longer holds (a late velocity or teleport after
// the removal), and it keeps a picked-up drop until the removal packet arrives. Neither is something a player sees.
// A mob that died stays listed for its death animation (about a second): it is no longer a mob to fight or fear.
// itemSpawn gives an item's raw spawn facts, what a player watching saw: {at, pos, player: {name, pos} (the player
// nearest to it then, by feet), tosses (the body's last tosses [{at, item, count}], from noteToss)}. Who threw it is jobs.lib.loose's call.
const collected = new WeakMap()
const spawns = new WeakMap()
const tosses = new WeakMap()
const tossKeep = 8

const xyz = p => ({ x: p.x, y: p.y, z: p.z })
const d2 = (a, b) => (a.x - b.x) ** 2 + (a.y - b.y) ** 2 + (a.z - b.z) ** 2

const nearestPlayer = (bot, pos) => Object.values(bot.entities ?? {})
  .filter(e => e.type === 'player' && e.username && e.position)
  .reduce((best, e) => (!best || d2(e.position, pos) < d2(best.position, pos) ? e : best), null)

// the toss primitive calls it just before it throws
export const noteToss = (bot, item, count) => tosses.set(bot, [...(tosses.get(bot) ?? []), { at: Date.now(), item, count }].slice(-tossKeep))

export function trackLiveEntities (bot) {
  if (!bot._client || collected.has(bot)) return
  const ids = new Set()
  collected.set(bot, ids)
  const born = new Map()
  spawns.set(bot, born)
  bot.on('entitySpawn', e => {
    if (e?.name !== 'item' || !e.position) return
    const p = nearestPlayer(bot, e.position)
    const recent = tosses.get(bot)
    born.set(e.id, {
      at: Date.now(),
      pos: xyz(e.position),
      ...(p && { player: { name: p.username, pos: xyz(p.position) } }),
      ...(recent && { tosses: recent })
    })
  })
  bot._client.on('collect', ({ collectedEntityId, pickupItemCount }) => {
    const stack = bot.entities?.[collectedEntityId]?.getDroppedItem?.()
    if (stack && pickupItemCount >= (stack.count ?? Infinity)) ids.add(collectedEntityId)
  })
  // 3: death; the body's own entity outlives its death (respawn reuses it), so it stays listed
  bot._client.on('entity_status', ({ entityId, entityStatus }) => { if (entityStatus === 3 && entityId !== bot.entity?.id) ids.add(entityId) })
  // ids are reused later, so forget one as soon as its entity is removed
  bot.on('entityGone', e => { ids.delete(e.id); born.delete(e.id) })
}

// spawn packets set a type or a name; a bare entity has neither
const spawned = e => Boolean(e.type || e.name || e.username)

export const liveEntities = bot => Object.values(bot.entities ?? {})
  .filter(e => spawned(e) && !collected.get(bot)?.has(e.id))

export const liveEntity = (bot, id) => liveEntities(bot).find(e => e.id === id)

export const itemSpawn = (bot, id) => spawns.get(bot)?.get(id)

// Whether item entity id is the body's own toss of that item: its spawn came within ownTossMs after the toss call.
const ownTossMs = 2000
export const isOwnToss = (bot, id, item) => {
  const s = itemSpawn(bot, id)
  return Boolean(s?.tosses?.some(t => t.item === item && s.at - t.at >= 0 && s.at - t.at <= ownTossMs))
}
