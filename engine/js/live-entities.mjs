// Why JavaScript: Mineflayer boundary; reads the raw entity table and the collect packet Mineflayer only half uses.
// Mineflayer creates a bare entity for any packet naming an id it no longer holds (a late velocity or teleport after
// the removal), and it keeps a picked-up drop until the removal packet arrives. Neither is something a player sees.
// A mob that died stays listed for its death animation (about a second): it is no longer a mob to fight or fear.
// An item that spawns at a player's eye (eye height less 0.3, where vanilla's drop puts it) was thrown by that player, as a
// player watching would tell; a block's or a mob's drop spawns elsewhere. itemSpawn gives {at, by} (by absent when none).
const collected = new WeakMap()
const spawns = new WeakMap()
const THROW_DY = [0.8, 1.8]
const THROW_DXZ = 0.6

const thrower = (bot, pos) => Object.values(bot.entities ?? {})
  .find(e => e.type === 'player' && e.username && e.position &&
    Math.abs(e.position.x - pos.x) <= THROW_DXZ && Math.abs(e.position.z - pos.z) <= THROW_DXZ &&
    pos.y - e.position.y >= THROW_DY[0] && pos.y - e.position.y <= THROW_DY[1])?.username

export function trackLiveEntities (bot) {
  if (!bot._client || collected.has(bot)) return
  const ids = new Set()
  collected.set(bot, ids)
  const born = new Map()
  spawns.set(bot, born)
  bot.on('entitySpawn', e => {
    if (e?.name !== 'item' || !e.position) return
    const by = thrower(bot, e.position)
    born.set(e.id, { at: Date.now(), ...(by && { by }) })
  })
  bot._client.on('collect', ({ collectedEntityId, pickupItemCount }) => {
    const stack = bot.entities?.[collectedEntityId]?.getDroppedItem?.()
    if (stack && pickupItemCount >= (stack.count ?? Infinity)) ids.add(collectedEntityId)
  })
  bot._client.on('entity_status', ({ entityId, entityStatus }) => { if (entityStatus === 3) ids.add(entityId) }) // 3: death
  // ids are reused later, so forget one as soon as its entity is removed
  bot.on('entityGone', e => { ids.delete(e.id); born.delete(e.id) })
}

// spawn packets set a type or a name; a bare entity has neither
const spawned = e => Boolean(e.type || e.name || e.username)

export const liveEntities = bot => Object.values(bot.entities ?? {})
  .filter(e => spawned(e) && !collected.get(bot)?.has(e.id))

export const liveEntity = (bot, id) => liveEntities(bot).find(e => e.id === id)

export const itemSpawn = (bot, id) => spawns.get(bot)?.get(id)
