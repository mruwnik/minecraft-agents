// Why JavaScript: Mineflayer boundary; reads the raw entity table and the collect packet Mineflayer only half uses.
// Mineflayer creates a bare entity for any packet naming an id it no longer holds (a late velocity or teleport after
// the removal), and it keeps a picked-up drop until the removal packet arrives. Neither is something a player sees.
// A mob that died stays listed for its death animation (about a second): it is no longer a mob to fight or fear.
const collected = new WeakMap()

export function trackLiveEntities (bot) {
  if (!bot._client || collected.has(bot)) return
  const ids = new Set()
  collected.set(bot, ids)
  bot._client.on('collect', ({ collectedEntityId, pickupItemCount }) => {
    const stack = bot.entities?.[collectedEntityId]?.getDroppedItem?.()
    if (stack && pickupItemCount >= (stack.count ?? Infinity)) ids.add(collectedEntityId)
  })
  bot._client.on('entity_status', ({ entityId, entityStatus }) => { if (entityStatus === 3) ids.add(entityId) }) // 3: death
  // ids are reused later, so forget one as soon as its entity is removed
  bot.on('entityGone', e => ids.delete(e.id))
}

// spawn packets set a type or a name; a bare entity has neither
const spawned = e => Boolean(e.type || e.name || e.username)

export const liveEntities = bot => Object.values(bot.entities ?? {})
  .filter(e => spawned(e) && !collected.get(bot)?.has(e.id))

export const liveEntity = (bot, id) => liveEntities(bot).find(e => e.id === id)
