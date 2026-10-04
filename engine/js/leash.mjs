// Who holds an entity's lead. 26.1 sends it only in the attach_entity packet (holder 0 on release), not in metadata,
// so the packets are tracked per bot from the moment it is adopted.
const holders = new WeakMap()

export function trackLeashes (bot) {
  if (!bot._client || holders.has(bot)) return
  const held = new Map()
  holders.set(bot, held)
  bot._client.on('attach_entity', ({ entityId, vehicleId }) => {
    if (vehicleId > 0) held.set(entityId, vehicleId)
    else held.delete(entityId)
  })
  // ids are reused after an entity unloads; a disappearing holder alone proves nothing, but a gone animal is gone
  bot.on('entityGone', e => held.delete(e.id))
}

// Keys appear only when the entity is on a lead: absent means not leashed (as far as the packets seen say)
export function leashFields (bot, e) {
  const holder = holders.get(bot)?.get(e.id)
  return holder === undefined ? {} : { leashed: true, leashedToMe: holder === bot.entity?.id, leashHolder: holder }
}
