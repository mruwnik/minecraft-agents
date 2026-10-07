// Why JavaScript: Mineflayer boundary; entity use/attack calls and the refusal guard before the click.
// Using an item (or the empty hand) on an entity: feed, shear, lead, unleash. The server answers none of these with a
// result, so the call watches for what changed and reports it.
import vec3 from 'vec3'
import { emptyHand } from './unequip.mjs'

export const INTERACT_WAIT_MS = 500
export const LOVE_STATUS = 18
export const LOVE_GRACE_MS = 150
const POLL_MS = 50
const DEFAULT_EYE = 1.62
const WOOL_SHEARED = 0x10
const WOOL_FALLBACK = 18

const sleepMs = ms => new Promise(resolve => setTimeout(resolve, ms))

const metaIndex = (bot, e, key, fallback) => {
  const at = bot.registry?.entitiesByName?.[e.name]?.metadataKeys?.indexOf(key)
  return at >= 0 ? at : fallback
}
const hasMetaKey = (bot, e, key) => bot.registry?.entitiesByName?.[e.name]?.metadataKeys?.includes(key) === true

// the dye (0..15, 0 white) of a sheep's wool, or null for another mob
export const sheepDye = (bot, e) => e.name === 'sheep' ? (e.metadata?.[metaIndex(bot, e, 'wool', WOOL_FALLBACK)] ?? 0) & 0x0f : null

// What a mob shows that a use can change: identity, age and (sheep) shearing. Keys appear only when they apply.
export const mobFields = (bot, e) => ({
  ...(typeof e.uuid === 'string' && { uuid: e.uuid }),
  ...(hasMetaKey(bot, e, 'baby') && { baby: e.metadata?.[metaIndex(bot, e, 'baby', -1)] === true }),
  ...(e.name === 'creeper' && { fusing: e.metadata?.[metaIndex(bot, e, 'swell_dir', 16)] === 1 }),
  ...(e.name === 'sheep' && { sheared: ((e.metadata?.[metaIndex(bot, e, 'wool', WOOL_FALLBACK)] ?? 0) & WOOL_SHEARED) !== 0 })
})

const OPENS_WINDOW = ['villager', 'wandering_trader', 'chest_minecart', 'hopper_minecart']
const MOUNTS = ['horse', 'donkey', 'mule', 'skeleton_horse', 'zombie_horse', 'camel', 'camel_husk', 'llama', 'trader_llama', 'minecart', 'happy_ghast']
const MOUNTING_SUFFIX = /_(boat|chest_boat|raft)$/

// Entities a use always answers by opening a window or taking the body aboard: name -> reason
export const REFUSED = Object.fromEntries([
  ...OPENS_WINDOW.map(n => [n, 'opens-window']),
  ...MOUNTS.map(n => [n, 'mounts'])
])
export const refusal = name => REFUSED[name] ?? (MOUNTING_SUFFIX.test(name) ? 'mounts' : null)

const middle = e => vec3(e.position.x, e.position.y + (e.height ?? 1) / 2, e.position.z)
const carried = (bot, name) => name ? bot.inventory.items().filter(i => i.name === name).reduce((n, i) => n + i.count, 0) : 0
const differences = (before, after) => Object.fromEntries(
  Object.keys(after).filter(k => before[k] !== after[k]).map(k => [k, [before[k], after[k]]]))

const DISMOUNT_WAIT_MS = 1000
const RELEASE_MS = 500
const sneak = (bot, on) => bot.setControlState('sneak', on)

// 26.1 dismounts on sneak. The raw player_input is overridden by mineflayer's physics tick,
// so go through the control state; mineflayer's own dismount() sends jump, which does nothing
async function dismount (bot, timeScale) {
  if (!bot.vehicle) return
  sneak(bot, true)
  const deadline = Date.now() + DISMOUNT_WAIT_MS * timeScale
  try {
    while (bot.vehicle && Date.now() < deadline) await sleepMs(POLL_MS * timeScale)
  } finally {
    sneak(bot, false)
  }
}

// synchronous variant for abort: sneak now, release later (never throws if the bot is gone)
function dismountOnAbort (bot) {
  if (!bot.vehicle) return
  sneak(bot, true)
  setTimeout(() => {
    try { sneak(bot, false) } catch {}
  }, RELEASE_MS)
}

export async function interactWith (bot, ctx, a, { timeScale = 1, reach }) {
  const target = bot.entities[a.id]
  if (!target) return { status: 'gone' }
  const refused = refusal(target.name)
  if (refused) return { status: 'cannot', reason: refused }
  const item = typeof a.item === 'string' ? bot.inventory.items().find(i => i.name === a.item) : null
  if (typeof a.item === 'string' && !item) return { status: 'no-item' }
  const eye = bot.entity.position.offset(0, bot.entity.height ?? DEFAULT_EYE, 0)
  if (eye.distanceTo(middle(target)) > reach) return { status: 'out-of-reach' }

  if (item) {
    await bot.equip(item, 'hand')
    ctx.alive()
  } else {
    const emptied = await emptyHand(bot, ctx)
    if (emptied.status === 'full') return { status: 'full' }
  }
  const before = { count: carried(bot, a.item), durabilityUsed: bot.heldItem?.durabilityUsed ?? 0, fields: mobFields(bot, target) }

  let love = false
  let leash = null
  const onStatus = p => { if (p.entityId === a.id && p.entityStatus === LOVE_STATUS) love = true }
  // mineflayer's entityAttach fires for an unleash too (26.1 sends holder 0), so read the raw packet
  const onAttach = p => { if (p.entityId === a.id) leash = p.vehicleId > 0 ? 'attached' : 'detached' }
  // mineflayer 4.39 never clears bot.vehicle on a dismount: the server sends set_passengers for the
  // vehicle without the body's id, and mineflayer only handles packets whose passengers include the body
  const onPassengers = p => { if (p.entityId === bot.vehicle?.id && !p.passengers.includes(bot.entity.id)) bot.vehicle = null }
  const undo = () => {
    if (bot.currentWindow) bot.closeWindow(bot.currentWindow)
    dismountOnAbort(bot)
  }
  bot._client.on('entity_status', onStatus)
  bot._client.on('attach_entity', onAttach)
  bot._client.on('set_passengers', onPassengers)
  try {
    await bot.lookAt(middle(target), true)
    ctx.alive()
    ctx.onAbort(undo)
    bot.useOn(target)
    const deadline = Date.now() + INTERACT_WAIT_MS * timeScale
    const read = () => {
      const now = bot.entities[a.id]
      const wornNow = bot.heldItem?.name === a.item ? (bot.heldItem?.durabilityUsed ?? 0) - before.durabilityUsed : 0
      return {
        gone: !now,
        consumed: Math.max(0, before.count - carried(bot, a.item)),
        worn: Math.max(0, wornNow),
        love,
        leash,
        changed: now ? differences(before.fields, mobFields(bot, now)) : {}
      }
    }
    const seen = r => r.consumed > 0 || r.worn > 0 || r.love || r.leash !== null || Object.keys(r.changed).length > 0
    // a feed's love status and slot update are separate packets: a bare consumption waits a grace for love
    const onlyConsumed = r => r.consumed > 0 && !r.love && r.worn === 0 && r.leash === null && Object.keys(r.changed).length === 0
    let consumedAt = null
    const settled = r => {
      if (!seen(r)) return false
      if (!onlyConsumed(r)) return true
      consumedAt ??= Date.now()
      return Date.now() - consumedAt >= LOVE_GRACE_MS * timeScale
    }
    let r = read()
    while (!settled(r) && !r.gone && !bot.vehicle && !bot.currentWindow && Date.now() < deadline) {
      await sleepMs(POLL_MS * timeScale)
      ctx.alive()
      r = read()
    }
    const { consumed, worn, changed } = r
    const fields = { consumed, worn, love, leash, changed }
    const reason = bot.currentWindow ? 'opened-window' : bot.vehicle ? 'mounted' : null
    if (reason === 'opened-window') {
      bot.closeWindow(bot.currentWindow)
      return { status: 'failed', reason, ...fields }
    }
    if (reason) {
      await dismount(bot, timeScale)
      return { status: 'failed', reason: bot.vehicle ? 'mounted-stuck' : 'mounted', ...fields }
    }
    return { status: seen(r) ? 'used' : 'no-effect', ...fields }
  } finally {
    bot._client.off('entity_status', onStatus)
    bot._client.off('attach_entity', onAttach)
    bot._client.off('set_passengers', onPassengers)
  }
}
