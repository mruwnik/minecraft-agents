// Storing a harvest, chest by chest, through the composite api: the pure decisions are in src/lib/storage.mjs. A chest
// that answers "the CHEST is full" took what fitted; the pockets before and after say how much, and the rest goes to
// the next chest in the fixed order. What no chest takes is storage_full=, not stuck=: the round goes on, the routine's
// day line carries it, and the stuck watch alerts after two days of it (card 63e91e8f).
import { CHEST_FULL, NEAREST_REACH, STORAGE_BLOCKS, STORAGE_REACH, chestOrder, depositLine, depositPlan, minusCounts, orderedChests, planChests, storageFullLine, wentIn } from './lib/storage.mjs'

// the runner's "inventory full and no chest to deposit in" hand-back asks: is there anywhere to put things at all?
export const canStore = (target, cells = []) => Boolean(target) && (target.kind !== 'plan' || planChests(cells).length > 0)

// the chests a target means, in the order they are tried. A marked place is walked to first: find_blocks looks round
// the body, not round a point (card 4552230d), and the deposit walks to each chest itself
const findChests = async (api, target) => {
  if (target.kind === 'place') {
    await api.act('goto', { x: target.x, y: target.y, z: target.z, range: 4 })
    const found = await api.act('find_blocks', { block: STORAGE_BLOCKS, maxDistance: STORAGE_REACH, count: 32 })
    return chestOrder(found?.positions ?? [], target)
  }
  if (target.kind === 'nearest') {
    const found = await api.act('find_blocks', { block: STORAGE_BLOCKS, maxDistance: NEAREST_REACH, count: 1 })
    return chestOrder(found?.positions ?? [], api.pos())
  }
  return []
}

const nowhere = target => target.kind === 'place'
  ? `no chest within ${STORAGE_REACH} of ${target.name}`
  : target.kind === 'nearest' ? `no chest within ${NEAREST_REACH}` : 'no chest in the plan'

// a chest cell that is loaded and holds no chest, barrel or trapped chest: the deposit would open the air (Jizo's plan
// marks a C cell where no chest was ever built: "containerToOpen is neither a block nor an entity", 09-26 23:51Z). A cell
// that is not loaded is tried: the deposit walks there first
const missingChest = (api, target, chest) => {
  const here = api.block(chest.x, chest.y, chest.z)
  if (!here || STORAGE_BLOCKS.includes(here.name)) return null
  const whose = target.kind === 'cell' && target.x === chest.x && target.y === chest.y && target.z === chest.z ? 'the deposit=' : "the plan's"
  return `${whose} chest at ${chest.x},${chest.y},${chest.z} is missing (${here.name} there): place one, or pass deposit=false`
}

// Put `surplus` away: { deposited, storage_full, stuck, chest_missing }, each only when there is something to say
export async function storeSurplus (api, { surplus, target, cells = [], checkError = () => {} }) {
  if (!target || !Object.keys(surplus).length) return {}
  const found = await findChests(api, target).then(list => ({ list }), e => { checkError(e); return { error: e.message } })
  if (found.error) return { stuck: found.error }
  const chests = orderedChests(target, cells, found.list)
  const drops = []
  const missing = []
  let left = { ...surplus }
  for (const chest of chests) {
    if (!Object.keys(left).length) break
    const gone = missingChest(api, target, chest)
    if (gone) { missing.push(gone); continue }
    const [drop] = depositPlan(left, [chest]).drops
    if (!drop) continue
    const before = { ...api.inv() }
    const failed = await api.act('deposit', { items: drop.items, x: chest.x, y: chest.y, z: chest.z }).then(() => null, e => { checkError(e); return e.message })
    const went = failed ? wentIn(drop.items, before, api.inv()) : drop.items
    if (Object.keys(went).length) drops.push({ x: chest.x, y: chest.y, z: chest.z, items: went })
    left = minusCounts(left, went)
    if (failed && !CHEST_FULL.test(failed)) return { ...(drops.length ? { deposited: depositLine(drops) } : {}), stuck: failed, ...(missing.length ? { chest_missing: missing.join('; ') } : {}) }
  }
  const line = depositLine(drops)
  const full = storageFullLine(left, chests.length ? null : nowhere(target))
  return { ...(line ? { deposited: line } : {}), ...(full ? { storage_full: full } : {}), ...(missing.length ? { chest_missing: missing.join('; ') } : {}) }
}

// what a composite adds to its summary from one round's storing: lines join across rounds, stuck keeps its first word
export const storeInto = (summary, stored) => {
  if (stored.deposited) summary.deposited = [summary.deposited, stored.deposited].filter(Boolean).join(' ')
  if (stored.storage_full) summary.storage_full = stored.storage_full
  else delete summary.storage_full
  if (stored.stuck) summary.stuck = summary.stuck ?? stored.stuck
  if (stored.chest_missing) summary.chest_missing = stored.chest_missing
  return summary
}
