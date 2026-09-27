import { cellKey as breedKey, safeFullBlock as safeFull, buildingMaterial as breedMaterial, woodenGate as breedGate } from '../enclosure/blocks.mjs'
import { roomPlan, roomInside } from '../enclosure/layout.mjs'
export { cellKey as breedKey, safeFullBlock as breedFullBlock, buildingMaterial as breedMaterial, woodenGate as breedGate } from '../enclosure/blocks.mjs'
// Geometry and observations for a bounded, roofed Java villager breeder.
export const BREED_FOOD = { bread: 3, carrot: 12, potato: 12, beetroot: 12 }
// Paper 26.2 TradeWithVillager throws half of a stack above 32; at 25..32 it
// throws only the amount above 24. Provision a shareable batch, then keep its
// reserve fixed as births consume food rather than topping up to 33 each round.
export function breedFoodReserve (births, food = 'bread', savedReserve) {
  if (!BREED_FOOD[food]) throw new Error('food= must be bread, carrot, potato or beetroot')
  if (!Number.isInteger(births) || births < 0 || births > 24) throw new Error('birth count must be an integer within 0..24')
  if (savedReserve !== undefined) {
    if (!Number.isInteger(savedReserve) || savedReserve < 0 || savedReserve > 33) throw new Error('shared food reserve must be a saved plan count within 0..33')
    return savedReserve
  }
  return births ? Math.max(food === 'bread' ? 4 : 16, 33 - births * 2 * BREED_FOOD[food]) : 0
}
const air = b => ['air', 'cave_air', 'void_air'].includes(b?.name)
const vegetation = b => ['short_grass', 'tall_grass', 'leaf_litter', 'fern', 'large_fern', 'dandelion', 'poppy', 'blue_orchid', 'allium', 'azure_bluet', 'red_tulip', 'orange_tulip', 'white_tulip', 'pink_tulip', 'oxeye_daisy', 'cornflower', 'lily_of_the_valley', 'sunflower', 'lilac', 'rose_bush', 'peony', 'dead_bush'].includes(b?.name)
export function breedPlan ({ x, y, z, target, size, entryX, entryZ, airlock = false }) {
  if (![x, y, z, target].every(Number.isInteger) || target < 2 || target > 24) throw new Error('integer x= y= z= and target=2..24 required')
  const capacity = n => Math.floor(n / 2) * Math.floor((n + 1) / 3)
  let width = size ?? 5
  if (size === undefined) while (capacity(width) < target) width++
  if (!Number.isInteger(width) || width < 5 || width > 15 || capacity(width) < target) throw new Error('size= must be 5..15 with enough two-cell bed slots for target')
  const at = (dx, dy, dz) => ({ x: x + dx, y: y + dy, z: z + dz })
  const entryGiven = entryX !== undefined || entryZ !== undefined
  if (entryGiven && (!Number.isInteger(entryX) || !Number.isInteger(entryZ) || entryX !== x + width || entryZ < z || entryZ >= z + width)) throw new Error('entryX/entryZ must name one east wall cell; keep size= fixed for this arrival entry')
  const entry = entryGiven ? { x: entryX, y, z: entryZ } : null
  if (airlock && (!entry || width < 8 || entry.z !== z + width - 1)) throw new Error('airlock=true needs size>=8 and an east entry at the last interior row')
  const room = roomPlan({ x, y, z, width, entry, airlock })
  const bedSlots = []
  // Keep x=0 clear from the gate to every cross aisle; reaching a bed never
  // depends on climbing over another bed or ducking below the gate lintel.
  for (let dz = 0; dz + 1 < width; dz += 3) for (let dx = 1; dx < width; dx += 2) {
    if (airlock && dx >= width - 3 && dz + 1 >= width - 2) continue
    if (entry && dx === width - 1 && (entry.z === z + dz || entry.z === z + dz + 1)) continue
    bedSlots.push({ foot: at(dx, 0, dz), head: at(dx, 0, dz + 1) })
  }
  if (bedSlots.length < target) throw new Error('size= has insufficient bed slots while preserving the arrival entry aisle; use a larger blueprint')
  const beds = bedSlots.slice(0, target)
  return { ...room, target, beds, bedSlots }
}
export const breedInside = roomInside
export function breedCensus (plan, entities) {
  const seen = new Map()
  for (const e of entities) {
    const xyz = String(e.exact ?? e.at ?? '').split(',').map(Number)
    if (xyz.length !== 3 || xyz.some(n => !Number.isFinite(n))) throw new Error('villager observation lacks exact position')
    const p = { x: xyz[0], y: xyz[1], z: xyz[2] }
    if (!breedInside(plan, p)) continue
    if (!/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(e.uuid ?? '')) throw new Error('villager observation lacks UUID')
    const metadata = typeof e.metadata === 'string' ? JSON.parse(e.metadata) : e.metadata
    const baby = typeof e.baby === 'boolean' ? e.baby : metadata?.[16]
    if (typeof baby !== 'boolean') throw new Error(`villager ${e.uuid} has unknown adult/baby metadata`)
    if (e.vehicleId !== undefined && e.vehicleId !== null) throw new Error(`villager ${e.uuid} must be on foot before breeding`)
    seen.set(e.uuid, { ...e, position: p, baby, sleeping: e.sleeping === true || metadata?.[6] === 2 })
  }
  return [...seen.values()]
}
export function breedPreflight (plan, blockAt, material = 'cobblestone', gateItem = 'oak_fence_gate') {
  if (!breedMaterial(material)) throw new Error('block= must be a full solid building block')
  if (!breedGate(gateItem)) throw new Error('gate= must be a fence gate item')
  const read = p => { const b = blockAt(p.x, p.y, p.z); if (!b) throw new Error(`unloaded breeder cell ${breedKey(p)}`); return b }
  const needed = []; const missingBeds = []; const clear = new Map()
  const vacant = (p, b) => {
    if (vegetation(b)) { clear.set(breedKey(p), { ...p, name: b.name }); return true }
    return air(b)
  }
  const bedCells = new Set((plan.bedSlots ?? plan.beds).flatMap(b => [breedKey(b.foot), breedKey(b.head)]))
  const lights = new Set(plan.lights.filter(p => p.y === plan.y).map(breedKey))
  for (let dx = -1; dx <= plan.width; dx++) for (let dz = -1; dz <= plan.width; dz++) {
    const p = { x: plan.x + dx, y: plan.y - 1, z: plan.z + dz }
    if (!safeFull(read(p))) throw new Error(`breeder needs flat solid dry floor at ${breedKey(p)}; partial or harmful support is not suitable`)
  }
  const reserved = new Set([...plan.shell, ...plan.gates.flatMap(p => [p, { ...p, y: p.y + 1 }])].map(breedKey))
  for (let dx = 0; dx < plan.width; dx++) for (let dz = 0; dz < plan.width; dz++) for (let dy = 0; dy < 3; dy++) {
    const p = { x: plan.x + dx, y: plan.y + dy, z: plan.z + dz }; const b = read(p)
    if (reserved.has(breedKey(p))) continue
    if (vacant(p, b) || (dy === 0 && bedCells.has(breedKey(p)) && /_bed$/.test(b.name)) || (dy === 0 && lights.has(breedKey(p)) && b.name === 'torch')) continue
    throw new Error(`breeder aisle or bed headroom blocked by ${b.name} at ${breedKey(p)}`)
  }
  for (const p of plan.shell) {
    const b = read(p)
    if (safeFull(b)) continue
    if (!vacant(p, b)) throw new Error(`breeder wall/roof blocked by ${b.name} at ${breedKey(p)}`)
    needed.push({ ...p, item: material })
  }
  for (const p of plan.gates) {
    const gate = read(p)
    if (breedGate(gate.name) && !['east', 'west'].includes(gate.properties?.facing)) throw new Error('breeder gate must face east or west across its doorway')
    if (!breedGate(gate.name)) {
      if (!vacant(p, gate)) throw new Error(`breeder gate cell occupied by ${gate.name}`)
      needed.push({ ...p, item: gateItem, facing: 'east' })
    }
    const head = { ...p, y: plan.y + 1 }
    if (!vacant(head, read(head))) throw new Error('breeder gate doorway head cell must be air')
  }
  let validBeds = 0
  const emptyBeds = []
  for (const bed of plan.bedSlots ?? plan.beds) {
    const f = read(bed.foot); const h = read(bed.head)
    if (vacant(bed.foot, f) && vacant(bed.head, h)) { emptyBeds.push(bed); continue }
    if (f.name !== h.name || !/_bed$/.test(f.name) || f.properties?.part !== 'foot' || h.properties?.part !== 'head' || f.properties?.facing !== 'south' || h.properties?.facing !== 'south') throw new Error(`bed at ${breedKey(bed.foot)} is partial or has wrong facing; preserve it and repair deliberately`)
    validBeds++
  }
  missingBeds.push(...emptyBeds.slice(0, Math.max(0, plan.target - validBeds)))
  for (const p of plan.lights) {
    const b = read(p)
    if (b.name === 'torch') continue
    if (!vacant(p, b)) throw new Error(`breeder torch cell occupied at ${breedKey(p)}`)
    needed.push({ ...p, item: 'torch' })
  }
  return { needed, missingBeds, clear: [...clear.values()], validBeds }
}
export function breedBill (preflight, inventory, births, food = 'bread', bedItem = 'white_bed', foodCredit = 0, extras = 0) {
  if (!BREED_FOOD[food]) throw new Error('food= must be bread, carrot, potato or beetroot')
  if (!/^(white|orange|magenta|light_blue|yellow|lime|pink|gray|light_gray|cyan|purple|blue|brown|green|red|black)_bed$/.test(bedItem)) throw new Error('bed= must be a colored bed item')
  const bill = {}
  for (const p of preflight.needed) bill[p.item] = (bill[p.item] ?? 0) + 1
  if (preflight.missingBeds.length) bill[bedItem] = preflight.missingBeds.length
  if (!Number.isInteger(foodCredit) || foodCredit < 0) throw new Error('food credit must be a nonnegative observed receipt count')
  if (births) bill[food] = (bill[food] ?? 0) + Math.max(0, 2 * BREED_FOOD[food] * births + extras - foodCredit)
  const shortages = Object.entries(bill).filter(([item, n]) => (inventory[item] ?? 0) < n).map(([item, n]) => `${item}:${n - (inventory[item] ?? 0)} (need ${n}, carry ${inventory[item] ?? 0})`)
  return { bill, shortages }
}

export function breedFeedGrounded (plan, adult, blockAt) {
  if (!adult || Math.abs(adult.y - plan.y) > 0.05) return false
  return safeFull(blockAt(Math.floor(adult.x), plan.y - 1, Math.floor(adult.z)))
}

export function breedFeedStance (plan, adult, blockAt, other = null) {
  const options = []
  for (let x = plan.x; x < plan.x + plan.width; x++) for (let z = plan.z; z < plan.z + plan.width; z++) {
    if (!breedInside(plan, { x: x + 0.5, y: plan.y, z: z + 0.5 })) continue
    const b = blockAt(x, plan.y, z)
    if (!(air(b) || b?.name === 'torch') || !air(blockAt(x, plan.y + 1, z)) || !safeFull(blockAt(x, plan.y - 1, z))) continue
    const distance = Math.hypot(x + 0.5 - adult.x, plan.y - adult.y, z + 0.5 - adult.z)
    const otherDistance = other ? Math.hypot(x + 0.5 - other.x, plan.y - other.y, z + 0.5 - other.z) : Infinity
    if (other && (otherDistance < 4 || otherDistance < distance + 0.8 || distance < 1.5 || distance > 2.25)) continue
    if (distance >= 0.8 && distance <= 2.5) options.push({ x, y: plan.y, z, distance })
  }
  options.sort((a, b) => a.distance - b.distance)
  if (!options.length) throw new Error('no dry solid aisle stance within three blocks of the selected adult')
  const { distance, ...stance } = options[0]
  return stance
}
