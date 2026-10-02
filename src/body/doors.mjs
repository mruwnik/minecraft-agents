// Doors and fence gates on the way: opened walking up, shut behind me, and the ones a task opened shut when it ends.
import { realCell, herdPassed, gatesByReach, openNow, shutNow, foodAway, GATE_OTHERS_NEAR, gatesLeftOpen } from '../lib.mjs'
import { findBlocksNear } from './helpers.mjs'
import { Vec3, bot, mcData, ready } from './state.mjs'
import { reflexes, fighting, surfacing, diggingOut, flee, holingUp } from './reflexes.mjs'
import { long } from './actions/tables.mjs'
import { leading, following, luring, feeding } from './actions/creature.mjs'

export const isWoodDoor = b => Boolean(b?.name?.endsWith('_door')) && b.name !== 'iron_door'
export const doorsIOpened = new Set()
export const heldOpen = new Set() // gates opened with `toggle`: they stay open until toggled shut
export const myClicks = new Map() // block -> when my own hand last clicked it
export const MY_CLICK_MS = 1500 // a gate that moves this soon after my own click on it moved because of me
export const othersToggled = new Map() // gate -> when a change that was not my doing last moved it: hands off for a minute
// everyone on the server but this body: bot.players is the tab list, so it holds players out of sight too
export const onlinePlayers = () => Object.keys(bot.players).filter(n => n !== bot.username)
const otherPlayerNear = at => Object.values(bot.players).some(p => p.entity && p.username !== bot.username && p.entity.position.distanceTo(at) <= GATE_OTHERS_NEAR)
export const doorAt = n => {
  const b = bot.blockAt(new Vec3(Math.floor(n.x), Math.floor(n.y), Math.floor(n.z)))
  return isWoodDoor(b) ? { x: b.position.x, y: b.position.y, z: b.position.z, half: b.getProperties().half } : null
}
export let doorBusy = false
export const gatesPassed = new Set() // fence gates this task walked through: their pens get a look for strays when it ends
// open wooden doors as we walk up to them, and shut the ones we opened once we're through
export async function doorTick () {
  if (doorBusy) return
  const doorIds = mcData.blocksArray.filter(isWoodDoor).map(b => b.id)
  // fence gates: the pathfinder opens them itself but never shuts them, and an open gate empties a pen. A gate is mine to
  // shut only when my own walk or click opened it (the gates.log listener decides that): "any open gate I pass" shut the
  // gate the human had just opened, 16 ms after, again and again (Perrin's idle body, 13:30Z)
  const gateIds = mcData.blocksArray.filter(b => b.name.endsWith('_fence_gate')).map(b => b.id)
  const doors = findBlocksNear({ matching: [...doorIds, ...gateIds], maxDistance: 5, count: 8 }).map(p => bot.blockAt(p)).filter(b => (b.getProperties().half ?? 'lower') === 'lower')
  if (foodAway({ held: bot.heldItem?.name, luring, feeding, gateNear: doors.some(d => d.name.endsWith('_fence_gate')), eating: Boolean(bot.autoEat?.isEating) })) {
    console.log('[food away] tempting food in hand at a gate: put away, or the animals follow me out')
    doorBusy = true
    await bot.unequip('hand').catch(() => {})
    doorBusy = false
  }
  const todo = doors.find(d => {
    const near = d.position.offset(0.5, 0, 0.5).distanceTo(bot.entity.position) < 1.6
    const open = d.getProperties().open
    // doors too, not only gates: a door found open and walked through stayed open behind me all night (my hut; Miles's cottage let a zombie in)
    const otherNear = otherPlayerNear(d.position)
    if (near && open && isWoodDoor(d) && !otherNear && !heldOpen.has(String(d.position))) doorsIOpened.add(String(d.position))
    if (near && open && d.name.endsWith('_fence_gate')) gatesPassed.add(String(d.position))
    // moving = the walk still has a goal: the pathfinder stands still while it works a gate, and "stopped" then shut the gate in my own face, for ever
    const feet = bot.entity.position.floored()
    const inDoorway = feet.x === d.position.x && feet.z === d.position.z
    const moving = Boolean(bot.pathfinder.goal) || bot.pathfinder.isMoving()
    return openNow({ near, open, door: isWoodDoor(d), moving, inDoorway }) || shutNow({ near, open, mine: doorsIOpened.has(String(d.position)), held: heldOpen.has(String(d.position)), leading: leading && !herdPassed(bot.entity.position.toArray(), d.position.offset(0.5, 0, 0.5).toArray(), following.filter(e => e.isValid).map(e => e.position.toArray())), moving, inDoorway, reflexes, otherNear, otherToggledMsAgo: Date.now() - (othersToggled.get(String(d.position)) ?? -Infinity) })
  })
  if (!todo) return
  doorBusy = true
  await bot.activateBlock(todo).catch(() => {})
  await bot.waitForTicks(3).catch(() => {})
  // believe the gate, not the click: a click that failed (out of reach at a sprint) used to strike the gate off my list, and it stayed open
  if (bot.blockAt(todo.position)?.getProperties().open) doorsIOpened.add(String(todo.position))
  else doorsIOpened.delete(String(todo.position))
  doorBusy = false
}
export async function shutGatesBehind () {
  const open = gatesLeftOpen(doorsIOpened, heldOpen, key => bot.blockAt(new Vec3(...key.match(/-?\d+/g).map(Number)))?.getProperties().open)
  const { near, far } = gatesByReach(open, bot.entity.position.toArray())
  for (const [x, y, z] of near) await long.toggle({ x, y, z, open: false })
  // a far one is named, not walked to: the body once crossed the map for two gates and left the cow it had just brought home
  return { shut: near.length, far: far && `${far}: still open and more than 32 blocks back: go and shut them (toggle x= y= z= open=false)` }
}
export async function shutTrackedGatesAfterCancel () {
  const tracked = [...doorsIOpened].filter(key => !heldOpen.has(key))
  if (!tracked.length) return {}
  const open = []
  const unknown = []
  for (const key of tracked) {
    const block = bot.blockAt(new Vec3(...key.match(/-?\d+/g).map(Number)))
    if (!block) unknown.push(key)
    else if (block.getProperties?.().open === true) open.push(key)
    else doorsIOpened.delete(key)
  }
  if (!open.length && !unknown.length) return {}
  const safeToWalk = ready && bot.entity && bot.health > 0 && !bot.isSleeping && !bot.vehicle &&
    !flee && !holingUp && !fighting && !surfacing && !diggingOut
  if (!safeToWalk) return { restorationPending: `job was cancelled with tracked gate state unresolved (${[...open, ...unknown].join('; ')}); close it after the body is safe` }
  let result = {}
  if (open.length) {
    try { result = await shutGatesBehind() }
    catch (error) { return { restorationPending: `could not close a tracked gate after cancellation: ${error.message}` } }
  }
  const unresolved = [
    ...(result.far ? [result.far] : []),
    ...(unknown.length ? [`gate state is unloaded at ${unknown.join('; ')}`] : [])
  ]
  return { ...result, ...(unresolved.length ? { restorationPending: unresolved.join('; ') } : {}) }
}
// pressed against a fence, a wall or a shut gate my centre lies inside ITS cell, and every plan starts on the wrong side of it (see realCell): three steps
// back into the cell I really stand in, before any task plans a walk
// the cell I really stand in when my centre lies in a fence's cell, else null
export function fenceEscape () {
  const here = bot.entity.position
  const mine = bot.blockAt(here.floored())
  if (mine?.boundingBox !== 'block' || !bot.pathfinder.movements?.fences.has(mine.type)) return null
  return realCell(here, (x, y, z) => [0, 1].every(up => bot.blockAt(new Vec3(x, y + up, z))?.boundingBox === 'empty'))
}
export async function leaveFenceCell () {
  const here = bot.entity.position
  const mine = bot.blockAt(here.floored())
  const cell = fenceEscape()
  if (!cell) return
  await bot.lookAt(new Vec3(cell.x + 0.5, here.y + 1.6, cell.z + 0.5), true)
  bot.setControlState('forward', true)
  for (let i = 0; i < 12 && (Math.floor(bot.entity.position.x) !== cell.x || Math.floor(bot.entity.position.z) !== cell.z); i++) await bot.waitForTicks(1)
  bot.setControlState('forward', false)
  console.log(`[left fence cell] ${mine.name} -> ${cell.x},${cell.y},${cell.z}`)
}
