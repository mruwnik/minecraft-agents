// The mineflayer layer of the engine: a small set of time-bounded operations, each a cut point. The contract is in
// engine/README.md (Primitives). Ideas copied from src/body/actions/*.mjs; nothing here imports from src/.
import vec3 from 'vec3'
import pf from 'mineflayer-pathfinder'
import { connectBot } from './connect.mjs'

const { Vec3 } = vec3
const { goals } = pf

export const REACH = 4.5
export const ATTACK_REACH = 3.5
const MONSTER_RANGE = 8
const DROP_RADIUS = 2
const DROP_WAIT_S = 1
const POLL_MS = 50
const CONTAINER = /chest|barrel|shulker_box|furnace|smoker|hopper|dispenser|dropper|brewing_stand/
const DESTS = ['hand', 'off-hand', 'head', 'torso', 'legs', 'feet']
const DEFAULT_RADIUS = 16
const KINDS = ['hostile', 'passive', 'player', 'item', 'other']

const sleepMs = ms => new Promise(resolve => setTimeout(resolve, ms))
const xyz = v => ({ x: v.x, y: v.y, z: v.z })
const dist = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)
const center = p => ({ x: p.x + 0.5, y: p.y + 0.5, z: p.z + 0.5 })
const isAir = name => name === 'air' || name.endsWith('_air')
const isNum = n => typeof n === 'number' && Number.isFinite(n)
const isPos = p => Boolean(p) && isNum(p.x) && isNum(p.y) && isNum(p.z)
const cell = p => ({ x: Math.floor(p.x), y: Math.floor(p.y), z: Math.floor(p.z) })
const vec = p => new Vec3(p.x, p.y, p.z)

const codedError = (code, message) => Object.assign(new Error(message), { code, [code === 'cut' ? 'cut' : 'badArgs']: true })
export const cutError = () => codedError('cut', 'cut: the ownership token no longer matches')
const badArgs = message => codedError('bad-args', message)
const need = (ok, message) => { if (!ok) throw badArgs(message) }

const entityKind = e => {
  if (e.type === 'player') return 'player'
  if (e.name === 'item' || e.name === 'item_stack' || e.displayName === 'Item') return 'item'
  if (e.type === 'hostile' || /hostile/i.test(e.kind ?? '')) return 'hostile'
  if (['passive', 'animal', 'ambient', 'water_creature'].includes(e.type) || /passive|animal/i.test(e.kind ?? '')) return 'passive'
  return 'other'
}

const droppedItem = (bot, e) => {
  const item = e.getDroppedItem?.()
  if (item) return { name: item.name, count: item.count }
  const meta = (e.metadata ?? []).find(m => m && typeof m === 'object' && m.itemId !== undefined)
  const def = meta && bot.registry?.items?.[meta.itemId]
  return def ? { name: def.name, count: meta.itemCount ?? 1 } : null
}

const itemCounts = items => items.reduce((acc, i) => ({ ...acc, [i.name]: (acc[i.name] ?? 0) + i.count }), {})
const gained = (before, after) => Object.entries(after)
  .map(([name, count]) => ({ name, count: count - (before[name] ?? 0) }))
  .filter(g => g.count > 0)

// Builds the primitives over an already spawned bot. `timeScale` multiplies every time bound (tests shrink it).
export function createPrimitivesFromBot (bot, { timeScale = 1 } = {}) {
  let owner = null
  const inflight = new Set()
  const listeners = new Set()

  const isOwner = token => token !== null && token !== undefined && token === owner
  const setOwner = token => {
    owner = token ?? null
    for (const call of [...inflight]) if (call.token !== owner) call.cut()
  }

  const here = () => xyz(bot.entity.position)
  const eye = () => ({ x: bot.entity.position.x, y: bot.entity.position.y + (bot.entity.height ?? 1.62), z: bot.entity.position.z })
  const inventory = () => bot.inventory.items()
  const countsNow = () => itemCounts(inventory())
  const hostilesNear = () => Object.values(bot.entities)
    .filter(e => e !== bot.entity && entityKind(e) === 'hostile')
    .filter(e => Math.hypot(e.position.x - bot.entity.position.x, e.position.z - bot.entity.position.z) <= MONSTER_RANGE && Math.abs(e.position.y - bot.entity.position.y) <= 5)

  // Runs `body(ctx)` as one call owned by `token`. The call ends the moment the owner changes (rejects with cut), or
  // when its time bound passes (resolves onTimeout(), default {status: 'timeout'}); both run the aborts the body
  // registered, and `ctx.alive()` then throws so the body stops reaching the bot. Entry with a stale token throws.
  const act = (token, { boundS, onTimeout = () => ({ status: 'timeout' }) }, body) => {
    if (!isOwner(token)) return Promise.reject(cutError())
    return new Promise((resolve, reject) => {
      const aborts = []
      let settled = false
      const runAborts = () => aborts.splice(0).reverse().forEach(fn => { try { Promise.resolve(fn()).catch(() => {}) } catch { /* the bot may already be gone */ } })
      const settle = then => value => {
        if (settled) return
        settled = true
        clearTimeout(timer)
        inflight.delete(call)
        then(value)
      }
      const call = { token, cut: () => { if (settled) return; runAborts(); settle(reject)(cutError()) } }
      const ctx = {
        alive: () => { if (settled || !isOwner(token)) throw cutError() },
        onAbort: fn => aborts.push(fn)
      }
      const timer = setTimeout(() => { if (settled) return; runAborts(); settle(resolve)(onTimeout()) }, Math.max(1, boundS * 1000 * timeScale))
      inflight.add(call)
      Promise.resolve().then(() => body(ctx)).then(settle(resolve), err => settled ? undefined : settle(reject)(err))
    })
  }

  // a walk toward `goal`, abortable; resolves true when the pathfinder reached it, false when it gave up
  const walk = async (ctx, goal) => {
    ctx.onAbort(() => { bot.pathfinder.setGoal(null); bot.clearControlStates() })
    ctx.alive()
    const reached = await bot.pathfinder.goto(goal).then(() => true, () => false)
    ctx.alive()
    return reached
  }

  // ---- sensing ----

  const self = () => {
    const timeOfDay = bot.time.timeOfDay
    return {
      username: bot.username,
      pos: here(),
      health: bot.health,
      food: bot.food,
      timeOfDay,
      isDay: timeOfDay < 12542 || timeOfDay > 23460,
      held: bot.heldItem?.name ?? null,
      inventory: inventory().map(i => ({ name: i.name, count: i.count, slot: i.slot }))
    }
  }

  const entities = ({ radius = DEFAULT_RADIUS, kind, names, max = 32 } = {}) => {
    const me = here()
    return Object.values(bot.entities)
      .filter(e => e !== bot.entity && e.position)
      .map(e => ({ e, distance: dist(me, e.position), kind: entityKind(e) }))
      .filter(({ e, distance, kind: k }) => distance <= radius && (!kind || k === kind) && (!names || names.includes(e.name ?? e.username)))
      .sort((a, b) => a.distance - b.distance)
      .slice(0, max)
      .map(({ e, distance, kind: k }) => ({
        id: e.id,
        name: e.name ?? e.username,
        kind: k,
        pos: xyz(e.position),
        distance,
        ...(k === 'item' && { item: droppedItem(bot, e) })
      }))
  }

  const blocks = ({ radius = DEFAULT_RADIUS, names, match, max = 64 } = {}) => {
    const wanted = names && new Set(names)
    const matching = block => Boolean(block) && (wanted ? wanted.has(block.name) : match ? match(block.name) : !isAir(block.name))
    const me = here()
    return bot.findBlocks({ matching, maxDistance: radius, count: max })
      .map(p => ({ name: bot.blockAt(p).name, pos: xyz(p), distance: dist(me, p) }))
      .sort((a, b) => a.distance - b.distance)
  }

  const blockAt = pos => {
    const block = bot.blockAt(vec(pos))
    return block ? { name: block.name, pos: xyz(block.position) } : null
  }

  // ---- acting ----

  const moveTo = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos), 'moveTo needs pos {x, y, z}')
    const { range = 1, timeoutS = 20, maxDistance = 64 } = a
    const target = cell(a.pos)
    const start = here()
    const before = dist(start, target)
    const capped = before > maxDistance
    const goal = capped
      ? new goals.GoalNearXZ(Math.round(start.x + (target.x - start.x) * maxDistance / before), Math.round(start.z + (target.z - start.z) * maxDistance / before), 2)
      : new goals.GoalNear(target.x, target.y, target.z, range)
    const outcome = (reached) => {
      const distance = dist(here(), target)
      const status = reached && !capped ? 'arrived' : distance < before - 1 ? 'partial' : 'blocked'
      return { status, pos: here(), distance }
    }
    return act(token, { boundS: Math.min(timeoutS, 60), onTimeout: () => outcome(false) }, async ctx => outcome(await walk(ctx, goal)))
  }

  const dropsNear = p => Object.values(bot.entities)
    .filter(e => entityKind(e) === 'item' && dist(center(p), e.position) <= DROP_RADIUS)
    .map(e => ({ id: e.id, ...droppedItem(bot, e), pos: xyz(e.position) }))

  const dig = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos), 'dig needs pos {x, y, z}')
    const p = cell(a.pos)
    return act(token, { boundS: 10 }, async ctx => {
      const block = bot.blockAt(vec(p))
      if (!block || isAir(block.name)) return { status: 'missing' }
      if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
      if (!block.diggable) return { status: 'cannot' }
      ctx.onAbort(() => bot.stopDigging())
      ctx.alive()
      await bot.dig(block, true)
      ctx.alive()
      const deadline = Date.now() + DROP_WAIT_S * 1000 * timeScale
      let drops = dropsNear(p)
      while (drops.length === 0 && Date.now() < deadline) {
        await sleepMs(POLL_MS * timeScale)
        ctx.alive()
        drops = dropsNear(p)
      }
      return { status: 'dug', block: block.name, drops }
    })
  }

  const supportFor = p => [[0, -1, 0], [1, 0, 0], [-1, 0, 0], [0, 0, 1], [0, 0, -1], [0, 1, 0]]
    .map(([dx, dy, dz]) => ({ ref: bot.blockAt(new Vec3(p.x + dx, p.y + dy, p.z + dz)), face: new Vec3(-dx, -dy, -dz) }))
    .find(({ ref }) => ref && !isAir(ref.name) && ref.boundingBox === 'block')

  const place = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos) && typeof a.item === 'string', 'place needs pos {x, y, z} and item')
    const p = cell(a.pos)
    return act(token, { boundS: 5 }, async ctx => {
      const there = bot.blockAt(vec(p))
      if (there && !isAir(there.name) && there.name !== 'water' && there.name !== 'lava') return { status: 'occupied' }
      const item = inventory().find(i => i.name === a.item)
      if (!item) return { status: 'no-item' }
      const support = supportFor(p)
      if (!support) return { status: 'no-support' }
      if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
      ctx.alive()
      await bot.equip(item, 'hand')
      ctx.alive()
      await bot.placeBlock(support.ref, support.face)
      return { status: 'placed', block: a.item }
    })
  }

  const collect = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isNum(a.id), 'collect needs an entity id')
    const { id, timeoutS = 10 } = a
    const before = countsNow()
    return act(token, { boundS: Math.min(timeoutS, 20), onTimeout: () => ({ status: 'timeout', gained: gained(before, countsNow()) }) }, async ctx => {
      const target = bot.entities[id]
      if (!target || entityKind(target) !== 'item') return { status: 'gone' }
      const reached = await walk(ctx, new goals.GoalNear(target.position.x, target.position.y, target.position.z, 1))
      if (!reached && bot.entities[id]) return { status: 'unreachable', gained: gained(before, countsNow()) }
      while (bot.entities[id]) {
        await sleepMs(POLL_MS * timeScale)
        ctx.alive()
      }
      const got = gained(before, countsNow())
      return got.length ? { status: 'collected', gained: got } : { status: 'gone', gained: [] }
    })
  }

  const withWindow = async (ctx, block, use) => {
    const win = await bot.openContainer(block)
    ctx.onAbort(() => bot.closeWindow(win))
    try {
      ctx.alive()
      return await use(win)
    } finally {
      bot.closeWindow(win)
    }
  }
  const containerAt = p => {
    const block = bot.blockAt(vec(p))
    return block && CONTAINER.test(block.name) ? block : null
  }
  const slots = items => items.map(i => ({ name: i.name, count: i.count, slot: i.slot }))

  const inspectContainer = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos), 'inspectContainer needs pos {x, y, z}')
    const p = cell(a.pos)
    return act(token, { boundS: 5 }, async ctx => {
      const block = containerAt(p)
      if (!block) return { status: 'missing' }
      if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
      return withWindow(ctx, block, async win => ({ status: 'ok', items: slots(win.containerItems()) }))
    })
  }

  const transfer = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos) && typeof a.item === 'string' && ['deposit', 'withdraw'].includes(a.direction), 'transfer needs pos, item and direction deposit|withdraw')
    const p = cell(a.pos)
    return act(token, { boundS: 5 }, async ctx => {
      const block = containerAt(p)
      if (!block) return { status: 'missing' }
      if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
      const type = bot.registry.itemsByName[a.item]?.id
      return withWindow(ctx, block, async win => {
        const source = a.direction === 'deposit' ? inventory() : win.containerItems()
        const available = source.filter(i => i.name === a.item).reduce((sum, i) => sum + i.count, 0)
        const count = Math.min(a.count ?? available, available)
        if (count <= 0 || type === undefined) return { status: 'no-item', moved: 0 }
        ctx.alive()
        const failure = await (a.direction === 'deposit' ? win.deposit(type, null, count) : win.withdraw(type, null, count)).then(() => null, err => err)
        ctx.alive()
        if (failure && !/full|room|space/i.test(failure.message)) throw failure
        return failure ? { status: 'full', moved: 0 } : { status: 'ok', moved: count }
      })
    })
  }

  const equip = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    const { item: name, dest = 'hand' } = a
    need(typeof name === 'string' && DESTS.includes(dest), 'equip needs item and dest hand|off-hand|head|torso|legs|feet')
    return act(token, { boundS: 2 }, async ctx => {
      const item = inventory().find(i => i.name === name)
      if (!item) return { status: 'no-item' }
      await bot.equip(item, dest)
      return { status: 'equipped' }
    })
  }

  const bestFood = () => inventory()
    .filter(i => bot.registry.foodsByName?.[i.name])
    .sort((a, b) => bot.registry.foodsByName[b.name].foodPoints - bot.registry.foodsByName[a.name].foodPoints)[0]

  const eat = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    const startFood = bot.food
    return act(token, { boundS: 5, onTimeout: () => bot.food > startFood ? { status: 'ate', item: a.item ?? null, food: bot.food } : { status: 'timeout' } }, async ctx => {
      const item = a.item ? inventory().find(i => i.name === a.item) : bestFood()
      if (!item) return { status: 'no-food' }
      if (bot.food >= 20) return { status: 'full' }
      ctx.onAbort(() => bot.deactivateItem())
      await bot.equip(item, 'hand')
      ctx.alive()
      await bot.consume()
      return { status: 'ate', item: item.name, food: bot.food }
    })
  }

  const attack = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isNum(a.id), 'attack needs an entity id')
    return act(token, { boundS: 1 }, async ctx => {
      const target = bot.entities[a.id]
      if (!target) return { status: 'gone' }
      if (dist(eye(), { x: target.position.x, y: target.position.y + (target.height ?? 1) / 2, z: target.position.z }) > ATTACK_REACH) return { status: 'out-of-reach' }
      await bot.attack(target)
      ctx.alive()
      await sleepMs(POLL_MS * timeScale * 2)
      ctx.alive()
      const now = bot.entities[a.id]
      const health = typeof now?.health === 'number' ? now.health : undefined
      const killed = !now || (health !== undefined && health <= 0)
      return { status: killed ? 'killed' : 'hit', ...(health !== undefined && { health }) }
    })
  }

  const sleep = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos), 'sleep needs pos {x, y, z}')
    const p = cell(a.pos)
    return act(token, { boundS: 5 }, async ctx => {
      const block = bot.blockAt(vec(p))
      if (!block || !block.name.endsWith('_bed')) return { status: 'missing' }
      if (dist(eye(), center(p)) > REACH) return { status: 'unreachable' }
      const { timeOfDay } = bot.time
      if (timeOfDay < 12542 || timeOfDay > 23460) return { status: 'not-night' }
      if (hostilesNear().length > 0) return { status: 'monsters-near' }
      ctx.onAbort(() => bot.wake())
      ctx.alive()
      const failure = await bot.sleep(block).then(() => null, err => err)
      ctx.alive()
      if (failure && /occupied/i.test(failure.message)) return { status: 'occupied' }
      if (failure) throw failure
      return { status: 'sleeping' }
    })
  }

  const look = async (token, a = {}) => {
    if (!isOwner(token)) throw cutError()
    need(isPos(a.pos) || (isNum(a.yaw) && isNum(a.pitch)), 'look needs pos {x, y, z} or yaw and pitch')
    return act(token, { boundS: 1 }, async ctx => {
      await (isPos(a.pos) ? bot.lookAt(vec(a.pos), true) : bot.look(a.yaw, a.pitch, true))
      return { status: 'ok' }
    })
  }

  // ---- body events ----

  const emit = event => listeners.forEach(fn => fn(event))
  let lastHealth = bot.health
  bot.on('health', () => {
    if (bot.health < lastHealth) emit({ kind: 'hurt', health: bot.health, food: bot.food })
    lastHealth = bot.health
  })
  bot.on('death', () => emit({ kind: 'died', pos: here() }))
  bot.on('respawn', () => emit({ kind: 'respawned' }))
  bot.on('chat', (from, message) => emit({ kind: 'chat', from, message }))
  bot.on('wake', () => emit({ kind: 'woke' }))
  bot.on('spawn', () => emit({ kind: 'spawned' }))
  bot.on('end', reason => emit({ kind: 'disconnected', reason: String(reason) }))
  bot.on('kicked', reason => emit({ kind: 'disconnected', reason: JSON.stringify(reason) }))
  const onBodyEvent = listener => {
    listeners.add(listener)
    return () => listeners.delete(listener)
  }

  const close = async () => {
    setOwner(null)
    bot.quit()
  }

  return { setOwner, isOwner, self, entities, blocks, blockAt, moveTo, dig, place, collect, inspectContainer, transfer, equip, eat, attack, sleep, look, onBodyEvent, close }
}

// The README's factory: connects, resolves once spawned.
export async function createPrimitives (opts) {
  return createPrimitivesFromBot(await connectBot(opts))
}
