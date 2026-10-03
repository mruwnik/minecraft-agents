// A scriptable fake world exposing the primitives interface from README.md.
// Engine and job tests drive it; it never talks to a server.

const key = ({ x, y, z }) => `${x},${y},${z}`
const parseKey = (k) => { const [x, y, z] = k.split(',').map(Number); return { x, y, z } }
const dist = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)
const clone = (v) => structuredClone(v)

const REACH = 4.5
const FOODS = ['cooked_beef', 'cooked_porkchop', 'bread', 'baked_potato', 'cooked_chicken', 'carrot', 'apple', 'sweet_berries', 'beef', 'porkchop', 'mutton', 'chicken', 'rabbit']

export const isDayAt = (t) => t < 12542 || t > 23460

export class CutError extends Error {
  constructor () { super('cut: the ownership token changed'); this.code = 'cut' }
}

const OFFLINE_DEFAULT_MS = 5 * 60 * 1000
const OFFLINE_MAX_MS = 10 * 60 * 1000

const defaultSelf = {
  username: 'Fake',
  pos: { x: 0, y: 64, z: 0 },
  health: 20,
  food: 20,
  foodSaturation: 5,
  oxygen: 20,
  onFire: false,
  inWater: false,
  inLava: false,
  isSleeping: false,
  experience: { level: 0, points: 0, progress: 0 },
  dimension: 'overworld'
}

// players are awake and have a username, and creepers carry creeper: true, unless the spec says otherwise
const withEntityDefaults = (e) => ({
  health: 20,
  ...(e.kind === 'player' && { sleeping: false, username: e.name }),
  ...(e.name === 'creeper' && { creeper: true }),
  ...e
})

function initialState (spec) {
  return {
    self: { ...defaultSelf, ...clone(spec.self ?? {}), held: spec.self?.held ?? null },
    time: spec.time ?? 1000,
    blocks: new Map(Object.entries(spec.blocks ?? {})),
    unloaded: new Set(spec.unloaded ?? []), // "x,y,z" cells in an unloaded chunk: blockAt returns null there
    ages: new Map(Object.entries(spec.ages ?? {})), // "x,y,z" -> crop age, for blocks that have one
    entities: clone(spec.entities ?? []).map(withEntityDefaults),
    inventory: clone(spec.inventory ?? []),
    containers: new Map(Object.entries(clone(spec.containers ?? {}))),
    drops: { ...(spec.drops ?? {}) },
    unreachable: new Set(spec.unreachable ?? []),
    noPath: new Set(spec.noPath ?? []), // moveTo targets the pathfinder resolves on with no path
    swimFails: spec.swimFails ?? false,
    nextEntityId: 1000,
    offline: false,
    offlineScale: spec.offlineScale ?? 0.001 // offline waits ms * this, so tests need not sit out minutes
  }
}

function addItem (items, name, count) {
  const have = items.find(i => i.name === name)
  if (have) { have.count += count; return }
  items.push({ name, count })
}

function takeItem (items, name, count) {
  const have = items.find(i => i.name === name)
  if (!have) return 0
  const taken = Math.min(have.count, count)
  have.count -= taken
  if (have.count === 0) items.splice(items.indexOf(have), 1)
  return taken
}

const withSlots = (items) => items.map((i, slot) => ({ name: i.name, count: i.count, slot }))

function stepToward (from, to, max) {
  const d = dist(from, to)
  const f = max / d
  return {
    x: Math.round(from.x + (to.x - from.x) * f),
    y: Math.round(from.y + (to.y - from.y) * f),
    z: Math.round(from.z + (to.z - from.z) * f)
  }
}

function defaultActs (s) {
  const blockName = (pos) => s.blocks.get(key(pos)) ?? 'air'
  const near = (pos) => dist(s.self.pos, pos) <= REACH
  const entity = (id) => s.entities.find(e => e.id === id)
  const spawnItem = (pos, name, count) => {
    const e = { id: s.nextEntityId++, name: 'item', kind: 'item', pos: { ...pos }, item: { name, count } }
    s.entities.push(e)
    return e
  }

  // Where the body stands decides lava and water: lava is left by stepping out of it, water puts out fire.
  const settle = () => {
    const here = blockName(s.self.pos)
    s.self.inLava = here === 'lava'
    s.self.inWater = here === 'water'
    if (s.self.inWater) s.self.onFire = false
  }

  return {
    async moveTo (token, { pos, range = 1, maxDistance = 64 }) {
      if (s.unreachable.has(key(pos))) return { status: 'blocked', pos: { ...s.self.pos }, distance: dist(s.self.pos, pos) }
      if (s.noPath.has(key(pos))) return { status: 'blocked', reason: 'noPath', pos: { ...s.self.pos }, distance: dist(s.self.pos, pos) }
      const d = dist(s.self.pos, pos)
      if (d <= range) return { status: 'arrived', pos: { ...s.self.pos }, distance: d }
      if (d > maxDistance) {
        s.self.pos = stepToward(s.self.pos, pos, maxDistance)
        return { status: 'partial', pos: { ...s.self.pos }, distance: dist(s.self.pos, pos) }
      }
      s.self.pos = { ...pos }
      settle()
      return { status: 'arrived', pos: { ...pos }, distance: 0 }
    },

    async dig (token, { pos }) {
      const name = blockName(pos)
      if (name === 'air') return { status: 'missing' }
      if (!near(pos)) return { status: 'unreachable' }
      if (name === 'bedrock') return { status: 'cannot' }
      s.blocks.delete(key(pos))
      s.ages.delete(key(pos))
      const dropName = s.drops[name] === undefined ? name : s.drops[name]
      const drops = dropName ? [spawnItem(pos, dropName, 1)] : []
      return { status: 'dug', block: name, drops: drops.map(e => ({ id: e.id, name: e.item.name, count: e.item.count, pos: { ...e.pos } })) }
    },

    async place (token, { pos, item }) {
      if (!near(pos)) return { status: 'unreachable' }
      if (blockName(pos) !== 'air') return { status: 'occupied' }
      if (takeItem(s.inventory, item, 1) === 0) return { status: 'no-item' }
      if (item === 'water_bucket') { // pours water and leaves the empty bucket
        addItem(s.inventory, 'bucket', 1)
        s.blocks.set(key(pos), 'water')
        if (key(pos) === key(s.self.pos)) settle()
        return { status: 'placed', block: 'water' }
      }
      s.blocks.set(key(pos), item)
      return { status: 'placed', block: item }
    },

    async collect (token, { id }) {
      const e = entity(id)
      if (!e || e.kind !== 'item') return { status: 'gone', gained: [] }
      if (s.unreachable.has(key(e.pos))) return { status: 'unreachable', gained: [] }
      s.self.pos = { ...e.pos }
      s.entities.splice(s.entities.indexOf(e), 1)
      addItem(s.inventory, e.item.name, e.item.count)
      return { status: 'collected', gained: [{ ...e.item }] }
    },

    async inspectContainer (token, { pos }) {
      const items = s.containers.get(key(pos))
      if (!items) return { status: 'missing' }
      if (!near(pos)) return { status: 'unreachable' }
      return { status: 'ok', items: withSlots(items) }
    },

    async transfer (token, { pos, direction, item, count }) {
      const items = s.containers.get(key(pos))
      if (!items) return { status: 'missing', moved: 0 }
      if (!near(pos)) return { status: 'unreachable', moved: 0 }
      const [from, to] = direction === 'deposit' ? [s.inventory, items] : [items, s.inventory]
      const moved = takeItem(from, item, count)
      if (moved === 0) return { status: 'no-item', moved: 0 }
      addItem(to, item, moved)
      return { status: 'ok', moved }
    },

    async equip (token, { item }) {
      if (!s.inventory.some(i => i.name === item)) return { status: 'no-item' }
      s.self.held = item
      return { status: 'equipped' }
    },

    async eat (token, { item } = {}) {
      if (s.self.food >= 20) return { status: 'full', food: s.self.food }
      const food = item ?? FOODS.find(f => s.inventory.some(i => i.name === f))
      if (!food || takeItem(s.inventory, food, 1) === 0) return { status: 'no-food' }
      s.self.food = Math.min(20, s.self.food + 5)
      return { status: 'ate', item: food, food: s.self.food }
    },

    async attack (token, { id }) {
      const e = entity(id)
      if (!e) return { status: 'gone' }
      if (!near(e.pos)) return { status: 'out-of-reach' }
      e.health -= 5
      if (e.health > 0) return { status: 'hit', health: e.health }
      s.entities.splice(s.entities.indexOf(e), 1)
      for (const d of e.drops ?? []) spawnItem(e.pos, d.name, d.count)
      return { status: 'killed', health: 0 }
    },

    async sleep (token, { pos }) {
      if (isDayAt(s.time)) return { status: 'not-night' }
      if (!blockName(pos).endsWith('_bed')) return { status: 'missing' }
      if (!near(pos)) return { status: 'unreachable' }
      s.time = 0
      return { status: 'sleeping' }
    },

    // Rises to the top water cell above the body and refills oxygen; swimFails makes it time out unmoved.
    async swim () {
      const before = s.self.oxygen
      const head = () => blockName({ ...s.self.pos, y: s.self.pos.y + 1 })
      if (s.swimFails) return { status: 'timeout', oxygen: { before, after: before } }
      while (head() === 'water') s.self.pos = { ...s.self.pos, y: s.self.pos.y + 1 }
      s.self.oxygen = 20
      return { status: 'surfaced', oxygen: { before, after: 20 } }
    },

    async look () {
      return { status: 'ok' }
    }
  }
}

export function createFake (spec = {}) {
  const s = initialState(spec)
  const ageOf = (k) => (s.ages.has(k) ? { age: s.ages.get(k) } : {})
  const listeners = new Set()
  const calls = []
  const holds = new Map() // name -> array of pending hold records
  const pending = new Set() // { token, reject }
  const overrides = new Map()
  const acts = defaultActs(s)
  let owner = null

  const checkOwner = (token) => { if (token !== owner) throw new CutError() }

  // Wait on a hold if one is armed; resolves to a forced result or undefined.
  const waitHold = (name, token) => {
    const queue = holds.get(name)
    const armed = queue?.find(h => !h.taken)
    if (!armed) return Promise.resolve(undefined)
    armed.taken = true
    return new Promise((resolve, reject) => {
      const entry = { token, reject }
      pending.add(entry)
      armed.release = (result) => { pending.delete(entry); queue.splice(queue.indexOf(armed), 1); resolve(result) }
      if (armed.released) armed.release(armed.result)
    })
  }

  const wrap = (name) => async (token, args = {}) => {
    calls.push({ name, token, args: clone(args) })
    checkOwner(token)
    const forced = await waitHold(name, token)
    checkOwner(token)
    if (forced !== undefined) return forced
    const impl = acts[name]
    const override = overrides.get(name)
    return override ? override(token, args, impl) : impl(token, args)
  }

  const primitives = {
    setOwner (token) {
      owner = token
      for (const p of [...pending]) {
        if (p.token === token) continue
        pending.delete(p)
        p.reject(new CutError())
      }
    },
    isOwner: (token) => token === owner,

    self () {
      return {
        username: s.self.username,
        pos: { ...s.self.pos },
        health: s.self.health,
        food: s.self.food,
        foodSaturation: s.self.foodSaturation,
        oxygen: s.self.oxygen,
        onFire: s.self.onFire,
        inWater: s.self.inWater,
        inLava: s.self.inLava,
        isSleeping: s.self.isSleeping,
        experience: { ...s.self.experience },
        dimension: s.self.dimension,
        timeOfDay: s.time,
        isDay: isDayAt(s.time),
        held: s.self.held,
        inventory: withSlots(s.inventory)
      }
    },

    entities ({ radius = 16, kind, names, max = 32 } = {}) {
      return s.entities
        .map(e => ({ ...clone(e), distance: dist(s.self.pos, e.pos) }))
        .filter(e => e.distance <= radius && (!kind || e.kind === kind) && (!names || names.includes(e.name)))
        .sort((a, b) => a.distance - b.distance)
        .slice(0, max)
    },

    blocks ({ radius = 16, names, match, max = 64 } = {}) {
      const ok = names ? (n) => names.includes(n) : match ?? (() => true)
      return [...s.blocks.entries()]
        .map(([k, name]) => ({ name, pos: parseKey(k), ...ageOf(k) }))
        .map(b => ({ ...b, distance: dist(s.self.pos, b.pos) }))
        .filter(b => b.distance <= radius && ok(b.name))
        .sort((a, b) => a.distance - b.distance)
        .slice(0, max)
    },

    blockAt (pos) {
      if (s.unloaded.has(key(pos))) return null
      return { name: s.blocks.get(key(pos)) ?? 'air', pos: { ...pos }, ...ageOf(key(pos)) }
    },

    onBodyEvent (listener) {
      listeners.add(listener)
      return () => listeners.delete(listener)
    },

    close () {},

    world: {
      state: s,
      calls,
      // The next call to `name` waits until release(result?) is called or the owner changes.
      hold (name) {
        const queue = holds.get(name) ?? []
        holds.set(name, queue)
        const h = { taken: false, released: false, result: undefined }
        queue.push(h)
        return (result) => {
          if (h.release) return h.release(result)
          h.released = true
          h.result = result
        }
      },
      override (name, fn) { overrides.set(name, fn) },
      emit (event) { for (const l of listeners) l(event) },
      setTime (t) { s.time = t },
      // Dies where the body stands: emits died like the real body, then drops the
      // inventory there as item entities and resets the experience.
      die () {
        const { level, points } = s.self.experience
        primitives.world.emit({ kind: 'died', pos: { ...s.self.pos }, inventory: withSlots(s.inventory), experience: { level, points } })
        for (const i of s.inventory) s.entities.push({ id: s.nextEntityId++, name: 'item', kind: 'item', pos: { ...s.self.pos }, item: { name: i.name, count: i.count } })
        s.inventory = []
        s.self.experience = { level: 0, points: 0, progress: 0 }
      }
    }
  }

  // Leaves for a shortened wait, then comes back; a cut during the wait still comes back, then says so.
  acts.offline = async (token, { ms } = {}) => {
    const wanted = Math.floor(Math.min(ms ?? OFFLINE_DEFAULT_MS, OFFLINE_MAX_MS))
    s.offline = true
    primitives.world.emit({ kind: 'offline', ms: wanted })
    await new Promise(resolve => setTimeout(resolve, wanted * s.offlineScale))
    s.offline = false
    primitives.world.emit({ kind: 'online', pos: { ...s.self.pos } })
    return token === owner ? { status: 'ok', ms: wanted } : { status: 'cut' }
  }

  for (const name of Object.keys(acts)) primitives[name] = wrap(name)
  return primitives
}
