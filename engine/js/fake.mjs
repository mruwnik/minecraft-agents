// A scriptable fake world exposing the primitives interface from README.md.
// Engine and job tests drive it; it never talks to a server.

import { lineClear } from './sight.mjs'
import { isReplaceable } from './blocks.mjs'
import { fakeUseOn } from './fake-use-on.mjs'

const key = ({ x, y, z }) => `${x},${y},${z}`
const parseKey = (k) => { const [x, y, z] = k.split(',').map(Number); return { x, y, z } }
const dist = (a, b) => Math.hypot(a.x - b.x, a.y - b.y, a.z - b.z)
const clone = (v) => structuredClone(v)

const REACH = 4.5
const EYE = 1.62
const BODY_MIDDLE = 0.9
const SEE_THROUGH = new Set(['air', 'water', 'lava', 'fire', 'short_grass', 'tall_grass', 'snow', 'glass', 'glass_pane'])
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
  onGround: true,
  isSleeping: false,
  effects: [],
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
    self: { ...clone(defaultSelf), ...clone(spec.self ?? {}), held: spec.self?.held ?? null },
    time: spec.time ?? 1000,
    blocks: new Map(Object.entries(spec.blocks ?? {})),
    unloaded: new Set(spec.unloaded ?? []), // "x,y,z" cells in an unloaded chunk: blockAt returns null there
    ages: new Map(Object.entries(spec.ages ?? {})), // "x,y,z" -> crop age, for blocks that have one
    states: new Map(Object.entries(clone(spec.states ?? {}))), // "x,y,z" -> block state properties (composter level ...)
    entities: clone(spec.entities ?? []).map(withEntityDefaults),
    inventory: clone(spec.inventory ?? []),
    containers: new Map(Object.entries(clone(spec.containers ?? {}))),
    drops: { ...(spec.drops ?? {}) },
    unreachable: new Set(spec.unreachable ?? []),
    noPath: new Set(spec.noPath ?? []), // moveTo targets the pathfinder resolves on with no path
    swimFails: spec.swimFails ?? false,
    nextEntityId: 1000,
    offline: false,
    controls: {},
    yaw: 0,
    pitch: 0,
    settles: spec.settles ?? false, // coming back (offline, respawn) opens a settling window until world.settle(false)
    settling: false,
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

function defaultActs (s, emit) {
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
      s.states.delete(key(pos))
      const dropName = s.drops[name] === undefined ? name : s.drops[name]
      const drops = dropName ? [spawnItem(pos, dropName, 1)] : []
      return { status: 'dug', block: name, drops: drops.map(e => ({ id: e.id, name: e.item.name, count: e.item.count, pos: { ...e.pos } })) }
    },

    async place (token, { pos, item }) {
      if (!near(pos)) return { status: 'unreachable' }
      if (item === 'bucket') { // scoops the water cell it is aimed at
        if (blockName(pos) !== 'water') return { status: 'missing' }
        if (takeItem(s.inventory, 'bucket', 1) === 0) return { status: 'no-item' }
        addItem(s.inventory, 'water_bucket', 1)
        s.blocks.delete(key(pos))
        return { status: 'placed', block: 'bucket' }
      }
      if (blockName(pos) !== 'air' && !isReplaceable(blockName(pos))) return { status: 'occupied' }
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

    // Raises the body one block per placement: needs the item, something solid under the feet and the two cells
    // above the feet free (the cell the head moves into is the one two above the start).
    async jumpPlace (token, { item, count = 1 }) {
      const total = Math.min(count ?? 1, 8)
      let placed = 0
      const outcome = (reason) => ({ status: placed === total ? 'done' : placed > 0 ? 'partial' : 'failed', placed, ...(reason && { reason }) })
      while (placed < total) {
        const at = s.self.pos
        const under = blockName({ ...at, y: at.y - 1 })
        if (!s.inventory.some(i => i.name === item)) return outcome('no-item')
        if (under === 'air' || under === 'water' || under === 'lava') return outcome('no-support')
        if (blockName({ ...at, y: at.y + 2 }) !== 'air') return outcome('no-headroom')
        takeItem(s.inventory, item, 1)
        s.blocks.set(key(at), item)
        s.self.pos = { ...at, y: at.y + 1 }
        placed += 1
      }
      return outcome()
    },

    async collect (token, { id }) {
      const e = entity(id)
      if (!e || e.kind !== 'item') return { status: 'gone', gained: [] }
      if (s.unreachable.has(key(e.pos))) return { status: 'unreachable', gained: [] }
      s.self.pos = { ...e.pos }
      s.entities.splice(s.entities.indexOf(e), 1)
      addItem(s.inventory, e.item.name, e.item.count)
      emit({ kind: 'picked-up', item: e.item.name, count: e.item.count })
      return { status: 'collected', gained: [{ ...e.item }] }
    },

    // Throws the item three blocks along +x as one item entity (the real body throws where it looks).
    async toss (token, { item, count, slot }) {
      if (typeof slot === 'number') { // exactly that slot's whole stack
        const stack = s.inventory[slot]
        if (!stack || stack.name !== item) return { status: 'no-item', count: 0 }
        s.inventory.splice(slot, 1)
        spawnItem({ ...s.self.pos, x: s.self.pos.x + 3 }, item, stack.count)
        return { status: 'tossed', count: stack.count }
      }
      const total = s.inventory.filter(i => i.name === item).reduce((sum, i) => sum + i.count, 0)
      const n = Math.min(count ?? total, total)
      if (n <= 0) return { status: 'no-item', count: 0 }
      for (let left = n; left > 0;) left -= takeItem(s.inventory, item, left)
      spawnItem({ ...s.self.pos, x: s.self.pos.x + 3 }, item, n)
      return { status: 'tossed', count: n }
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
      if (e.invulnerable) return { status: 'hit', health: e.health, hurt: false }
      e.health -= 5
      if (e.health > 0) return { status: 'hit', health: e.health, hurt: true }
      s.entities.splice(s.entities.indexOf(e), 1)
      for (const d of e.drops ?? []) spawnItem(e.pos, d.name, d.count)
      return { status: 'killed', health: 0, hurt: true }
    },

    async sleep (token, { pos }) {
      if (isDayAt(s.time)) return { status: 'not-night' }
      if (!blockName(pos).endsWith('_bed')) return { status: 'missing' }
      if (!near(pos)) return { status: 'unreachable' }
      s.time = 0
      return { status: 'sleeping' }
    },

    // Rises to the top water cell above the body and refills oxygen; swimFails makes it time out unmoved.
    async swim (token, { toward } = {}) {
      const before = s.self.oxygen
      const head = () => blockName({ ...s.self.pos, y: s.self.pos.y + 1 })
      if (s.swimFails) return { status: 'timeout', oxygen: { before, after: before } }
      while (head() === 'water') s.self.pos = { ...s.self.pos, y: s.self.pos.y + 1 }
      s.self.oxygen = 20
      if (!toward) return { status: 'surfaced', oxygen: { before, after: 20 } }
      // toward: lands on the target when it is within 6 blocks and standable (free cell over a solid, dry block)
      const under = blockName({ ...toward, y: toward.y - 1 })
      const standable = blockName(toward) === 'air' && under !== 'air' && under !== 'water' && under !== 'lava'
      if (!standable || dist(s.self.pos, toward) > 6) return { status: 'timeout', oxygen: { before, after: 20 } }
      s.self.pos = { ...toward }
      settle()
      return { status: 'landed', oxygen: { before, after: 20 } }
    },

    async useOn (token, args) {
      return fakeUseOn(s, args, { spawnItem, near })
    },

    async look () {
      return { status: 'ok' }
    },

    async wait () {
      return { status: 'ok' }
    }
  }
}

export function createFake (spec = {}) {
  const s = initialState(spec)
  const ageOf = (k) => (s.ages.has(k) ? { age: s.ages.get(k) } : {})
  const propsOf = (k) => {
    const properties = { ...(s.states.get(k) ?? {}), ...ageOf(k) }
    return Object.keys(properties).length ? { properties } : {}
  }
  const listeners = new Set()
  const calls = []
  // a cell with a block that is not see-through stops the eye; unknown cells and unloaded ones do not
  const blocksSight = (cell) => !SEE_THROUGH.has(s.blocks.get(key(cell)) ?? 'air') && !s.unloaded.has(key(cell))
  const canSee = (e) => lineClear(
    { x: s.self.pos.x + 0.5, y: s.self.pos.y + EYE, z: s.self.pos.z + 0.5 },
    { x: e.pos.x + 0.5, y: e.pos.y + BODY_MIDDLE, z: e.pos.z + 0.5 },
    blocksSight)
  const holds = new Map() // name -> array of pending hold records
  const pending = new Set() // { token, reject }
  const overrides = new Map()
  const acts = defaultActs(s, event => primitives.world.emit(event))
  let owner = null
  let sleeper = null // the offline call in its wait: { token, wake }
  let away = null // promise of the body being back, while offline

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
    if (away) await away
    checkOwner(token)
    const forced = await waitHold(name, token)
    checkOwner(token)
    if (forced !== undefined) return forced
    if (name !== 'sleep') s.self.isSleeping = false // acting leaves the bed first, as the real primitives do
    const impl = acts[name]
    const override = overrides.get(name)
    return override ? override(token, args, impl) : impl(token, args)
  }

  const primitives = {
    setOwner (token) {
      if (token !== owner) s.controls = {}
      owner = token
      if (sleeper && sleeper.token !== token) sleeper.wake()
      for (const p of [...pending]) {
        if (p.token === token) continue
        pending.delete(p)
        p.reject(new CutError())
      }
    },
    isOwner: (token) => token === owner,

    // Manual takeover: records controls (booleans) and the look in Minecraft degrees on the state.
    drive (token, { controls = {}, look } = {}) {
      checkOwner(token)
      Object.assign(s.controls, controls)
      if (look) {
        const yaw = look.yaw ?? s.yaw + (look.dyaw ?? 0)
        const pitch = look.pitch ?? s.pitch + (look.dpitch ?? 0)
        s.yaw = ((yaw % 360) + 360) % 360
        s.pitch = Math.min(90, Math.max(-90, pitch))
      }
      return { pos: { ...s.self.pos }, yaw: s.yaw, pitch: s.pitch }
    },
    stopDriving () { s.controls = {} },

    isOffline: () => s.offline,
    isSettling: () => !s.offline && s.settling,

    self () {
      if (s.offline) return { status: 'offline' }
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
        onGround: s.self.onGround,
        settling: s.settling,
        isSleeping: s.self.isSleeping,
        effects: s.self.effects.map(e => ({ ...e })),
        experience: { ...s.self.experience },
        dimension: s.self.dimension,
        timeOfDay: s.time,
        isDay: isDayAt(s.time),
        held: s.self.held,
        inventory: withSlots(s.inventory)
      }
    },

    entities ({ radius = 16, kind, names, max = 32 } = {}) {
      if (s.offline) return []
      return s.entities
        .map(e => ({ ...clone(e), distance: dist(s.self.pos, e.pos), ...(e.kind === 'hostile' && { visible: e.visible ?? canSee(e) }) }))
        .filter(e => e.distance <= radius && (!kind || e.kind === kind) && (!names || names.includes(e.name)))
        .sort((a, b) => a.distance - b.distance)
        .slice(0, max)
    },

    blocks ({ radius = 16, names, match, max = 64 } = {}) {
      if (s.offline) return []
      const ok = names ? (n) => names.includes(n) : match ?? (() => true)
      return [...s.blocks.entries()]
        .map(([k, name]) => ({ name, pos: parseKey(k), ...ageOf(k), ...propsOf(k) }))
        .map(b => ({ ...b, distance: dist(s.self.pos, b.pos) }))
        .filter(b => b.distance <= radius && ok(b.name))
        .sort((a, b) => a.distance - b.distance)
        .slice(0, max)
    },

    blockAt (pos) {
      if (s.offline) return null
      if (s.unloaded.has(key(pos))) return null
      return { name: s.blocks.get(key(pos)) ?? 'air', pos: { ...pos }, ...ageOf(key(pos)), ...propsOf(key(pos)) }
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
      settle (on) { s.settling = on },
      // Respawns where the body stands: emits respawned like the real body and opens a settling window if the spec settles.
      respawn () {
        s.settling = s.settles
        primitives.world.emit({ kind: 'respawned', pos: { ...s.self.pos }, dimension: s.self.dimension })
      },
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

  // Leaves for a shortened wait, then comes back. A cut ends the wait early; the body is still back (online
  // event, isOffline false) before the call resolves 'cut'.
  acts.offline = async (token, { ms } = {}) => {
    const wanted = Math.floor(Math.min(ms ?? OFFLINE_DEFAULT_MS, OFFLINE_MAX_MS))
    let release
    away = new Promise(resolve => { release = resolve })
    s.offline = true
    primitives.world.emit({ kind: 'offline', ms: wanted })
    await new Promise(resolve => {
      const timer = setTimeout(resolve, wanted * s.offlineScale)
      sleeper = { token, wake: () => { clearTimeout(timer); resolve() } }
    })
    sleeper = null
    s.offline = false
    s.settling = s.settles
    away = null
    primitives.world.emit({ kind: 'online', pos: { ...s.self.pos } })
    release()
    return token === owner ? { status: 'ok', ms: wanted } : { status: 'cut' }
  }

  for (const name of Object.keys(acts)) primitives[name] = wrap(name)
  return primitives
}
