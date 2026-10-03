// A minimal stand-in for a mineflayer bot, enough for primitives.mjs tests. Never connects to anything.
// Every method that talks to the world is recorded in `calls`; methods listed in `hang` never settle and
// methods named in `reject` ({name: message}) reject with an Error carrying that message.
import vec3 from 'vec3'
import { EventEmitter } from 'node:events'

const { Vec3 } = vec3

const never = () => new Promise(() => {})
const key = (x, y, z) => `${x},${y},${z}`

export function stubBot ({ oxygen = 20, blocks = {}, items = [], entities = {}, hang = [], reject = {}, pos = [0, 64, 0], food = 10, timeOfDay = 15000, containers = {}, props = {}, effects = [], unloaded = false, sleeping = false, onActivate = () => {} } = {}) {
  const calls = []
  const bot = new EventEmitter()
  const hangs = new Set(hang)
  const asleepRejects = new Set(['goto', 'setGoal', 'setControlState', 'dig', 'placeBlock', '_placeBlockWithOptions', 'attack', 'activateItem', 'equip', 'toss', 'tossStack'])
  const act = (name, impl = () => undefined) => (...args) => {
    calls.push({ name, args })
    if (bot.isSleeping && asleepRejects.has(name)) return Promise.reject(new Error('asleep'))
    if (name in reject) return Promise.reject(new Error(reject[name]))
    return hangs.has(name) ? never() : Promise.resolve(impl(...args))
  }
  let notLoaded = unloaded // the chunks have not arrived: blockAt answers null everywhere until loadWorld()
  const blockAt = v => {
    if (notLoaded) return null
    const name = blocks[key(v.x, v.y, v.z)]
    if (name === undefined) return { name: 'air', position: new Vec3(v.x, v.y, v.z), boundingBox: 'empty', diggable: false, getProperties: () => ({}) }
    return { name, position: new Vec3(v.x, v.y, v.z), boundingBox: 'block', diggable: name !== 'bedrock', getProperties: () => props[key(v.x, v.y, v.z)] ?? {} }
  }
  const ownColumn = v => { const p = bot.entity.position; return v.x === p.x && v.y === p.y && v.z === p.z }
  const windowOf = pos => ({
    containerItems: () => containers[pos] ?? [],
    deposit: act('deposit'),
    withdraw: act('withdraw')
  })
  Object.assign(bot, {
    username: 'Stub',
    entity: { position: new Vec3(...pos), height: 1.62, onGround: true, velocity: new Vec3(0, 0, 0), effects: Object.fromEntries(effects.map(e => [e.id, { id: e.id, amplifier: e.amplifier, duration: e.duration }])) },
    entities,
    health: 20,
    food,
    time: { timeOfDay },
    isSleeping: sleeping,
    heldItem: null,
    inventory: { items: () => items, slots: Object.fromEntries(items.map(i => [i.slot, i])) },
    registry: { effects: Object.fromEntries(effects.map(e => [e.id, { id: e.id, name: e.name }])), itemsByName: { bread: { id: 1 }, cobblestone: { id: 2 } }, foodsByName: { bread: { foodPoints: 5 }, apple: { foodPoints: 4 } } },
    physics: { playerHalfWidth: 0.3 },
    calls,
    loadWorld: () => { notLoaded = false },
    unloadWorld: () => { notLoaded = true },
    // the probe at the body's own position (chunkLoaded, the physics watchdog) is not a world read worth recording
    blockAt: v => { if (!ownColumn(v)) calls.push({ name: 'blockAt', args: [v] }); return blockAt(v) },
    findBlocks: ({ matching, maxDistance, count }) => {
      const found = Object.keys(blocks).map(k => new Vec3(...k.split(',').map(Number)))
        .filter(p => matching(blockAt(p)) && p.distanceTo(bot.entity.position) <= maxDistance)
      return found.slice(0, count)
    },
    pathfinder: { goto: act('goto'), setGoal: act('setGoal'), setMovements: () => {} },
    oxygenLevel: oxygen,
    controlState: {},
    clearControlStates: act('clearControlStates', () => { bot.controlState = {} }),
    setControlState: act('setControlState', (control, state) => { bot.controlState[control] = state }),
    dig: act('dig'),
    stopDigging: act('stopDigging'),
    placeBlock: act('placeBlock'),
    _placeBlockWithOptions: act('_placeBlockWithOptions'),
    equip: act('equip'),
    toss: act('toss'),
    tossStack: act('tossStack'),
    consume: act('consume'),
    deactivateItem: act('deactivateItem'),
    activateItem: act('activateItem', () => onActivate(bot)),
    attack: act('attack'),
    sleep: act('sleep', () => { bot.isSleeping = true }),
    wake: act('wake'), // records only: the real bot.wake() sends a wrong id on this protocol
    // the raw client: leave_bed (by name) is what gets the body out of bed
    _client: { write: (name, data) => { calls.push({ name: 'write', args: [name, data] }); if (name === 'entity_action' && data.actionId === 'leave_bed') bot.isSleeping = false } },
    lookAt: act('lookAt', point => {
      const delta = point.minus(bot.entity.position.offset(0, bot.entity.height, 0))
      Object.assign(bot.entity, { yaw: Math.atan2(-delta.x, -delta.z), pitch: Math.atan2(delta.y, Math.hypot(delta.x, delta.z)) })
    }),
    look: act('look', (yaw, pitch) => { Object.assign(bot.entity, { yaw, pitch }) }),
    closeWindow: act('closeWindow'),
    openContainer: act('openContainer', block => windowOf(key(block.position.x, block.position.y, block.position.z))),
    quit: act('quit')
  })
  return bot
}
export const names = bot => bot.calls.map(c => c.name)
export { Vec3 }
